#-----------------------------------------
# This script aims to build stationarity 
# testing using the Dickey-Fuller test
#
# NOTE: This script requires setup.R to
# have been run first
#-----------------------------------------

#-----------------------------------------
# Author: Trent Henderson, 27 April 2020
#-----------------------------------------

# Load and prep data

d <- read.csv("data/1minutebins.csv") %>%
  clean_names()

#----------
# WRANGLING
#----------

# Define general regex cleaning function

time_cleaner <- function(data){
  data <- data %>%
    gather(key = minute, value = value, 1:15) %>% # Pull into 2 columns
    mutate(minute = gsub("_.*", "", minute)) %>% # Remove nuisance symbols
    mutate(minute = gsub("[A-z]", "\\1", minute)) %>% # Extract just minute number
    mutate(minute = as.numeric(minute))
}

# T1 Rest

df_t1_base <- d %>%
  dplyr::select(c(1:15)) %>%
  mutate(id = row_number())

df_t1_base <- time_cleaner(df_t1_base) %>%
  mutate(condition = "T1") %>%
  mutate(state = "Rest")

# T1 Meditation

df_t1_med <- d %>%
  dplyr::select(c(16:30)) %>%
  mutate(id = row_number())

df_t1_med <- time_cleaner(df_t1_med) %>%
  mutate(condition = "T1") %>%
  mutate(state = "Meditation") %>%
  mutate(minute = case_when(
    minute == 16 ~ 1,
    minute == 17 ~ 2,
    minute == 18 ~ 3,
    minute == 19 ~ 4,
    minute == 20 ~ 5,
    minute == 21 ~ 6,
    minute == 22 ~ 7,
    minute == 23 ~ 8,
    minute == 24 ~ 9,
    minute == 25 ~ 10,
    minute == 26 ~ 11,
    minute == 27 ~ 12,
    minute == 28 ~ 13,
    minute == 29 ~ 14,
    minute == 30 ~ 15)) # Recode into 1:15 to make equivalent - probably a better way though

# T2 Rest

df_t2_base <- d %>%
  dplyr::select(c(31:45)) %>%
  mutate(id = row_number())

df_t2_base <- time_cleaner(df_t2_base) %>%
  mutate(condition = "T2") %>%
  mutate(state = "Rest") %>%
  mutate(minute = case_when(
    minute == 31 ~ 1,
    minute == 32 ~ 2,
    minute == 33 ~ 3,
    minute == 34 ~ 4,
    minute == 35 ~ 5,
    minute == 36 ~ 6,
    minute == 37 ~ 7,
    minute == 38 ~ 8,
    minute == 39 ~ 9,
    minute == 40 ~ 10,
    minute == 41 ~ 11,
    minute == 42 ~ 12,
    minute == 43 ~ 13,
    minute == 44 ~ 14,
    minute == 45 ~ 15))

# T2 Meditation

df_t2_med <- d %>%
  dplyr::select(c(46:60)) %>%
  mutate(id = row_number())

df_t2_med <- time_cleaner(df_t2_med) %>%
  mutate(condition = "T2") %>%
  mutate(state = "Meditation") %>%
  mutate(minute = case_when(
    minute == 46 ~ 1,
    minute == 47 ~ 2,
    minute == 48 ~ 3,
    minute == 49 ~ 4,
    minute == 50 ~ 5,
    minute == 51 ~ 6,
    minute == 52 ~ 7,
    minute == 53 ~ 8,
    minute == 54 ~ 9,
    minute == 55 ~ 10,
    minute == 56 ~ 11,
    minute == 57 ~ 12,
    minute == 58 ~ 13,
    minute == 59 ~ 14,
    minute == 60 ~ 15))

#----------
# MERGING
#----------

# Bind files together and convert to factors

df_prep <- bind_rows(df_t1_base, df_t1_med, df_t2_base, df_t2_med) %>%
  mutate(minute = as.factor(minute)) %>%
  mutate(state = as.factor(state)) %>%
  mutate(condition = as.factor(condition))

#----------------DICKEY-FULLER-------------------

id_list <- unique(df_prep$id)

# Extract DF test statistic and p-value for each participant, state and condition combo
# DF tests for the presence of a unit root

a_list <- list()
for(i in id_list){
  
  raw_data <- df_prep %>%
    filter(id == i)
  
  t1_rest <- raw_data %>%
    filter(condition == "T1") %>%
    filter(state == "Rest")
  
  t1_med <- raw_data %>%
    filter(condition == "T1") %>%
    filter(state == "Meditation")
  
  t2_rest <- raw_data %>%
    filter(condition == "T2") %>%
    filter(state == "Rest")
  
  t2_med <- raw_data %>%
    filter(condition == "T2") %>%
    filter(state == "Meditation")
  
  # Defaults to Dickey Fuller from ADF at 0 lags
  # This is more appropriate given lack of data complexity here
  
  t1_rest_results <- adf.test(t1_rest$value, k = 0,
                              alternative = c("stationary"))
  t1_med_results <- adf.test(t1_med$value, k = 0,
                             alternative = c("stationary"))
  t2_rest_results <- adf.test(t2_rest$value, k = 0,
                              alternative = c("stationary"))
  t2_med_results <- adf.test(t2_med$value, k = 0,
                             alternative = c("stationary"))
  
  station_data <- data.frame(id = c(i),
                             state = c("Rest", "Meditation", "Rest", "Meditation"),
                             condition = c("T1", "T1", "T2", "T2"),
                             statistic = c(t1_rest_results$statistic, t1_med_results$statistic,
                                           t2_rest_results$statistic, t2_med_results$statistic),
                             p_val = c(t1_rest_results$p.value, t1_med_results$p.value,
                                       t2_rest_results$p.value, t2_med_results$p.value))
  
  a_list[[i]] <- station_data
  
}

stationary_data_adf <- rbindlist(a_list, use.names = TRUE)

# See if any p-values are over p > .05 as H0 is "data is not stationary"

p_check_adf <- stationary_data_adf %>%
  filter(p_val > .05)

# Make into frequency dataframe for bar chart

total_n <- length(id_list)

p_bar_data_adf <- p_check_adf %>%
  group_by(state, condition) %>%
  summarise(counter = n()) %>%
  ungroup() %>%
  mutate(prop_counter = round((counter / total_n)*100, digits = 2))

# Plot frequency of non-stationarity violations

sig_chart_adf <- p_bar_data_adf %>%
  ggplot(aes(x = condition, y = prop_counter)) +
  geom_bar(aes(fill = state), stat = "identity") +
  labs(title = "Dickey-Fuller stationarity violations",
       x = "Condition",
       y = "Proportion of participants violated") +
  theme_bw() +
  scale_y_continuous(limits = c(0,80),
                     breaks = c(0,20,40,60,80),
                     labels = function(x) paste0(x,"%")) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  facet_wrap(~state)
print(sig_chart_adf)

#----------------STANDARD DEVIATION-------------------

dev_data <- df_prep %>%
  group_by(id, state, condition) %>%
  summarise(avg = mean(value),
            std_dev = sd(value)) %>%
  ungroup()

#---------------------------
# Produce Cleveland Dot Plot
#---------------------------

dev_data %>%
  mutate(id = as.factor(id),
         state = factor(state, levels = c("Rest", "Meditation"))) %>%
  ggplot(aes(x = id, y = std_dev)) +
  geom_line(aes(group = id)) +
  geom_point(aes(colour = state)) +
  labs(title = "Time series standard deviation in HR-HRV",
       x = "Participant ID",
       y = "HR-HRV standard deviation",
       colour = "State") +
  scale_x_discrete(breaks = seq(from = 1, to = 40, by = 1)) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom") +
  facet_wrap(~condition)
