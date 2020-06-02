#-----------------------------------------
# This script aims to load in 1 minute
# bins data and produce time series
# charts by condition
#
# NOTE: This script requires setup.R to
# have been run first
#-----------------------------------------

#-----------------------------------------
# Author: Trent Henderson, 2 June 2020
#-----------------------------------------

# Load data

d <- read.csv("data/1minutebins.csv") %>%
  clean_names()

#---------------PRE PROCESSING-------------------

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

# T1

df_t1_base <- d %>%
  dplyr::select(c(1:15))
df_t1_base <- time_cleaner(df_t1_base) %>%
  mutate(timer = "T1") %>%
  mutate(condition = "Rest")

df_t1_med <- d %>%
  dplyr::select(c(16:30))
df_t1_med <- time_cleaner(df_t1_med) %>%
  mutate(timer = "T1") %>%
  mutate(condition = "Meditation")

# T2

df_t2_base <- d %>%
  dplyr::select(c(31:45))
df_t2_base <- time_cleaner(df_t2_base) %>%
  mutate(timer = "T2") %>%
  mutate(condition = "Rest")

df_t2_med <- d %>%
  dplyr::select(c(46:60))
df_t2_med <- time_cleaner(df_t2_med) %>%
  mutate(timer = "T2") %>%
  mutate(condition = "Meditation")

#---------------TIME SERIES PREP-----------------

# Define reusable function

summary_function <- function(data){
  data <- data %>%
    group_by(minute, timer, condition)%>%
    summarise(value = mean(value)) %>%
    ungroup()
  
  data <- data %>%
    mutate(id = rownames(data)) %>%
    dplyr::select(c(value, condition, id, timer)) %>%
    rename(minute = id) %>%
    mutate(minute = as.character(minute)) %>%
    mutate(minute = as.numeric(minute))
  
  return(data)
}

# Average and aggregation

t1_base_clean <- summary_function(df_t1_base)
t1_med_clean <- summary_function(df_t1_med)
t2_base_clean <- summary_function(df_t2_base)
t2_med_clean <- summary_function(df_t2_med)

# Merge

merged_data <- bind_rows(t1_base_clean, t1_med_clean, t2_base_clean, t2_med_clean)

#---------------PLOT------------------

the_palette <- c("#D53E4F", "#FDAE61") # Spectral palette Jeff used initially

# Compute average percentage difference between meditation and rest for T1 and T2

pct_calc <- function(v1, v2){
  ((abs(v1-v2))/((v1+v2)/2))*100
}

pct_data <- merged_data %>%
  group_by(timer, condition) %>%
  summarise(avg_val = mean(value)) %>%
  ungroup() %>%
  spread(key = condition, value = avg_val) %>%
  group_by(timer) %>%
  mutate(pct_diff = round(pct_calc(Meditation, Rest), digits = 2)) %>%
  ungroup()

# Plot

agg_plot <- merged_data %>%
  ggplot(aes(x = minute, y = value)) +
  geom_line(aes(colour = condition), stat = "identity", size = 1.25) +
  geom_label(data = pct_data, aes(x = 11.6, y = 9.5, label = paste0("Mean % diff\nbetween rest and\nmeditation = ",pct_diff,"%")), 
            colour = "#3288BD") +
  labs(x = "Minute",
       y = "HF-HRV",
       colour = NULL) +
  scale_x_continuous(breaks = seq(from = 1, to = 15, by = 1)) +
  scale_y_continuous(limits = c(1,10),
                     breaks = seq(from = 1, to = 10, by = 1)) +
  scale_colour_manual(values = the_palette) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  facet_wrap(~timer)
print(agg_plot)

#---------------OUTPUT-------------

CairoPNG("output/")
print(agg_plot)
dev.off()
