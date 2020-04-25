#-----------------------------------------
# This script aims to load in 1 minute
# bins data and produce time series 
# analysis of Rest vs Meditation
# conditions
#
# NOTE: This script requires setup.R to
# have been run first
#-----------------------------------------

#-----------------------------------------
# Author: Trent Henderson, 25 April 2020
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

# Make time series

t1_rest_ts <- ts(t1_base_clean$value, start = c(1), end = c(15), frequency = 1)
t1_med_ts <- ts(t1_med_clean$value, start = c(1), end = c(15), frequency = 1)
t2_rest_ts <- ts(t2_base_clean$value, start = c(1), end = c(15), frequency = 1)
t2_med_ts <- ts(t2_med_clean$value, start = c(1), end = c(15), frequency = 1)

#---------------TIME SERIES DIAGNOSTICS----------

#----------------
# GENERAL TREND
#----------------

# Plot

agg_plot <- merged_data %>%
  ggplot(aes(x = minute, y = value)) +
  geom_line(aes(colour = condition), stat = "identity", size = 1.25) +
  labs(title = "Mean value over time by condition",
       x = "Minute",
       y = "Value",
       colour = "Condition") +
  scale_x_continuous(breaks = seq(from = 1, to = 15, by = 1)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  facet_wrap(~timer)
print(agg_plot)

#----------------
# AUTOCORRELATION
#----------------

# Define reusable plotting function

autocor_plot <- function(data){
  p <- ggAcf(data) +
    labs(x = "Lag",
         y = "ACF") +
    theme_bw()
}

# Plots

t1_rest_auto <- autocor_plot(t1_rest_ts) +
  labs(title = "Autocorrelation for T1 Rest")

t1_med_auto <- autocor_plot(t1_med_ts) +
  labs(title = "Autocorrelation for T1 Meditation")

t2_rest_auto <- autocor_plot(t2_rest_ts) +
  labs(title = "Autocorrelation for T2 Rest")

t2_med_auto <- autocor_plot(t2_med_ts) +
  labs(title = "Autocorrelation for T2 Meditation")

#----------------
# OUTPUT
#----------------

# First plot

print(agg_plot)

# Put all autocorrelation plots on 1 pages

ggarrange(t1_rest_auto, t1_med_auto, t2_rest_auto, t2_med_auto,
          ncol = 2, nrow = 2)
