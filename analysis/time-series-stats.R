#-----------------------------------------
# This script aims to load in 1 minute
# bins data and produce time series
# statistics
#
# NOTE: This script requires setup.R to
# have been run first
#-----------------------------------------

#-----------------------------------------
# Author: Trent Henderson, 24 April 2020
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
df_t1_base <- time_cleaner(df_t1_base)

df_t1_med <- d %>%
  dplyr::select(c(16:30))
df_t1_med <- time_cleaner(df_t1_med)

# T2

df_t2_base <- d %>%
  dplyr::select(c(31:45))
df_t2_base <- time_cleaner(df_t2_base)

df_t2_med <- d %>%
  dplyr::select(c(46:60))
df_t2_med <- time_cleaner(df_t2_med)

#---------------TIME SERIES PREP-----------------

# Define reusable function

summary_function <- function(data){
  data <- data %>%
    group_by(minute)%>%
    summarise(value = mean(value)) %>%
    ungroup()
}

# Clean just meditation data initially for a first plot

t1_mean <- summary_function(df_t1_med)
t2_mean <- summary_function(df_t2_med)

# Put into time series format

t1_ts <- ts(t1_mean$value, start = c(1), end = c(15), frequency = 1)
t2_ts <- ts(t2_mean$value, start = c(1), end = c(15), frequency = 1)

#---------------TIME SERIES DIAGNOSTICS----------

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

# Plot

t1_auto_plot <- autocor_plot(t1_ts) +
  labs(title = "Autocorrelation plot for T1")

t2_auto_plot <- autocor_plot(t2_ts) +
  labs(title = "Autocorrelation plot for T2")

# Arrange onto 1 page for visual inspection

ggarrange(t1_auto_plot, t2_auto_plot,
          ncol = 2, nrow = 1)
