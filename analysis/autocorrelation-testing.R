#------------------------------------------
# This script aims to build autocorrelation
# diagnostics
#
# NOTE: This script requires setup.R to
# have been run first
#------------------------------------------

#------------------------------------------
# Author: Trent Henderson, 28 April 2020
#------------------------------------------

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
  mutate(minute = as.numeric(minute)) %>%
  mutate(state = as.factor(state)) %>%
  mutate(condition = as.factor(condition)) %>%
  rename(time = minute) %>%
  select()

#--------------
# CALCULATE ACF
#--------------

# Need to do this manually as forecast::ggAcf doesn't allow faceting

# Function for ACF calculation

acf_calculator <- function(data, the_condition, the_state){

df_acf <- data %>%
  filter(condition == the_condition) %>%
  filter(state == the_state) %>%
  group_by(id) %>% 
  summarise(list_acf = list(acf(value, plot = FALSE))) %>%
  mutate(acf_vals = purrr::map(list_acf, ~as.numeric(.x$acf))) %>% 
  select(-list_acf) %>% 
  unnest() %>% 
  group_by(id) %>% 
  mutate(lag = row_number() - 1)
}

# Function for ACF confidence intervals

acf_ci_calculator <- function(data){

df_ci <- data %>% 
  group_by(id) %>% 
  summarise(ci = qnorm((1 + 0.95)/2)/sqrt(n()))
}

# Do calculations for each subset of the experimental conditions

t1_rest_acf <- acf_calculator(df_prep, "T1", "Rest")
t1_rest_ci <- acf_ci_calculator(t1_rest_acf)
t1_med_acf <- acf_calculator(df_prep, "T1", "Meditation")
t1_med_ci <- acf_ci_calculator(t1_med_acf)
t2_rest_acf <- acf_calculator(df_prep, "T2", "Rest")
t2_rest_ci <- acf_ci_calculator(t2_rest_acf)
t2_med_acf <- acf_calculator(df_prep, "T2", "Meditation")
t2_med_ci <- acf_ci_calculator(t2_med_acf)

#---------------AUTOCORRELATION MODELLING----------

# Define reusable plotting function

acf_plotter <- function(data, ci_data){
  
  ggplot(data = data, aes(x = lag, y = acf_vals)) +
    geom_bar(stat = "identity", width = .05) +
    geom_hline(yintercept = 0) +
    geom_hline(data = ci_data, aes(yintercept = -ci), color = "blue", linetype = "dotted") +
    geom_hline(data = ci_data, aes(yintercept = ci), color = "blue", linetype = "dotted") +
    labs(x = "Lag", 
         y = "ACF") +
    facet_wrap(~id)
}

# Produce plots

t1_rest_plot <- acf_plotter(t1_rest_acf, t1_rest_ci) +
  labs(title = "T1 Rest")

t1_med_plot <- acf_plotter(t1_med_acf, t1_med_ci) +
  labs(title = "T1 Meditation")

t2_rest_plot <- acf_plotter(t2_rest_acf, t2_rest_ci) +
  labs(title = "T2 Rest")

t2_med_plot <- acf_plotter(t2_med_acf, t2_med_ci) +
  labs(title = "T2 Meditation")

# Print plots

print(t1_rest_plot)
print(t1_med_plot)
print(t2_rest_plot)
print(t2_med_plot)
