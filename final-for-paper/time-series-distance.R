#-------------------------------------------
# This script aims to build GAM models to
# see what the differences are between
# states in T1 and T2
#
# NOTE: This script requires setup.R to
# have been run first
#-------------------------------------------

#-------------------------------------------
# Author: Trent Henderson, 28 April 2020
#-------------------------------------------

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
  mutate(minute = minute-15)

# T2 Rest

df_t2_base <- d %>%
  dplyr::select(c(31:45)) %>%
  mutate(id = row_number())

df_t2_base <- time_cleaner(df_t2_base) %>%
  mutate(condition = "T2") %>%
  mutate(state = "Rest") %>%
  mutate(minute = minute-(2*15))

# T2 Meditation

df_t2_med <- d %>%
  dplyr::select(c(46:60)) %>%
  mutate(id = row_number())

df_t2_med <- time_cleaner(df_t2_med) %>%
  mutate(condition = "T2") %>%
  mutate(state = "Meditation") %>%
  mutate(minute = minute-(3*15))

#----------
# MERGING
#----------

df_ind <- bind_rows(df_t1_base, df_t1_med, df_t2_base, df_t2_med)

#-------------------TIME SERIES SIMILARITY----------------

# Compute correlation distances based on Pearson's correlation

pearson_distances <- df_ind %>%
  spread(key = state, value = value) %>%
  group_by(id, condition) %>%
  summarise(the_dist = CorDistance(Meditation, Rest)) %>%
  group_by(condition) %>%
  summarise(avg_dist = round(mean(the_dist), digits = 2)) %>%
  ungroup() %>%
  mutate("Distance Measure" = "Pearson's Correlation")

# Compute correlation distances based on Euclidean distance

euclid_distances <- df_ind %>%
  spread(key = state, value = value) %>%
  group_by(id, condition) %>%
  summarise(the_dist = EuclideanDistance(Meditation, Rest)) %>%
  group_by(condition) %>%
  summarise(avg_dist = round(mean(the_dist), digits = 2)) %>%
  ungroup() %>%
  mutate("Distance Measure" = "Euclidean Distance")

# Merge and present as a table

distances <- bind_rows(pearson_distances, euclid_distances) %>%
  rename(Condition = condition) %>%
  spread(key = Condition, value = avg_dist)

dist_table <- autofit(flextable(distances))
dist_table
