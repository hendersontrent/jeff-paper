#-----------------------------------------
# This script aims to build a panel
# regression model to understand time
# series and experimental effects
#
# NOTE: This script requires setup.R to
# have been run first
#-----------------------------------------

#-----------------------------------------
# Author: Trent Henderson, 25 April 2020
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

# Bind files together and convert to factors for the panel model

df_prep <- bind_rows(df_t1_base, df_t1_med, df_t2_base, df_t2_med) %>%
  mutate(minute = as.factor(minute)) %>%
  mutate(state = as.factor(state)) %>%
  mutate(condition = as.factor(condition))

# Reorder to have condition and minute columns as the first two columns for plm index

df_prep <- df_prep[,c(4,2,5,3,1)]

#----------------BUILD PANEL REGRESSION MODEL------------

model <- plm(value ~ state, 
                      data = df_prep,
                      index = c("condition", "minute"), 
                      model = "within", 
                      effect = "twoways")

summary(model)

coeftest(model, vcov = vcovHC, type = "HC1")
