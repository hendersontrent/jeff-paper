#-----------------------------------------
# This script aims to build a ribbon plot
# to show variability over time.
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

# Bind files together and convert to factors

df_prep <- bind_rows(df_t1_base, df_t1_med, df_t2_base, df_t2_med) %>%
  mutate(minute = as.factor(minute)) %>%
  mutate(state = as.factor(state)) %>%
  mutate(condition = as.factor(condition))

ribbon_prep <- df_prep %>%
  group_by(minute, state, condition) %>%
  summarise(avg = mean(value),
            the_lower = min(value),
            the_upper = max(value)) %>%
  ungroup() %>%
  mutate(minute = as.character(minute)) %>%
  mutate(minute = as.numeric(minute))

#----------------LINE VISUALISATION----------------------

the_palette <- c("#D53E4F", "#FDAE61") # Spectral palette Jeff used initially

CairoPNG("output/line-chart-final.png", 600,400)
p1 <- df_prep %>%
  mutate(minute = as.numeric(minute)) %>%
  ggplot(aes(x = minute, y = value, group = id)) +
  geom_line(colour = "#edf0f3", stat = "identity") +
  geom_line(data = ribbon_prep, aes(group = state, x = minute, y = avg, colour = state), 
            stat = "identity", size = 1.25, alpha = 0.8) +
  labs(x = "Minute",
       y = "n.u. HF-HRV",
       colour = NULL,
       caption = "Coloured line indicates mean.") +
  scale_x_continuous(breaks = seq(from = 1, to = 15, by = 1)) +
  scale_y_continuous(limits = c(1,10),
                     breaks = seq(from = 1, to = 10, by = 1)) +
  scale_colour_manual(values = the_palette) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  guides(fill = FALSE) +
  facet_grid(rows = vars(state), cols = vars(condition))
print(p1)
dev.off()
