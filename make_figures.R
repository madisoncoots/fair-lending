# Code to generate plots.
library(tidyverse)
library(lubridate)
library(scales)
library(fs)

################################################################################
# Paths

src_path <- "/Users/madisoncoots/Documents/harvard/research/fair-lending/src_data"
out_path <- "/Users/madisoncoots/Documents/harvard/research/fair-lending/figures"

################################################################################
# Colors

# color_palette <- c(
#   "#0077b9", # blue
#   "#984ea3", # purple
#   "#ff7700", # orange
#   "#00a400" # green
# )

color_palette <- c(
  "#26547c", # dark blue
  "#e02b35", # red
  "#fcbf49", # gold
  "#59a89c" # teal
)

# color_palette <- c(
#   "#8cc5e3", # light blue
#   "#4a2377", # red
#   "#f55f74", # purple
#   "#0d7d87" # teal
# )

group_color_map <- c("Asian" = color_palette[4],
                     "Black" = color_palette[2],
                     "Hispanic" = color_palette[3],
                     "White" = color_palette[1])

group_names <- c("Asian", "Black", "Hispanic", "White")

gender_color_palette <- c(
  "#c701ff", # purple
  "#4ecb8d" # green
)
gender_group_color_map <- c("Male" = gender_color_palette[2],
                            "Female" = gender_color_palette[1])

gender_group_names <- c("Male", "Female")

################################################################################
# Risk vs. target IRR, all groups

target_returns_all_plot_data <- read_csv(path(src_path, "target_v_risk.csv"))

target_returns_all_plot_data %>%
  ggplot(aes(x = bin_avg_risk, y = bin_avg_target_return)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  theme(legend.title = element_blank()) +
  xlab("Risk score") +
  ylab("Target return") +
  coord_cartesian(ylim = c(0.055, 0.077)) +
  scale_x_continuous(labels = scales::percent) + 
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 0.12, 0.005))

ggsave(path(out_path, "target_return_v_risk.pdf"),
       width = 3.7,
       height = 3.5)

################################################################################
# IRR vs. APR by race, separated by year

irr_by_apr_plot_data <- read_csv(path(src_path, "irr_v_apr.csv"))

irr_by_apr_plot_data %>%
  filter(year == 2019) %>%
  ggplot(aes(x = bin_avg_apr, y = annualized_irr, color = race)) +
  geom_hline(yintercept = 0, color = "gray") +
  # geom_smooth(se = FALSE, linewidth = 0.65, method = "loess", span = 1) +
  geom_smooth(se = FALSE, linewidth = 0.65, method = "lm", span = 1) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  xlab("APR") +
  ylab("Annualized IRR") +
  scale_x_continuous(labels = scales::percent, breaks = seq(0, 1, 0.05)) +
  scale_y_continuous(labels = scales::percent, breaks = seq(-0.15, 0.15, 0.03)) +
  scale_color_manual(values=group_color_map,
                     breaks = group_names,
                     guide = guide_legend(title = NULL)) 

ggsave(path(out_path, "irr_v_apr.pdf"),
       width = 4.7,
       height = 3.5)

################################################################################
# Race blind calibration plot

race_blind_calibration_data <- read_csv(path(src_path, "race_blind_calibration.csv"))

race_blind_calibration_data %>%
  ggplot(aes(x = mean_risk, y = mean_default, color = race)) +
  geom_smooth(se=FALSE, linewidth=0.65) +
  geom_abline(slope = 1, intercept = 0, color = "darkgray", linetype = "dashed") +
  theme_bw() +
  xlab("Race-blind risk score") +
  ylab("Empricial default rate") + 
  scale_x_continuous(labels = percent, breaks = seq(0, 0.6, 0.05)) +
  scale_y_continuous(labels = percent, breaks = seq(0, 0.6, 0.05)) +
  scale_color_manual(values=group_color_map,
                     breaks = group_names,
                     guide = guide_legend(title = NULL)) 

ggsave(path(out_path, "race_blind_calibration.pdf"),
       width = 4.6,
       height = 3.5)

################################################################################
# IRR by interest rate plot using Upstart's cash_flows by race

irr_interest_rate_race_plot_data <- read_csv(path(src_path, "irr_v_interest_race.csv"))

irr_interest_rate_race_plot_data %>%
  ggplot(aes(x = interest_rate, y = annualized_irr, color = race)) +
  # geom_point() +
  # geom_line() +
  geom_smooth(aes(weight = loan_amount),se = F, method='lm', linewidth = 0.65) +
  # geom_smooth(aes(weight = loan_amount), se = F, method='loess', linewidth = 0.65) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  scale_color_manual(values = group_color_map,
                     breaks = group_names) +
  xlab("Interest rate") +
  ylab("Annualized IRR") +
  coord_cartesian(ylim=c(0,0.15)) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 0.16, 0.02))

ggsave(path(out_path, "irr_v_interest_race.pdf"),
       width = 4.6,
       height = 3.5)


################################################################################
# IRR by APR plot using Upstart's cash_flows by gender

irr_interest_rate_gender_plot_data <- read_csv(path(src_path, "irr_v_interest_gender.csv"))

irr_interest_rate_gender_plot_data %>%
  ggplot(aes(x = interest_rate, y = annualized_irr, color = gender)) +
  # geom_point() +
  # geom_line() +
  geom_smooth(aes(weight = loan_amount),se = F, method='lm', linewidth = 0.65) +
  # geom_smooth(aes(weight = loan_amount), se = F, method='loess', linewidth = 0.65) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  scale_color_manual(values = gender_group_color_map,
                     breaks = gender_group_names) +
  xlab("Interest rate") +
  ylab("Annualized IRR") +
  coord_cartesian(ylim=c(0,0.15)) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 0.16, 0.02))

ggsave(path(out_path, "irr_v_interest_gender.pdf"),
       width = 4.6,
       height = 3.5)

################################################################################
# Gender blind calibration plot

gender_blind_calibration_data <- read_csv(path(src_path, "gender_blind_calibration.csv"))

gender_blind_calibration_data %>%
  ggplot(aes(x = mean_risk, y = mean_default, color = gender)) +
  # geom_line() +
  # geom_point() +
  geom_smooth(se=FALSE, linewidth=0.5) +
  geom_abline(slope = 1, intercept = 0, color = "darkgray", linetype = "dashed") +
  theme_bw() +
  xlab("Gender-blind risk score") +
  ylab("Empricial default rate") + 
  scale_x_continuous(labels = percent, breaks = seq(0, 0.6, 0.05)) +
  scale_y_continuous(labels = percent, breaks = seq(0, 0.6, 0.05)) +
  scale_color_manual(values=gender_group_color_map,
                     breaks = gender_group_names,
                     guide = guide_legend(title = NULL))

ggsave(path(out_path, "gender_blind_calibration.pdf"),
       width = 4.6,
       height = 3.5)

################################################################################
# Group IRR dot plot

group_irr_dot_plot_data <- read_csv(path(src_path, "group_irr_dot_plot.csv"))




