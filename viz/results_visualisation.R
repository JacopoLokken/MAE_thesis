library(ggplot2)

# setwd("C:\\Users\\jacop\\OneDrive\\Desktop\\Thesis\\Code_Thesis") ## IMPORTANT! Un-comment before running
aggregated_data <- readRDS("results_analysis/aggregated_results.rds")
aggregate_results <- aggregated_data$aggregate_results
aggregate_sd <- aggregated_data$aggregate_sd
reference_results <- readRDS("data/reference_results.rds")
aggregate_rmse <- aggregated_data$aggregate_rmse
aggregate_bias <- aggregated_data$aggregate_bias
aggregate_rmse_sd <- aggregated_data$aggregate_rmse_sd
results <-readRDS("running_experiment/simulation_results1.rds")
####
#### Overall OOB ####
####

# Prepare data 
plot_data <- merge(
  aggregate_results,
  aggregate_sd,
  by = c("misP", "hand"),
  suffixes = c("_mean", "_sd")
)
plot_data <- plot_data[, c("misP", "hand", "oob_all_mean", "oob_all_sd")]
upper_bound <- reference_results$Overall_OOB[1]  # Baseline model (0.194)
lower_bound <- reference_results$Overall_OOB[2]  # Lower bound model (0.289)

# Plot 
ggplot(plot_data, aes(x = factor(misP), group = hand)) +
  geom_hline(yintercept = upper_bound, linetype = "dashed", 
             color = "darkgray", linewidth = 0.8) +
  annotate("text", x = Inf, y = upper_bound + 0.003, 
           label = "Baseline model", color = "#666666", size = 3.5,
           hjust = 1.1, vjust = -0.5) +
  
  geom_hline(yintercept = lower_bound, linetype = "dashed",
             color = "darkgray", linewidth = 0.8) +
  annotate("text", x = Inf, y = lower_bound - 0.003, 
           label = "Lower bound model", color = "#666666", size = 3.5,
           hjust = 1.1, vjust = 1.5) +
  
  geom_line(aes(y = oob_all_mean, color = hand, group = hand), 
            linewidth = 1) +
  geom_point(aes(y = oob_all_mean, color = hand), size = 3) +
  geom_errorbar(aes(ymin = oob_all_mean - oob_all_sd, 
                    ymax = oob_all_mean + oob_all_sd, 
                    color = hand), 
                width = 0.1, linewidth = 0.8) +
  
  scale_x_discrete(labels = c("20%", "50%", "80%")) +
  scale_color_manual(
    name = "Handling Method",
    values = c("omit" = "#E69F00", "roughfix" = "#56B4E9", "rfimpute" = "#009E73"),
    labels = c("Listwise Deletion", "Single Imputation", "Proximity Imputation")
  ) +
  labs(
    x = "Proportion of Missing Values",
    y = "Overall OOB Error Rate"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(colour = "black"),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

####
#### Missing VS Complete Cases ###
####

# Prepare data
plot_data1 <- merge(
  aggregated_data$aggregate_results,
  aggregated_data$aggregate_sd,
  by = c("misP", "hand"),
  suffixes = c("_mean", "_sd")
)

long_data1 <- reshape(
  plot_data1,
  direction = "long",
  varying = list(
    c("oob_missing_mean", "oob_complete_mean"),
    c("oob_missing_sd", "oob_complete_sd")
  ),
  timevar = "case_type",
  times = c("Missing", "Complete"),
  v.names = c("oob_mean", "oob_sd"),
  idvar = c("misP", "hand")
)


# Plot
ggplot(long_data1, aes(x = factor(misP), y = oob_mean, group = hand)) +
  geom_hline(yintercept = upper_bound, linetype = "dashed", color = "darkgray", linewidth = 0.6) +
  geom_hline(yintercept = lower_bound, linetype = "dashed", color = "darkgray", linewidth = 0.6) +
  geom_line(aes(color = hand), linewidth = 1) +
  geom_point(aes(color = hand), size = 2.5) +
  geom_errorbar(aes(ymin = oob_mean - oob_sd, ymax = oob_mean + oob_sd, color = hand), width = 0.1, linewidth = 0.6) +
  facet_wrap(~case_type) + # Facet by case type instead of hand
  scale_x_discrete(labels = c("20%", "50%", "80%")) +
  scale_color_manual(
    name = "Handling Method",
    values = c("omit" = "#E69F00", "roughfix" = "#56B4E9", "rfimpute" = "#009E73"),
    labels = c("Listwise Deletion", "Single Imputation", "Proximity Imputation")
  ) +
  labs(x = "Proportion of Missing Values", y = "OOB Error Rate") +
  theme_minimal() +
  theme(
    axis.line = element_line(colour = "black"),
    legend.position = "bottom",
    strip.background = element_rect(fill = "gray90", color = NA),
    strip.text = element_text(face = "bold")
  )

####
#### Females VS Males ####
####
# Prepare data
plot_data_gender <- merge(
  aggregated_data$aggregate_results,
  aggregated_data$aggregate_sd,
  by = c("misP", "hand"),
  suffixes = c("_mean", "_sd")
)
long_data_gender <- reshape(
  plot_data_gender,
  direction = "long",
  varying = list(
    c("oob_male_mean", "oob_female_mean"),
    c("oob_male_sd", "oob_female_sd")
  ),
  timevar = "gender",
  times = c("Male", "Female"),
  v.names = c("oob_mean", "oob_sd"),
  idvar = c("misP", "hand")
)

# Plot
ggplot(long_data_gender, aes(x = factor(misP), y = oob_mean, group = hand)) +
  geom_hline(yintercept = upper_bound, linetype = "dashed", color = "darkgray", linewidth = 0.6) +
  geom_hline(yintercept = lower_bound, linetype = "dashed", color = "darkgray", linewidth = 0.6) +
  geom_line(aes(color = hand), linewidth = 1) +
  geom_point(aes(color = hand), size = 2.5) +
  geom_errorbar(aes(ymin = oob_mean - oob_sd, ymax = oob_mean + oob_sd, color = hand), width = 0.1, linewidth = 0.6) +
  facet_wrap(~gender) + # Facet by gender instead of hand
  scale_x_discrete(labels = c("20%", "50%", "80%")) +
  scale_color_manual(
    name = "Handling Method",
    values = c("omit" = "#E69F00", "roughfix" = "#56B4E9", "rfimpute" = "#009E73"),
    labels = c("Listwise Deletion", "Single Imputation", "Proximity Imputation")
  ) +
  labs(x = "Proportion of Missing Values", y = "OOB Error Rate") +
  theme_minimal() +
  theme(
    axis.line = element_line(colour = "black"),
    legend.position = "bottom",
    strip.background = element_rect(fill = "gray90", color = NA),
    strip.text = element_text(face = "bold")
  )

####
#### Plot RMSE and Bias ####
####

# Reshape RMSE data into long format
rmse_long <- reshape(
  aggregate_rmse,
  direction = "long",
  varying = list(
    c("oob_all", "oob_missing", "oob_complete", "oob_male", "oob_female")
  ),
  timevar = "metric",
  times = c("Overall", "Missing", "Complete", "Male", "Female"),
  v.names = "RMSE",
  idvar = c("misP", "hand")
)

# Reshape Bias data into long format
bias_long <- reshape(
  aggregate_bias,
  direction = "long",
  varying = list(
    c("oob_all", "oob_missing", "oob_complete", "oob_male", "oob_female")
  ),
  timevar = "metric",
  times = c("Overall", "Missing", "Complete", "Male", "Female"),
  v.names = "Bias",
  idvar = c("misP", "hand")
)

# Reshape SD data into long format (for bias - but SD is the same as the aggregated SDs of OOB error rates)
sd_long <- reshape(
  aggregate_sd,
  direction = "long",
  varying = list(
    c("oob_all", "oob_missing", "oob_complete", "oob_male", "oob_female")
  ),
  timevar = "metric",
  times = c("Overall", "Missing", "Complete", "Male", "Female"),
  v.names = "sd",
  idvar = c("misP", "hand")
)


# Merge all data
combined_long <- merge(rmse_long, bias_long, by = c("misP", "hand", "metric"))
combined_long <- merge(combined_long, sd_long, by = c("misP", "hand", "metric"))


# Convert metric to factor with specified order
combined_long$metric <- factor(combined_long$metric,
                               levels = c("Overall", "Missing", "Complete", "Female", "Male"))

# Plot RMSE
rmse_plot <- ggplot(combined_long, aes(x = factor(misP), y = RMSE, group = hand, color = hand)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  facet_wrap(~metric, nrow = 1) +
  scale_x_discrete(labels = c("20%", "50%", "80%")) +
  scale_color_manual(
    name = "Handling Method",
    values = c("omit" = "#E69F00", "roughfix" = "#56B4E9", "rfimpute" = "#009E73"),
    labels = c("Listwise Deletion", "Single Imputation", "Proximity Imputation")
  ) +
  labs(x = "Proportion of Missing Values", y = "RMSE") +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"),
    legend.position = "bottom"
    )

# Plot Bias
bias_plot <- ggplot(combined_long, aes(x = factor(misP), y = Bias, group = hand, color = hand)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Bias - sd, ymax = Bias + sd), 
                width = 0.1, linewidth = 0.8) +
  facet_wrap(~metric, nrow = 1) +
  scale_x_discrete(labels = c("20%", "50%", "80%")) +
  scale_color_manual(
    name = "Handling Method",
    values = c("omit" = "#E69F00", "roughfix" = "#56B4E9", "rfimpute" = "#009E73"),
    labels = c("Listwise Deletion", "Single Imputation", "Proximity Imputation")
  ) +
  labs(x = "Proportion of Missing Values", y = "Bias") +
  theme_minimal() +
  theme( 
    axis.line = element_line(colour = "black"),
    legend.position = "bottom"
  )

print(bias_plot)
print(rmse_plot)

# Save plots
ggsave("viz/rmse_plot.png", rmse_plot, width = 12, height = 6)
ggsave("viz/bias_plot.png", bias_plot, width = 12, height = 6)


####
#### Plot Mean Decrease Gini for variable importance
###

# Reshape Gini data into long format
gini_long <- reshape(
  results,
  direction = "long",
  varying = grep("^gini_", names(results), value = TRUE),
  timevar = "variable",
  times = gsub("gini_", "", grep("^gini_", names(results), value = TRUE)),
  v.names = "importance",
  idvar = c("misP", "hand", "rep")
)


# Get baseline variable importance order
baseline_imp <- readRDS("data/var_imp_baseline.rds")
variable_order <- baseline_imp$Variable[order(baseline_imp$Rank, decreasing = T)]

# Order variables by baseline importance
gini_long$variable <- factor(gini_long$variable, levels = variable_order)

# Plot
gini_strip <- ggplot(gini_long, aes(x = importance, y = variable, color = hand)) +
  geom_jitter(alpha = 0.2, height = 0.2, size = 1.5) +
  facet_wrap(~misP, nrow = 1, 
             labeller = labeller(misP = c("0.2" = "20%", "0.5" = "50%", "0.8" = "80%"))) +
  scale_color_manual(
    name = "Handling Method",
    values = c("omit" = "#E69F00", "roughfix" = "#56B4E9", "rfimpute" = "#009E73"),
    labels = c("Listwise Deletion", "Single Imputation", "Proximity Imputation")
  ) +
  labs(
    x = "Mean Decrease Gini",
    y = "Variable",
    title = "Percentage of Missing Data" # Not actually the title, but the general label for the three facets 
  ) +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        strip.background = element_rect(fill = "gray90", color = NA),
        strip.text = element_text(face = "bold"),
        panel.spacing = unit(1.5, "lines"),
        plot.title = element_text(hjust = 0.55, size = 11, face = "plain", # Not actually the title
                                  margin = margin(b = 15, t = 5)),
        plot.title.position = "plot")

print(gini_strip)
