setwd("C:\\Users\\jacop\\OneDrive\\Desktop\\Thesis\\Code_Thesis")

# Loading simulation results
results <-readRDS("running_experiment/simulation_results1.rds")

# Loading true data results results to print them later next to simulation results
reference_results <-readRDS("data/reference_results.rds")
rf_baseline <-readRDS("data/rf_baseline_model.rds")
rf_lower <- readRDS("data/rf_lower_model.rds")
var_imp_baseline <-readRDS("data/var_imp_baseline.rds")
var_imp_lower <- readRDS("data/var_imp_lower.rds")

# Extracting OOB estimates form RF with complete data to later compute RMSE and BIAS
oobBaseline <- reference_results$Overall_OOB[reference_results$Model == "Baseline (Complete Data)"]
oob_baseline_male <- reference_results$Male_OOB[reference_results$Model == "Baseline (Complete Data)"]
oob_baseline_female <- reference_results$Female_OOB[reference_results$Model == "Baseline (Complete Data)"]

####
#### Aggregating results from simulation ####
####
# Overall means for oob_all (include omit)
aggregate_oob_all <- aggregate(oob_all ~ misP + hand, data = results, mean, na.rm = TRUE)
# Overall means for oob_missing and oob_complete (exclude omit)
aggregate_missing_complete <- aggregate(cbind(oob_missing, oob_complete) ~ misP + hand, 
                                        data = results, mean, na.rm = TRUE)
# Overall means for oob_male and oob_female (include omit)
aggregate_gender <- aggregate(cbind(oob_male, oob_female) ~ misP + hand, 
                              data = results, mean, na.rm = TRUE)
# Merge the results
aggregate_results <- merge(aggregate_oob_all, aggregate_missing_complete, by = c("misP", "hand"), all = TRUE)
aggregate_results <- merge(aggregate_results, aggregate_gender, by = c("misP", "hand"), all = TRUE)

# Same as above for standard deviations 
aggregate_sd_oob_all <- aggregate(oob_all ~ misP + hand, data = results, sd, na.rm = TRUE)
aggregate_sd_missing_complete <- aggregate(cbind(oob_missing, oob_complete) ~ misP + hand, 
                                           data = results, sd, na.rm = TRUE)
aggregate_sd_gender <- aggregate(cbind(oob_male, oob_female) ~ misP + hand, 
                                 data = results, sd, na.rm = TRUE)
aggregate_sd <- merge(aggregate_sd_oob_all, aggregate_sd_missing_complete, by = c("misP", "hand"), all = TRUE)
aggregate_sd <- merge(aggregate_sd, aggregate_sd_gender, by = c("misP", "hand"), all = TRUE)

# Computing and aggregating RMSE 
aggregate_rmse_oob_all <- aggregate(oob_all ~ misP + hand, data = results,
                                    function(x) sqrt(mean((x - oobBaseline)^2, na.rm = TRUE)))
aggregate_rmse_missing_complete <- aggregate(cbind(oob_missing, oob_complete) ~ misP + hand, data = results,
                                             function(x) sqrt(mean((x - oobBaseline)^2, na.rm = TRUE)))
aggregate_rmse_gender <- aggregate(cbind(oob_male, oob_female) ~ misP + hand, data = results,
                                   function(x) sqrt(mean((x - oobBaseline)^2, na.rm = TRUE)))
aggregate_rmse <- merge(aggregate_rmse_oob_all, aggregate_rmse_missing_complete, by = c("misP", "hand"), all = TRUE)
aggregate_rmse <- merge(aggregate_rmse, aggregate_rmse_gender, by = c("misP", "hand"), all = TRUE)

# Computing and aggregrating BIAS 
aggregate_bias_oob_all <- aggregate(oob_all ~ misP + hand, data = results,
                                    function(x) mean(x - oobBaseline, na.rm = TRUE))
aggregate_bias_missing_complete <- aggregate(cbind(oob_missing, oob_complete) ~ misP + hand, data = results,
                                             function(x) mean(x - oobBaseline, na.rm = TRUE))
aggregate_bias_gender <- aggregate(cbind(oob_male, oob_female) ~ misP + hand, data = results,
                                   function(x) mean(x - oobBaseline, na.rm = TRUE))
aggregate_bias <- merge(aggregate_bias_oob_all, aggregate_bias_missing_complete, by = c("misP", "hand"), all = TRUE)
aggregate_bias <- merge(aggregate_bias, aggregate_bias_gender, by = c("misP", "hand"), all = TRUE)

# Print the results
print(rf_baseline$confusion)
print(var_imp_baseline)
print(rf_lower$confusion)
print(var_imp_lower)
print(reference_results)

print(aggregate_results)
print(aggregate_sd)
print(aggregate_rmse)
print(aggregate_bias)

# Saving them for visualization later
saveRDS(list(
  aggregate_results = aggregate_results,
  aggregate_sd = aggregate_sd,
  aggregate_rmse = aggregate_rmse,
  aggregate_bias = aggregate_bias
), "results_analysis/aggregated_results.rds")


####
#### Analysis of Rank Stability Across Replications ####
####
# Extract rank columns 
rank_cols <- grep("^rank_", names(results), value = TRUE)
rank_data <- results[, c("misP", "hand", "rep", rank_cols)]

# Calculate variance in ranks ACROSS REPLICATIONS for each method and missingness level
# This should show how consistent/stable the variable rankings are when we repeat the experiment
rank_var_by_method <- aggregate(. ~ misP + hand, 
                                data = rank_data[, !names(rank_data) %in% "rep"], 
                                FUN = var)

# Rename columns (for clarity) and print results
colnames(rank_var_by_method) <- gsub("rank_", "var_rank_", colnames(rank_var_by_method))
cat("Variance in Variable Importance Ranks Across Replications:\n")
print(rank_var_by_method)

# Calculate the average rank variance across all variables for each method
# Higher values --> less stable variable importance rankings
rank_var_by_method$avg_variance <- rowMeans(rank_var_by_method[, grep("var_rank_", names(rank_var_by_method))])

# Reshape to compare methods directly (one row per missing proportion)
rank_var_wide <- reshape(rank_var_by_method,
                         idvar = "misP",
                         timevar = "hand",
                         direction = "wide")

cat("\nComparison of Average Rank Variance Across Methods:\n")
print(rank_var_wide[, c("misP", "avg_variance.omit", "avg_variance.roughfix", "avg_variance.rfimpute")])

