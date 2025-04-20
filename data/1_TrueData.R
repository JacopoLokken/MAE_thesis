library("randomForest")
library("cranlogs")
library("ggplot2")
library("PASWR2")
setwd("C:\\Users\\jacop\\OneDrive\\Desktop\\Thesis\\Code_Thesis")
set.seed(12)

#### Random Forest packages downloads overview ####
rf_packages <- c("randomForest", "ranger", "party", "randomForestSRC", "h2o", "Rborist")

# Get data for last 5 years (1825 days)
last_year <- cran_downloads(packages = rf_packages,
                            from = Sys.Date() - 1070,
                            to = Sys.Date())
last_year$month <- format(last_year$date, "%Y-%m") # And extract month

# Aggregate and sort the data
monthly_data <- aggregate(count ~ package + month, data = last_year, FUN = sum)
names(monthly_data)[names(monthly_data) == "count"] <- "monthly_downloads"
monthly_data <- monthly_data[order(monthly_data$month, -monthly_data$monthly_downloads), ]

# Create visualization with regular numbers on y-axis
ggplot(monthly_data, 
       aes(x = month, y = monthly_downloads, color = package, group = package)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(
    x = "Month",
    y = "Number of Downloads",
    color = "Package"
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(breaks = unique(monthly_data$month)[seq(1, length(unique(monthly_data$month)), by = 4)])



####
####  Setting up dataset ####
####
preds <- c("pclass", "sex", "age", "sibsp", "parch", "fare")
dat <- TITANIC3
dat <- dat[,c("pclass","sex","age","sibsp","parch","fare","survived")]
dat <- na.omit(dat)
dat$survived <- as.factor(dat$survived)
saveRDS(dat, "data/cleaned_titanic_data.rds")
saveRDS(preds, "data/rf_predictors.rds")

####
#### Variable importance ####
####

# Checking variable importance by running a random forest on the complete dataset
rf.complete <- randomForest(dat[,c(preds)], dat$survived, 
                            importance=TRUE)  # Enable importance calculation
importance.complete <- importance(rf.complete)  # This will give both Mean Decrease Accuracy and Mean Decrease Gini. 
print(importance(rf.complete)) 
MDA <- varImpPlot(rf.complete, # But we are interested in Mean Decrease Accuracy 
                  type = 1,
) # Plot the importance measures

# I customize it to make it APA - Compliant (deleting  text that I will add myself in word)
importance_accuracy <- importance.complete[, "MeanDecreaseAccuracy"]
dotchart(sort(importance_accuracy), 
         xlim = c(0, 100))

####
#### Descriptives ####
###
str(dat)
summary(dat) # no sense to have mean, sd, median of some variables

cat("\nPassenger Class (pclass):\n")
pclass_summary <- as.data.frame(table(dat$pclass))
names(pclass_summary) <- c("pclass", "Count")
pclass_summary$Percentage <- sprintf("%.2f%%", 100 * pclass_summary$Count / sum(pclass_summary$Count))
print(pclass_summary)

cat("\nGender (sex):\n")
sex_summary <- as.data.frame(table(dat$sex))
names(sex_summary) <- c("sex", "Count")
sex_summary$Percentage <- sprintf("%.2f%%", 100 * sex_summary$Count / sum(sex_summary$Count))
print(sex_summary)

cat("\nSurvival Status (survived):\n")
surv_summary <- as.data.frame(table(dat$survived))
names(surv_summary) <- c("survived", "Count")
surv_summary$Percentage <- sprintf("%.2f%%", 100 * surv_summary$Count / sum(surv_summary$Count))
print(surv_summary)

# For categorical count variables (How many sibling and/or spouses for each passenger. Same for parents and/or children)
cat("\nNumber of Siblings/Spouses aboard (sibsp):\n")
sibsp_summary <- as.data.frame(table(dat$sibsp))
names(sibsp_summary) <- c("sibsp", "Count")
sibsp_summary$Percentage <- sprintf("%.2f%%", 100 * sibsp_summary$Count / sum(sibsp_summary$Count))
print(sibsp_summary)

cat("\nNumber of Parents/Children aboard (parch):\n")
parch_summary <- as.data.frame(table(dat$parch))
names(parch_summary) <- c("parch", "Count")
parch_summary$Percentage <- sprintf("%.2f%%", 100 * parch_summary$Count / sum(parch_summary$Count))
print(parch_summary)

# For continuous  variables
cat("\nAge:\n")
age_stats <- c(
  N = length(dat$age),
  Mean = round(mean(dat$age), 2),
  SD = round(sd(dat$age), 2),
  Median = round(median(dat$age), 2),
  Min = round(min(dat$age), 2),
  Max = round(max(dat$age), 2)
)
print(age_stats)

cat("\nFare:\n")
fare_stats <- c(
  N = length(dat$fare),
  Mean = round(mean(dat$fare), 2),
  SD = round(sd(dat$fare), 2),
  Median = round(median(dat$fare), 2),
  Min = round(min(dat$fare), 2),
  Max = round(max(dat$fare), 2)
)
print(fare_stats)

# Save descriptive statistics
saveRDS(list(
  pclass_summary = pclass_summary,
  sex_summary = sex_summary,
  surv_summary = surv_summary,
  sibsp_summary = sibsp_summary,
  parch_summary = parch_summary,
  age_stats = age_stats,
  fare_stats = fare_stats
), "data/descriptive_stats.rds")

####
#### Correlations ####
####

# Function to determine the type of a variable. Might be useful if deciding to add more variables and they are coded differently
get_var_type <- function(x) {
  if (is.numeric(x)) {
    if (length(unique(x)) == 2) { # If numeric with 2 unique values --> Dichotomous...
      return("dichot")
    } else {
      return("continuous") #...otherwise continuous 
    }
  } else if (is.factor(x)) { # if not numeric, check if it's a factor
    if (nlevels(x) == 2) { # If said factor has exactly 2 levels...
      return("dichot") # ...return dichotomous
    } else {
      return("categorical") # otherwise categorical
    }
  } else {
    stop("Variable type not included!") # just in case
  }
}

# Function to converts variables for  correlation calculation
convert_for_corr <- function(x, type) {
  if (type == "dichot") { # For dichotomous variables, we recode the first level as 0 and the second as 1.
    if (is.factor(x)) {
      # Recode factor: first level becomes 0, second becomes 1.
      return(ifelse(x == levels(x)[1], 0, 1))
    } else {
      # NOTE: Assuming numeric dichotomous variables are already coded 0/1.
      return(x)
    }
  } else if (type == "continuous") { # If type is continuous, convert it to numeric
    return(as.numeric(x))
  } else if (type == "categorical") {
    # For categorical variables, using the underlying numeric codes is okay for spoearman's ro
    if (is.factor(x)) {
      return(as.numeric(x))
    } else {
      return(as.numeric(x))
    }
  } else {
    stop("Variable type not included!") # Again, just in case new added variable doesn't match
  }
}

# Function to compute the correlation between two variables based on their types.
compute_corr <- function(x, y, type_x, type_y) { 
  x_num <- convert_for_corr(x, type_x)
  y_num <- convert_for_corr(y, type_y)
  
  if (type_x == "dichot" && type_y == "dichot") { # If dichotomous vs. dichotomous...
    return(cor(x_num, y_num, method = "pearson")) # ...Phi coefficient: just a pearson correlation betweenn dichotomous variables. Note: not comparable to other coefficients
  } else if (type_x == "continuous" && type_y == "continuous") {   # If continuous vs. continuous... 
    return(cor(x_num, y_num, method = "pearson")) # ...Pearson's r for continuous variables.
  } else if ((type_x == "continuous" && type_y == "dichot") || # If dichotomous vs. categorical/count or vs. continuous ...
             (type_x == "dichot" && type_y == "continuous") ||
             (type_x == "categorical" && type_y == "dichot") ||
             (type_x == "dichot" && type_y == "categorical")) {
    return(cor(x_num, y_num, method = "pearson")) #... Point-Biserial correlation: just a Pearson correlation with the above data types. Note: not comparable to other coefficients.
  } else if ((type_x == "continuous" && type_y == "categorical") || # If continuous vs. categorical/count or categroical/count vs. categorical/count...
             (type_x == "categorical" && type_y == "continuous") ||
             (type_x == "categorical" && type_y == "categorical")) { # ...Spearman's rho for continuous vs. categorical/count or categorical/count vs. categorical/count
    
    return(cor(x_num, y_num, method = "spearman"))
  } else {
    return(NA)
  }
}

vars <- names(dat) # store variables
var_types <- sapply(dat, get_var_type) # apply variable type function
corr_matrix <- matrix(NA, nrow = length(vars), ncol = length(vars), # empty correlation matrix
                      dimnames = list(vars, vars))

for (i in 1:length(vars)) { # Loop over all pairs of variables to compute the appropriate correlation
  for (j in i:length(vars)) {
    corr_val <- compute_corr(dat[[vars[i]]], dat[[vars[j]]],
                             var_types[vars[i]], var_types[vars[j]])
    corr_matrix[i, j] <- corr_val
    corr_matrix[j, i] <- corr_val  # fill in the matrix
  }
}

# Display the correlation matrix, and save it
print(round(corr_matrix, 3))
saveRDS(corr_matrix, "data/correlation_matrix.rds")

####
#### Distributions of variables by sex ####
###
sex_distribution <- function(data) {
  # Total counts by sex
  sex_counts <- table(data$sex)
  sex_percentages <- prop.table(sex_counts) * 100
  cat("Overall sex distribution:\n")
  print(data.frame(
    Sex = names(sex_counts),
    Count = sex_counts,
    Percentage = sprintf("%.1f%%", sex_percentages)
  ))
  
  # For the categorical predictors & dichotomous outcome
  cat("\nPassenger class (pclass) by sex:\n")
  print(table(data$sex, data$pclass))
  
  cat("\nSiblings/Spouses (sibsp) by sex:\n")
  print(table(data$sex, data$sibsp))
  
  cat("\nParents/Children (parch) by sex:\n")
  print(table(data$sex, data$parch))
  
  cat("\nSurvival by sex:\n")
  print(table(data$sex, data$survived))
  
  # For continuous variables, just show means
  cat("\nMean age by sex:\n")
  print(tapply(data$age, data$sex, mean))
  
  cat("\nMean fare by sex:\n")
  print(tapply(data$fare, data$sex, mean))
}

sex_distribution(dat)

# Save sex distribution information
sex_dist_info <- list(
  sex_counts = table(dat$sex),
  sex_by_pclass = table(dat$sex, dat$pclass),
  sex_by_sibsp = table(dat$sex, dat$sibsp),
  sex_by_parch = table(dat$sex, dat$parch),
  sex_by_survived = table(dat$sex, dat$survived),
  age_by_sex = tapply(dat$age, dat$sex, mean),
  fare_by_sex = tapply(dat$fare, dat$sex, mean)
)

saveRDS(sex_dist_info, "data/sex_distribution.rds")

####
#### TRUE DATA - UPPER AND LOWER BOUND ####
####


#### Baseline model - complete dataset
rf_baseline <- randomForest(dat[,c(preds)], dat$survived, importance = T)
oobBaseline <- rf_baseline$err.rate[nrow(rf_baseline$err.rate), "OOB"] # Overall error rate

# Gender specific error rates for baseline
oob_preds_baseline <- predict(rf_baseline, type = "response") # Gets OOB predictions from our baseline model
oob_errors_baseline <- oob_preds_baseline != dat$survived # Boolean vector where if prediction error, on the complete dataset
oob_baseline_male <- mean(oob_errors_baseline[dat$sex == "male"]) # Mean OOB for males
oob_baseline_female <- mean(oob_errors_baseline[dat$sex == "female"]) # Mean OOB for females

# Importance for Baseline model
imp_baseline <- importance(rf_baseline)[, "MeanDecreaseAccuracy"]
rank_baseline <- rank(-imp_baseline)  # Negative sign to rank in descending order
var_imp_baseline <- data.frame(
  Variable = names(imp_baseline),
  MeanDecreaseAccuracy = imp_baseline,
  Rank = rank_baseline
)
var_imp_baseline <- var_imp_baseline[order(var_imp_baseline$Rank), ]
imp_matrix_baseline <- importance(rf_baseline)

##### Lower bound model - without sex variable
rf_lower <- randomForest(dat[,c("pclass","age","sibsp","parch","fare")], dat$survived, importance = T)
oobLower <- rf_lower$err.rate[nrow(rf_lower$err.rate), "OOB"] # Overall error rates

# Gender specific error rates (same as did above for baseline model)
oob_preds_lower <- predict(rf_lower, type = "response") 
oob_errors_lower <- oob_preds_lower != dat$survived
oob_lower_male <- mean(oob_errors_lower[dat$sex == "male"])
oob_lower_female <- mean(oob_errors_lower[dat$sex == "female"])

# Importance for lower bound model
imp_lower <- importance(rf_lower)[, "MeanDecreaseAccuracy"]
rank_lower <- rank(-imp_lower)
var_imp_lower <- data.frame(
  Variable = names(imp_lower),
  MeanDecreaseAccuracy = imp_lower,
  Rank = rank_lower
)
var_imp_lower <- var_imp_lower[order(var_imp_lower$Rank), ]
imp_matrix_lower <- importance(rf_lower)

# Print results
print(var_imp_baseline)
print(var_imp_lower)

# Create dataframe for results
reference_results <- data.frame(
  Model = c("Baseline (Complete Data)", "Without Sex Variable"),
  Overall_OOB = c(oobBaseline, oobLower),
  Male_OOB = c(oob_baseline_male, oob_lower_male),
  Female_OOB = c(oob_baseline_female, oob_lower_female)
)
saveRDS(rf_baseline, "data/rf_baseline_model.rds")
saveRDS(rf_lower, "data/rf_lower_model.rds")
saveRDS(reference_results, "data/reference_results.rds")
saveRDS(var_imp_baseline, "data/var_imp_baseline.rds")
saveRDS(var_imp_lower, "data/var_imp_lower.rds")


