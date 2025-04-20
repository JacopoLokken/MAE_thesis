library(randomForest)
setwd("C:\\Users\\jacop\\OneDrive\\Desktop\\Thesis\\Code_Thesis")
set.seed(1729)

####
#### Setting up data ####
####
dat <- readRDS("data/cleaned_titanic_data.rds")
preds <- readRDS("data/rf_predictors.rds")

####
#### Simulation ####
#### 
reps <- 1000 # Number of repetitions in the simulation

# Results dataframe with additional columns for ranks and Gini importance
results <- data.frame(
  misP = numeric(),        # proportion missing responses
  rep = numeric(),         # replication 
  hand = character(),      # handling of missing data
  oob_all = numeric(),     # oob error rate for all cases
  oob_missing = numeric(), # oob error rate for cases that had missing values
  oob_complete = numeric(),# oob error rate for cases that were complete
  oob_male = numeric(),    # oob error rate for males
  oob_female = numeric(),  # oob error rate for females
  n_missing = numeric(),   # number of cases that had missing values
  n_complete = numeric(),  # number of cases that were complete
  
  # Variable ranks based on Mean Decrease Accuracy
  rank_pclass = numeric(),
  rank_sex = numeric(),
  rank_age = numeric(),
  rank_sibsp = numeric(), 
  rank_parch = numeric(),
  rank_fare = numeric(),
  
  # Gini importance values
  gini_pclass = numeric(),
  gini_sex = numeric(),
  gini_age = numeric(),
  gini_sibsp = numeric(), 
  gini_parch = numeric(),
  gini_fare = numeric(),
  
  stringsAsFactors = FALSE
)

ind <- 1
for (misP in c(0.2, 0.5, 0.8)) {
  print(misP)
  for (r in 1:reps) {
    results[ind:(ind + 2), "misP"] <- misP
    results[ind:(ind + 2), "rep"] <- r
    
    dat.r <- dat
    
    # Create missingness indicator and store it
    misI <- sample(c(1, 0), nrow(dat.r), replace = TRUE, prob = c(misP, 1 - misP))
    dat.r$sex[misI == 1] <- NA
    
    # Store number of missing/complete cases
    results[ind:(ind + 2), "n_missing"] <- sum(misI)
    results[ind:(ind + 2), "n_complete"] <- sum(!misI)
    
    # 1. Roughfix
    results[ind, "hand"] <- "roughfix"
    dat.r.roughfix <- na.roughfix(dat.r)
    rf <- randomForest(dat.r.roughfix[, preds], dat.r.roughfix$survived, importance=TRUE)
    
    # Get OOB predictions
    oob_preds <- predict(rf, type = "response")
    oob_errors <- oob_preds != dat.r.roughfix$survived
    
    # Calculate error rates for different groups
    results[ind, "oob_all"] <- rf$err.rate[nrow(rf$err.rate), "OOB"]
    results[ind, "oob_missing"] <- mean(oob_errors[misI == 1])
    results[ind, "oob_complete"] <- mean(oob_errors[misI == 0])
    results[ind, "oob_male"] <- mean(oob_errors[dat.r.roughfix$sex == "male"])
    results[ind, "oob_female"] <- mean(oob_errors[dat.r.roughfix$sex == "female"])
    
    # Extract and store variable importance ranks and Gini values
    imp_mda <- importance(rf)[, "MeanDecreaseAccuracy"]
    imp_gini <- importance(rf)[, "MeanDecreaseGini"]
    rnk <- rank(-imp_mda)  # Higher importance = lower rank number
    
    for (v in 1:length(preds)) {
      var_name <- preds[v]
      results[ind, paste0("rank_", var_name)] <- rnk[v]
      results[ind, paste0("gini_", var_name)] <- imp_gini[v]
    }
    
    # 2. Omit with Option 1b (majority class assignment for missing cases)
    results[ind + 1, "hand"] <- "omit"
    
    # Create a subset with only complete cases for training
    dat.r.omit <- na.omit(dat.r)
    
    # Train Random Forest on complete cases only
    rf <- randomForest(dat.r.omit[, preds], dat.r.omit$survived, importance=TRUE)
    
    # Extract and store variable importance ranks and Gini values
    imp_mda <- importance(rf)[, "MeanDecreaseAccuracy"]
    imp_gini <- importance(rf)[, "MeanDecreaseGini"]
    rnk <- rank(-imp_mda)
    
    for (v in 1:length(preds)) {
      var_name <- preds[v]
      results[ind + 1, paste0("rank_", var_name)] <- rnk[v]
      results[ind + 1, paste0("gini_", var_name)] <- imp_gini[v]
    }
    
    # Create a prediction vector for all observations in the original dataset
    all_preds <- rep(NA, nrow(dat.r))
    
    # Identify which observations have complete data
    complete_mask <- complete.cases(dat.r[, c("sex", preds)])
    
    # For complete cases, use the Random Forest's OOB predictions
    all_preds[complete_mask] <- rf$predicted
    
    # For cases with missing values, use majority class from the dataset
    majority_class <- names(which.max(table(dat.r.omit$survived)))
    all_preds[!complete_mask] <- majority_class
    
    # Calculate error rates for different groups by comparing as character strings
    all_errors <- as.character(all_preds) != as.character(dat.r$survived)
    
    # Store results in the results dataframe
    results[ind + 1, "oob_all"] <- mean(all_errors)
    results[ind + 1, "oob_missing"] <- mean(all_errors[!complete_mask])
    results[ind + 1, "oob_complete"] <- mean(all_errors[complete_mask])
    
    # For gender-specific error rates, we need to handle missing sex values
    results[ind + 1, "oob_male"] <- mean(all_errors[!is.na(dat.r$sex) & dat.r$sex == "male"])
    results[ind + 1, "oob_female"] <- mean(all_errors[!is.na(dat.r$sex) & dat.r$sex == "female"])
    
    # 3. RF Impute
    results[ind + 2, "hand"] <- "rfimpute"
    rf.formula <- as.formula(paste("survived ~", paste(preds, collapse = "+")))
    dat.r.imputed <- rfImpute(rf.formula, data = dat.r, iter = 5, ntree = 300)
    rf <- randomForest(dat.r.imputed[, preds], dat.r.imputed$survived, importance=TRUE)
    
    # Get OOB predictions
    oob_preds <- predict(rf, type = "response")
    oob_errors <- oob_preds != dat.r.imputed$survived
    
    results[ind + 2, "oob_all"] <- rf$err.rate[nrow(rf$err.rate), "OOB"]
    results[ind + 2, "oob_missing"] <- mean(oob_errors[misI == 1])
    results[ind + 2, "oob_complete"] <- mean(oob_errors[misI == 0])
    results[ind + 2, "oob_male"] <- mean(oob_errors[dat.r.imputed$sex == "male"])
    results[ind + 2, "oob_female"] <- mean(oob_errors[dat.r.imputed$sex == "female"])
    
    # Extract and store variable importance ranks and Gini values
    imp_mda <- importance(rf)[, "MeanDecreaseAccuracy"]
    imp_gini <- importance(rf)[, "MeanDecreaseGini"]
    rnk <- rank(-imp_mda)
    
    for (v in 1:length(preds)) {
      var_name <- preds[v]
      results[ind + 2, paste0("rank_", var_name)] <- rnk[v]
      results[ind + 2, paste0("gini_", var_name)] <- imp_gini[v]
    }
    
    ind <- ind + 3
    if (r %% 100 == 0) print(paste("Completed replication", r))
  }
}

saveRDS(results, "running_experiment/simulation_results1.rds")