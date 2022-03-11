
# This script builds multiple GBMs to model frequency
# I will use an additive model attemot soi we can interpret results 

# Load packages
library(plyr)
library(dplyr)
library(dbplyr)
library(tidyr)
library(caret)
library(fastDummies)
library(lubridate)
library(readxl)
library(stringr)
library(gbm)
library(ModelMetrics)

# Load data
frequency_data <- readRDS("/Volumes/GoogleDrive/Shared drives/Pricing & Underwriting/Data Science/UK/UK_risk_GBM/data/frequency_data_23062021")

# I have acouple of ideas for factors to add
frequency_data <- within(frequency_data,{
  
  VEHICLEVALUEDEPRECIATION_1 <- VEHICLECOSTNEW - VEHICLEINDICATIVEVALUE
  VEHICLEVALUEDEPRECIATION_2 <- VEHICLECOSTNEW - VEHICLEUSEDPRIVATEPARTYVALUE
  VEHICLEVALUEDEPRECIATION_3 <- VEHICLECOSTNEW - VEHICLEUSEDTRADEINVALUE
  
  VEHICLEVALUEDEPRECIATION_1 <- ifelse(VEHICLEVALUEDEPRECIATION_1 < 0, 0, VEHICLEVALUEDEPRECIATION_1)
  VEHICLEVALUEDEPRECIATION_2 <- ifelse(VEHICLEVALUEDEPRECIATION_2 < 0, 0, VEHICLEVALUEDEPRECIATION_2)
  VEHICLEVALUEDEPRECIATION_3 <- ifelse(VEHICLEVALUEDEPRECIATION_3 < 0, 0, VEHICLEVALUEDEPRECIATION_3)
  
  VEHICLEVALUEDEPRECIATION_PERC1 <- VEHICLEINDICATIVEVALUE/VEHICLECOSTNEW
  VEHICLEVALUEDEPRECIATION_PERC2 <- VEHICLEUSEDPRIVATEPARTYVALUE/VEHICLECOSTNEW
  VEHICLEVALUEDEPRECIATION_PERC3 <- VEHICLEUSEDTRADEINVALUE/VEHICLECOSTNEW
  
  VEHICLEVALUEDEPRECIATION_PERC1 <- ifelse(VEHICLEVALUEDEPRECIATION_PERC1 > 1, 1, VEHICLEVALUEDEPRECIATION_PERC1)
  VEHICLEVALUEDEPRECIATION_PERC2 <- ifelse(VEHICLEVALUEDEPRECIATION_PERC2 > 1, 1, VEHICLEVALUEDEPRECIATION_PERC2)
  VEHICLEVALUEDEPRECIATION_PERC3 <- ifelse(VEHICLEVALUEDEPRECIATION_PERC3 > 1, 1, VEHICLEVALUEDEPRECIATION_PERC3)
  
})

# Remove any rows that correspond to the most recent quarter
frequency_data <- filter(frequency_data, ACCIDENTQUARTER_2021_Q2 == 0)

# To incorprate the effects of time, I will build a glm that fits by quarter and then offset in the main model
time_glm <- glm(TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS ~ 
                  ACCIDENTQUARTER_2018_Q4 +
                  ACCIDENTQUARTER_2019_Q1 +
                  ACCIDENTQUARTER_2019_Q2 +
                  ACCIDENTQUARTER_2019_Q3 +
                  ACCIDENTQUARTER_2019_Q4 +
                  ACCIDENTQUARTER_2020_Q1 +
                  ACCIDENTQUARTER_2020_Q2 +
                  ACCIDENTQUARTER_2020_Q4 +
                  ACCIDENTQUARTER_2021_Q1 +
                  
                  offset(EARNEDVEHICLEYEARS_LOG)
                ,
                data = frequency_data,
                family = poisson(log))

frequency_data$time_prediction <- predict(time_glm, newdata = frequency_data, type = "response")

# Get test and train samples
set.seed(182)
model_data_index <- caret::createDataPartition(frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, p = .85, 
                                               list = FALSE, 
                                               times = 1)
model_data_train <- as.data.frame(frequency_data[model_data_index,])
model_data_test  <- as.data.frame(frequency_data[-model_data_index,])

for(i in 41:50){
  
  print(paste0("Run ", i, " started at: ", Sys.time()))
  
  if(i==1){number_trees <- 100}else{number_trees <- sample(c(500:5000), 1, replace=T)}
  interaction_depth <- 1
  min_bin_size <- sample(c(20:500),1, replace=T)
  if(i==1){shrinkage <- 0.05}else{shrinkage <- sample(c(1:10)/100,1, replace=T)}
  bag_fraction <- sample(c(2:10)/10,1, replace=T)
  train_fraction <- sample(c(75:100)/100,1, replace=T)
  cv_folds <- 5
  distribution <- "poisson"
  weights <- model_data_train$EARNEDVEHICLEYEARS_LOG
  
  print(paste0("Number trees = ", number_trees))
  print(paste0("Interaction depth = ", interaction_depth))
  print(paste0("Min bin size = ", min_bin_size))
  print(paste0("Shrinkage = ", shrinkage))
  print(paste0("Bag fraction = ", bag_fraction))
  print(paste0("Train fraction = ", train_fraction))
  
  startTime <- Sys.time()
  rating_gbm <- gbm(TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS ~ 
                      NB_QTI
                    + TERM
                    + DRIVERAGE
                    + LICENCEYEARS
                    + TAXILICENCEYEARS
                    + LICENCEYEARS_BEFORETAXI
                    + LICENCEGROUP_1
                    + LICENCEGROUP_2
                    + LICENCEGROUP_3
                    + LICENCEGROUP_4
                    + LICENCEGROUP_5
                    + LICENCEGROUP_6
                    + LICENCEGROUP_7
                    + LICENCEGROUP_8
                    + LICENCEGROUP_9
                    + LICENCEGROUP_10
                    + LICENCEGROUP_11
                    + LICENCEGROUP_12
                    + LICENCEGROUP_13
                    + LICENCEGROUP_14
                    + LICENCEGROUP_16
                    + LICENCEGROUP_17
                    + LICENCEGROUP_18
                    + LICENCEGROUP_19
                    
                    + PREVIOUSCLAIMS
                    + PREVIOUSFAULTCLAIMS
                    + PREVIOUSNONFAULTCLAIMS
                    + PREVIOUSFIREANDTHEFTCLAIMS
                    + POINTS
                    + LASTCALCULATEDDEMERITPOINTS
                    #+ NOCLAIMSDISCOUNT
                    #+ NCDUNKNOWN
                    #+ PROTECTEDNOCLAIMSDISCOUNT
                    + SEATINGCAPACITY
                    + INSURANCEGROUP
                    + VEHICLEAGE
                    + ENGINESIZE
                    + VEHICLECOSTNEW
                    + VEHICLEINDICATIVEVALUE
                    + VEHICLEUSEDPRIVATEPARTYVALUE
                    + VEHICLEUSEDTRADEINVALUE
                    +	OVERNIGHTLOCATIONAREAGROUP
                    +	COVERTYPE_Gold
                    +	COVERTYPE_Silver
                    +	VEHICLEMAKEBANDED_BMW
                    +	VEHICLEMAKEBANDED_CITROEN
                    +	VEHICLEMAKEBANDED_FORD
                    +	VEHICLEMAKEBANDED_HONDA
                    +	VEHICLEMAKEBANDED_HYUNDAI
                    +	VEHICLEMAKEBANDED_MERCEDESBENZ
                    +	VEHICLEMAKEBANDED_OTHER
                    +	VEHICLEMAKEBANDED_SKODA
                    +	VEHICLEMAKEBANDED_VAUXHALL
                    +	VEHICLEMAKEBANDED_VOLKSWAGEN
                    +	VEHICLETYPE_COUPE_SPORT
                    +	VEHICLETYPE_ESTATE
                    +	VEHICLETYPE_HATCHBACK
                    +	VEHICLETYPE_MPV
                    +	VEHICLETYPE_SALOON
                    + VEHICLEVALUEDEPRECIATION_1
                    + VEHICLEVALUEDEPRECIATION_2
                    + VEHICLEVALUEDEPRECIATION_3
                    + VEHICLEVALUEDEPRECIATION_PERC1
                    + VEHICLEVALUEDEPRECIATION_PERC2
                    + VEHICLEVALUEDEPRECIATION_PERC3
                    +	FUELTYPE_DIESEL
                    +	FUELTYPE_ELECTRIC
                    +	FUELTYPE_HYBRID_ELECTRIC
                    +	FUELTYPE_OTHER
                    +	FUELTYPE_PETROL
                    +	VEHICLETRANSMISSION_AUTOMATIC
                    +	VEHICLETRANSMISSION_MANUAL
                    +	VEHICLETRANSMISSION_SEMIAUTO
                    +	REGION_EAST_ANGLIA
                    +	REGION_EAST_MIDLANDS
                    +	REGION_NE_ENG
                    +	REGION_NW_ENG
                    +	REGION_SCOTLAND
                    +	REGION_SE_ENG
                    +	REGION_SW_ENG
                    +	REGION_WALES
                    +	REGION_WEST_MIDLANDS
                    +	REGION_YORKS_HUMBS
                    
                    + UBERAVERAGEMONTHLYTRIPS +
                      UBERPRO +
                      UBERONLYUBER +
                      UBEREXCLUSIVE +
                      UBERPROTIER_e_Diamond +
                      UBERPROTIER_c_Gold +
                      UBERPROTIER_a_NonUber_Pro +
                      UBERPROTIER_d_Platinum +
                      UBERRATING +
                      UBERTRIPS +
                      UBERTRIPSPASTYEAR +
                      UBERNEWDRIVER
                    
                    + offset(log(time_prediction))
                    ,
                    data = model_data_train,
                    distribution = distribution, 
                    weights = c(weights),
                    n.trees = number_trees, 
                    shrinkage = shrinkage,
                    interaction.depth = interaction_depth, 
                    bag.fraction = bag_fraction, 
                    train.fraction = train_fraction,
                    n.minobsinnode = min_bin_size, 
                    cv.folds = 5, 
                    keep.data = TRUE,
                    verbose = FALSE, 
                    n.cores = 1)
  
  endTime <- Sys.time()
  
  # Calculate run time
  model_run_time <- round(as.numeric(difftime(endTime, startTime, units = "secs")), 0)
  
  
  # Predict values for train and test samples
  model_data_train$prediction <- (predict(rating_gbm, newdata = model_data_train, n.trees = rating_gbm$n.trees, type = "response")*model_data_train[,"time_prediction"])
  model_data_test$prediction <- (predict(rating_gbm, newdata = model_data_test, n.trees = rating_gbm$n.trees, type = "response")*model_data_test[,"time_prediction"])
  
  
  # I want to look at predicted sd, does a bigger spread mean more range and better predictions?
  train_sd <- sd(model_data_train$prediction)
  test_sd <- sd(model_data_test$prediction)
  
  # Gini calc
  train_gini <- ModelMetrics::gini(model_data_train$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, model_data_train$prediction)
  test_gini <- ModelMetrics::gini(model_data_test$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, model_data_test$prediction)
  
  # Caluclate r-squared for both
  # I plan to choose the model with the best r-squared value
  train_r2 <- caret::R2(model_data_train$prediction, model_data_train$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS)
  test_r2 <- caret::R2(model_data_test$prediction, model_data_test$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS)
  
  # Calculate residuals
  model_data_train$residual <- model_data_train$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS - model_data_train$prediction
  train_residuals <- sum(model_data_train$residual)
  train_residual_sd <- sd(model_data_train$residual)
  model_data_test$residual <- model_data_test$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS - model_data_test$prediction
  test_residuals <- sum(model_data_test$residual)
  test_residual_sd <- sd(model_data_test$residual)
  
  # Mean predictions for flagged and unflagged
  with_claim_train <- filter(model_data_train, TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS > 0)
  mean_pred_claim_train <- mean(with_claim_train$prediction, na.rm=T)
  no_claim_train <- filter(model_data_train, TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS == 0)
  mean_pred_noclaim_train <- mean(no_claim_train$prediction, na.rm=T)
  mean_diff_train <- mean_pred_claim_train - mean_pred_noclaim_train
  
  with_claim_test <- filter(model_data_test, TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS > 0)
  mean_pred_claim_test <- mean(with_claim_test$prediction, na.rm=T)
  no_claim_test <- filter(model_data_test, TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS == 0)
  mean_pred_noclaim_test <- mean(no_claim_test$prediction, na.rm=T)
  mean_diff_test <- mean_pred_claim_test - mean_pred_noclaim_test
  
  # Save the model
  model_save_name <- paste0('RDM_GBM_', i)
  saveRDS(rating_gbm, paste0("/Volumes/GoogleDrive/Shared drives/Pricing & Underwriting/Data Science/UK/UK_risk_GBM/models/", model_save_name, ".rds"))
  
  # Collate the model stats and r-squared
  model_info_single <- data.frame("run" = i, 
                                  "GBM_name" = model_save_name,
                                  "model_run_time" = model_run_time,
                                  "number_trees" = number_trees,
                                  "interaction_depth" = interaction_depth,
                                  "min_bin_size" = min_bin_size,
                                  "shrinkage" = shrinkage,
                                  "bag_fraction" = bag_fraction,
                                  "train_fraction" = train_fraction,
                                  "train_gini" = train_gini,
                                  "test_gini" = test_gini,
                                  "train_r2" = train_r2,
                                  "test_r2" = test_r2,
                                  "train_sd" = train_sd,
                                  "test_sd" = test_sd,
                                  "train_residuals" = train_residuals,
                                  "test_residuals" = test_residuals,
                                  "train_residual_sd" = train_residual_sd,
                                  "test_residual_sd" = test_residual_sd,
                                  "mean_pred_claim_train" = mean_pred_claim_train,
                                  "mean_pred_noclaim_train" = mean_pred_noclaim_train,
                                  "mean_diff_train" = mean_diff_train,
                                  "mean_pred_claim_test" = mean_pred_claim_test,
                                  "mean_pred_noclaim_test" = mean_pred_noclaim_test,
                                  "mean_diff_test" = mean_diff_test
  )
  
  # Collate multiple results
  if(i == 1){model_info <- model_info_single}else{model_info<-rbind(model_info, model_info_single)}
  
  # Save the iterated table, just in case
  saveRDS(model_info, paste0("/Volumes/GoogleDrive/Shared drives/Pricing & Underwriting/Data Science/UK/UK_risk_GBM/model_info.rds"))
  
  print(paste0("Run ", i, " completed at: ", Sys.time()))
  
}

summary(rating_gbm)
frequency_data$prediction <- predict(rating_gbm, newdata = frequency_data, n.trees = rating_gbm$n.trees, type = "response")*frequency_data[,"time_prediction"]
source("/Volumes/GoogleDrive/Shared drives/Pricing & Underwriting/Data Science/R_Resources/Functions/AvsE.r")
AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$UBERRATINGBANDED, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)




