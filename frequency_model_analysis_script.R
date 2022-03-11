# This script provides analysis for the frequency models

# Required packages
library(plyr)
library(dplyr)
library(dbplyr)
library(tidyr)
library(caret)
library(fastDummies)
library(lubridate)
library(readxl)
library(stringr)
library(xlsx)
library(openxlsx)
library(data.table)

##### This area has the bits needed to edit #####

# Load model
required_model <- readRDS("/Volumes/GoogleDrive/Shared drives/Pricing & Underwriting/Data Science/UK/UK_risk_GBM/models/RDM_GBM_8.rds")

# Excel save file for model parameters and relativity tables
model_parameter_excel_save_filename <- paste0("/Volumes/GoogleDrive/Shared drives/Pricing & Underwriting/Data Science/UK/UK_risk_GBM/pricing_model_iteration_v1_relativities.xlsx")

# Saving location for model factors AvsE graphs
model_factors_save_location <- paste0("/Volumes/GoogleDrive/Shared drives/Pricing & Underwriting/Data Science/UK/UK_risk_GBM/pricing_model_factors.pdf")

# Saving location for all factors AvsE graphs
all_factors_save_location <- paste0("/Volumes/GoogleDrive/Shared drives/Pricing & Underwriting/Data Science/UK/UK_risk_GBM/pricing_model_all_factors.pdf")

# Options to run
load_data <- TRUE
statistical_analysis <- TRUE
print_model_summary <- TRUE
calculate_realtivities <- TRUE
save_model_info <- TRUE
model_factors_avse_graphs <- TRUE
all_factors_avse_graphs <- TRUE
time_consistency <- TRUE
cross_validation <- TRUE
predicted_deciles <- TRUE

#################################################

# Load training data
if(load_data == TRUE){
  
  frequency_data <- readRDS("/Volumes/GoogleDrive/Shared drives/Pricing & Underwriting/Data Science/UK/UK_risk_GBM/data/frequency_data_27052021")

  frequency_data <- fastDummies::dummy_cols(frequency_data, 
                                            select_columns = c("LICENCEGROUP"),
                                            remove_most_frequent_dummy = TRUE,
                                            remove_selected_columns = FALSE)
}

# Model summary option
if(print_model_summary == TRUE){
  
  summary(required_model)

}

# Model statistics
if(statistical_analysis == TRUE){
  
  # Apply predictions
  frequency_data$prediction <- predict(required_model, newdata = frequency_data, n.trees = required_model$n.trees, type = "response")*frequency_data[,"time_prediction"]
  
  # Variable importance
  var_importance <-as.data.frame(relative.influence(required_model, n.trees = required_model$n.trees))
  names(var_importance) <- "Overall"
  var_importance$AbsOverall <- abs(var_importance$Overall)
  var_importance <- arrange(var_importance, desc(AbsOverall))
  var_importance <- tibble::rownames_to_column(var_importance, "Factors")
  names(var_importance) <- c("Factors", "Importance", "AbsImportance")
  
  # p values
  #p_values <- as.data.frame(coef(summary(required_model))[,4])
  #p_values <- tibble::rownames_to_column(p_values, "Factors")
  #names(p_values) <- c("Factors", "P-values")
  
  # Coefficients
  #coefficients <- as.data.frame(required_model$coefficients)
  #coefficients <- tibble::rownames_to_column(coefficients, "Factors")
  #names(coefficients) <- c("Factors", "Coefficients")
  
  # Combine the above
  #factor_summary <- left_join(var_importance, p_values, by = "Factors")
  #factor_summary <- left_join(factor_summary, coefficients, by = "Factors")
  
  factor_summary <- var_importance
  
  #rm(var_importance, p_values, coefficients)
  rm(var_importance)
  
  # Gini
  gini <- gini(frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, frequency_data$prediction)
  # Mean Absolute Error
  mae <- mae(frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, frequency_data$prediction)
  # Mean Squared Error
  mse <- mse(frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, frequency_data$prediction)
  # Root Mean Squared Error
  rmse <- rmse(frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, frequency_data$prediction)
  
  # Mean predictions for flagged and unflagged
  with_claim <- filter(frequency_data, TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS > 0)
  mean_prediction_claim <- mean(with_claim$prediction, na.rm=T)
  no_claim <- filter(frequency_data, TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS == 0)
  mean_prediction_noclaim <- mean(no_claim$prediction, na.rm=T)
  mean_difference <- mean_prediction_claim - mean_prediction_noclaim
  hist(with_claim$prediction)
  hist(no_claim$prediction)
  
  # Residuals
  frequency_data$residuals <- abs(frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS - frequency_data$prediction)
  residuals <- sum(frequency_data$residuals, na.rm=T)
  
  # Create mini row of rasults
  model_parameters <- data.frame(gini, mae, mse, rmse, mean_prediction_claim, mean_prediction_noclaim, mean_difference, residuals)
  
  rm(gini, mae, mse, rmse, mean_prediction_claim, mean_prediction_noclaim, mean_difference, with_claim, no_claim, residuals)
  
  # Source export factor strength scriptt
  source("/Volumes/GoogleDrive/Shared drives/Pricing & Underwriting/Data Science/R_Resources/Functions/export_factor_strengths_GBM.r")
  
  # Import base risk
  base_risk <- readRDS("/Volumes/GoogleDrive/Shared drives/Pricing & Underwriting/Data Science/UK/UK_risk_GBM/data/base_risk.rds")
  
  # Use Gary's script to get strength of factor when not split
  factor_strength <- export_factor_strengths(frequency_data, required_model, base_risk, frequency_data$EARNEDVEHICLEYEARS)
  
}

# Produce relativities for the model
if(calculate_realtivities == TRUE){
  
  # Get base risk values
  base_risk <- readRDS("/Volumes/GoogleDrive/Shared drives/Pricing & Underwriting/Data Science/UK/UK_risk_GBM/data/base_risk.rds")
  
  # Source script
  source("/Volumes/GoogleDrive/Shared drives/Pricing & Underwriting/Data Science/R_Resources/Functions/get_model_relativities_GBM.r")
  
  # Run function
  model_relativities <- get_model_relativities(frequency_data, required_model, base_risk)
  
}

# Transfer relativites and model parameters to an excel sheet
if(save_model_info == TRUE){
  
  # Get the AvsE script in
  source("/Volumes/GoogleDrive/Shared drives/Pricing & Underwriting/Data Science/R_Resources/Functions/write_gbm_relativities_to_excel.r")
  
  # Write the code
  write_glm_relativities_to_excel(model_relativities, factor_summary, factor_strength, model_parameter_excel_save_filename)
  
}

# Produce AvsE graphs for model factors
if(model_factors_avse_graphs == TRUE){
  
  # Get the AvsE script in
  source("/Volumes/GoogleDrive/Shared drives/Pricing & Underwriting/Data Science/R_Resources/Functions/AvsE.r")
  
  pdf(model_factors_save_location, width = 10, height = 8)
  
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$ACCIDENTMONTH, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$ACCIDENTQUARTER, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$TERMBANDED, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$COVERTYPE, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$DRIVERAGE, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$DRIVERAGEBANDED, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$ENGINESIZE, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$ENGINESIZEBANDED, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$LICENCEYEARS, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$LICENCEYEARSBANDED, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$LICENCEYEARS_BEFORETAXI, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$LICENCEYEARS_BEFORETAXIBANDED, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$NB_QTI, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$NB_QTI_BANDS, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$TAXILICENCEYEARS, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$TAXILICENCEYEARSBANDED, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$PREVIOUSNONFAULTCLAIMS, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$PREVIOUSFAULTCLAIMS, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$POINTS, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$LASTCALCULATEDDEMERITPOINTS, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$NOCLAIMSDISCOUNT, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = round(frequency_data$UBERAVERAGEMONTHLYTRIPS,-1), exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$UBERAVERAGEMONTHLYTRIPSBANDED, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$UBERPROTIER, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$UBERRATING, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$UBERRATINGBANDED, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$UBERNEWDRIVER, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$VEHICLEMAKEBANDED, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$REGION, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = round(frequency_data$OVERNIGHTLOCATIONAREAGROUP,-1), exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$USERCONFIRMED, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$VEHICLEAGE, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$VEHICLETYPE, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  
  dev.off()
  
}

# Produce AvsE graphs for all factors
if(all_factors_avse_graphs == TRUE){
  
  # Get the AvsE script in
  source("/Volumes/GoogleDrive/Shared drives/Pricing & Underwriting/Data Science/R_Resources/Functions/AvsE.r")
  
  pdf(all_factors_save_location, width = 10, height = 8)
  
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$ABTESTGROUP, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$ACCIDENTMONTH, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$ACCIDENTQUARTER, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$ALLOWMARKETING, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$CHANNEL, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$COVERTYPE, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$DATEOFBIRTHMATCH, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$DRIVERAGE, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$DRIVERAGEBANDED, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$ELECTORALROLLMATCH, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$ENGINESIZE, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$ENGINESIZEBANDED, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$FUELTYPE, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$GENDER, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  frequency_data$INSURANCEGROUPBANDS <- cut(frequency_data$INSURANCEGROUP, c(-10,-1,0,1,5,10,15,20,25,30,35,40,100))
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$INSURANCEGROUPBANDS, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$ISKIOSK, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$LASTCALCULATEDDEMERITPOINTS, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$LICENCEGROUP, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$LICENCEYEARS, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$LICENCEYEARSBANDED, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$LICENCEYEARS_BEFORETAXI, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$LICENCEYEARS_BEFORETAXIBANDED, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$LICENCINGAUTHORITY, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$MTAINDEX, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$NB_PTI, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$NB_QTI, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$NB_QTP, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$NCDUNKNOWN, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$NOCLAIMSDISCOUNT, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$OVERNIGHTLOCATIONAREAGROUP, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = round(frequency_data$OVERNIGHTLOCATIONAREAGROUP,-1), exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$PHONEOS_IOS, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$POINTS, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$PREVIOUSCLAIMS, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$PREVIOUSFAULTCLAIMS, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$PREVIOUSFIREANDTHEFTCLAIMS, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$PREVIOUSNONFAULTCLAIMS, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$PROTECTEDNOCLAIMSDISCOUNT, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$PURCHASEDAY, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$PURCHASEHOUR, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$QUOTEDAY, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$QUOTEHOUR, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$REGION, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$RENEWALINDEX, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$RENEWALINDEX_14_29, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$RENEWALINDEX_30, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$RENEWALINDEX_31_90, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$RENEWALINDEX_365, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$RENEWALINDEX_7_13, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$SEATINGCAPACITY, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$TAXILICENCEYEARS, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$TAXILICENCEYEARSBANDED, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$TERM, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$TERMBANDED, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$UBERAVERAGEMONTHLYTRIPSBANDED, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$UBERDRIVINGLESSTHANONEMONTH, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$UBEREXCLUSIVE, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$UBERNEWDRIVER, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$UBERONLYUBER, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$UBERPRO, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$UBERPROTIER, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$UBERRATING, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$UBERRATINGBANDED, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$UBERTRIPSBANDED, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$USERCONFIRMED, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$VEHICLEAGE, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$VEHICLECOSTNEWBANDED, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$VEHICLEINDICATIVEVALUEBANDED, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$VEHICLEMAKEBANDED, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$VEHICLETRANSMISSION, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$VEHICLETYPE, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$VEHICLEUSEDPRIVATEPARTYVALUEBANDED, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)
  AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$VEHICLEUSEDTRADEINVALUEBANDED, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)

  dev.off() 
  
}

# Asses time consistency
if(time_consistency == TRUE){
  
  # Create summary for each accident quarter
  time_consistency_summary <- frequency_data %>% group_by(ACCIDENTQUARTER) %>% summarise(policy_count = n(),
                                                                                         EVY = sum(EARNEDVEHICLEYEARS),
                                                                                         Claims = sum(TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS),
                                                                                         Predicted_claims = sum(prediction),
                                                                                         Claims_freq = Claims/EVY,
                                                                                         Predicted_claims_freq = Predicted_claims/EVY,
                                                                                         Mean_prediction = mean(prediction),
                                                                                         Median_prediction = median(prediction))
  
  
}

# Cross validation
if(cross_validation == TRUE){
  
  # Get full data and then 10 random samples
  seeds <- c(0, 123,456,789,135,246,357,468,579,680,836)
  sizes <- c(1, 0.5, 0.5, 0.4, 0.4, 0.3, 0.3, 0.2, 0.2, 0.1, 0.1)
  
  for(i in 1:length(seeds)){
    
    cv_individual <- sample_frac(frequency_data, size = sizes[i]) %>% summarise(Sample_size = sizes[i],      
                                                                                policy_count = n(),
                                                                                EVY = sum(EARNEDVEHICLEYEARS),
                                                                                Claims = sum(TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS),
                                                                                Predicted_claims = sum(prediction),
                                                                                Claims_freq = Claims/EVY,
                                                                                Predicted_claims_freq = Predicted_claims/EVY,
                                                                                Mean_prediction = mean(prediction),
                                                                                Median_prediction = median(prediction))
    
    # Combine
    if(i == 1){cross_validation_summary <- cv_individual}else{cross_validation_summary <- rbind(cross_validation_summary, cv_individual)}
  }
  
}

# Actual versus Predicted Deciles
if(predicted_deciles == TRUE){
  
  # Calculate prediction deciles
  frequency_data$prediction_deciles <- ntile(frequency_data$prediction, n=10)
  
  # Get full data and then 10 random samples
  seeds <- c(0, 123,456,789,135,246,357,468,579,680,836)
  sizes <- c(1, 0.5, 0.5, 0.4, 0.4, 0.3, 0.3, 0.2, 0.2, 0.1, 0.1)
  
  for(i in 1:length(seeds)){
    
    decile_individual <- sample_frac(frequency_data, size = sizes[i]) %>% group_by(prediction_deciles) %>% summarise(
                                                                                Claims_freq = sum(TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS)/sum(EARNEDVEHICLEYEARS))

    # Edit column names
    names(decile_individual) <- c("prediction_decile", paste0("Cfreq_", i))
    
    # Combine
    if(i == 1){decile_summary <- decile_individual}else{decile_summary <- left_join(decile_summary, decile_individual, by="prediction_decile")}
  }
  
}

# Lift chart info
if(lift_chart == TRUE){
  
  frequency_data$prediction_deciles <- ntile(frequency_data$prediction, 10)
  
  lift_chart_data <- frequency_data %>% group_by(prediction_deciles) %>% summarise(pols = n(),
                                                                                   claims = sum(TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS))
  
  
}
