# This script looks at the third iteration of the UK Risk GLM
# The factors used come from the LASSO model

# required packages
library(glmnet)
library(plyr)
library(dplyr)
library(dbplyr)
library(tidyr)
library(caret)
library(ModelMetrics)

# Get data
frequency_data <- readRDS("/Volumes/GoogleDrive/Shared drives/Pricing & Underwriting/Data Science/UK/UK_risk_GLM/base_data/frequency_data_18122020.rds")

# Here are a few additional edits to the data
frequency_data <- within(frequency_data, {
  
  # All non-zero are now 1
  LASTCALCULATEDDEMERITPOINTS <- ifelse(LASTCALCULATEDDEMERITPOINTS > 0, 1, 0)
  
  # All those over 3 are grouped into 6
  POINTS <- ifelse(POINTS > 3, 6, POINTS)
  
  # NB QTI is promising but needs banding
  NB_QTI_BANDS <- ifelse (NB_QTI > 0, 1, NB_QTI)
  
  # Different bands on Licence years
  LICENCEYEARSBANDED <- "hOther"
  LICENCEYEARSBANDED <- ifelse(LICENCEYEARS < 6, "aUnder6", LICENCEYEARSBANDED)
  LICENCEYEARSBANDED <- ifelse(LICENCEYEARS >= 6 & LICENCEYEARS <= 10, "b6_10", LICENCEYEARSBANDED)
  LICENCEYEARSBANDED <- ifelse(LICENCEYEARS >= 11 & LICENCEYEARS <= 15, "c11_15", LICENCEYEARSBANDED)
  LICENCEYEARSBANDED <- ifelse(LICENCEYEARS >= 16 & LICENCEYEARS <= 20, "d16_20", LICENCEYEARSBANDED)
  LICENCEYEARSBANDED <- ifelse(LICENCEYEARS >= 21 & LICENCEYEARS <= 25, "e20_25", LICENCEYEARSBANDED)
  LICENCEYEARSBANDED <- ifelse(LICENCEYEARS >= 26 & LICENCEYEARS <= 30, "f26_30", LICENCEYEARSBANDED)
  LICENCEYEARSBANDED <- ifelse(LICENCEYEARS > 30, "gOver30", LICENCEYEARSBANDED)
  
  # Combine 7+ seating capacity
  SEATINGCAPACITY <- ifelse(SEATINGCAPACITY > 7, 7, SEATINGCAPACITY)
  
  # Combine vehicle types MPV and Estate
  VEHICLETYPE_MPV_ESTATE <- ifelse(VEHICLETYPE %in% c("MPV", "ESTATE"), 1, 0)
  
  # Look at banding all under 30 day policies
  TERMBANDED <- ifelse(TERMBANDED %in% c("a7_13","b14_29"), "a01_29", TERMBANDED)
  
})

frequency_data <- select(frequency_data, -starts_with("TERMBANDED_"))

frequency_data <- fastDummies::dummy_cols(frequency_data, 
                                          select_columns = c("TERMBANDED"),
                                          remove_most_frequent_dummy = TRUE,
                                          remove_selected_columns = FALSE)

# The model building part
# Channel has been removed first hand as it mahy be difficult to build
pricing_glm <- glm(TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS ~ 
                             ACCIDENTQUARTER_2018_Q4 +
                             ACCIDENTQUARTER_2019_Q1 +
                             ACCIDENTQUARTER_2019_Q2 +
                             ACCIDENTQUARTER_2019_Q3 +
                             ACCIDENTQUARTER_2019_Q4 +
                             ACCIDENTQUARTER_2020_Q1 +
                             ACCIDENTQUARTER_2020_Q2 +
                             
                     # Need to see if banded fit is better than comntinuous fit
                     # Attempted grouping of under 30 day pols
                     TERMBANDED_a01_29 +
                     TERMBANDED_e365 +
                     
                     # Covertype is one of the most influentioal factors
                     # I am going to fit TWO of the covertypes (LASSO suggests only silver)
                     COVERTYPE_Silver + 

                     # I need to revisit this at the end
                     ifelse(DRIVERAGE < 27, 1, 0) +
                     ifelse(DRIVERAGE >= 28 & DRIVERAGE <= 35, 1, 0) + 
                     pmin(43, pmax(36, DRIVERAGE)) +
                     ifelse(DRIVERAGE >= 46 & DRIVERAGE <= 49, 1, 0) + 
                     pmin(71, pmax(56, DRIVERAGE)) +
                     
                     # Bulk of engines do not need fitting, but 2000+ is a significant fit, as is < 1500
                     pmin(2500, pmax(2000, ENGINESIZE)) +

                     # Refit from several multiple fits        
                     pmin(22, pmax(0, LICENCEYEARS)) +

                     # Refit to only fit before 11 yrs         
                     ifelse(TAXILICENCEYEARS == 0, 1, 0) +
                     ifelse(TAXILICENCEYEARS == 1, 1, 0) +

                     # Licence yrs before taxi looks unusual when not fit
                     ifelse(LICENCEYEARS_BEFORETAXI > 6 & LICENCEYEARS_BEFORETAXI <= 15, 1, 0) +
                     
                     # Unknow (-1) and 1+ fit already
                     pmin(1, pmax(0, NB_QTI_BANDS)) +
                     
                     #  Zero fits auto, need to fit 1 - 2 doesn't fit, and has limited data  
                     ifelse(PREVIOUSFAULTCLAIMS == 1, 1, 0) +
                     
                     # Zero works ok, over 1 needs fitting
                     pmin(2, pmax(0, PREVIOUSNONFAULTCLAIMS)) + 
                     ifelse(PREVIOUSNONFAULTCLAIMS == 3, 1, 0) +
                            
                     # Those over 3 have been grouped into a band 
                     POINTS +
                             
                     # I have banded all non-zero values into one value (1)
                     LASTCALCULATEDDEMERITPOINTS +
                             
                     # 0 is very different to 1-4
                     # Past 4 the data gets limited - we could band
                     ifelse(NOCLAIMSDISCOUNT > 0 & NOCLAIMSDISCOUNT < 5, 1, 0) + 

                     # Unknowns and 500+ are odd
                     pmin(500, pmax(0, UBERAVERAGEMONTHLYTRIPS)) +

                     # May need to add other tiers - check later        
                     UBERPROTIER_a_NonUber_Pro +
                     
                     # Need a fit between 4.87 - 4.99
                     # I will fit new drivers here to fit the 5 rating
                     pmax(4.87 ,pmin(4.94, UBERRATING)) + 
                     ifelse(UBERRATING >= 4.95 & UBERRATING < 4.99, 1, 0) + 
                     ifelse(UBERRATING == 4.99, 1, 0) + 

                     # Keep as this for now
                     VEHICLEMAKEBANDED_HONDA +
                     
                     # Fit relevant regions
                     REGION_NW_ENG + 
                     #REGION_YORKS_HUMBS + 
                             
                     # Tail ends are out
                     ifelse(OVERNIGHTLOCATIONAREAGROUP <= 35, 1, 0) + 
                     pmin(90, pmax(36, OVERNIGHTLOCATIONAREAGROUP)) +
                     ifelse(OVERNIGHTLOCATIONAREAGROUP >= 95, 1, 0) +

                     # Can stay as is
                     USERCONFIRMED  +
                     
                     # Majority of vechicle age fits well, but 0 not so much
                     ifelse(VEHICLEAGE == 0, 1, 0) +
                     
                     # Vehicle type has some fittable parts
                     VEHICLETYPE_MPV_ESTATE +
                     
                     pmax(16, pmin(19, LICENCEGROUP)) +
                             
                             offset(EARNEDVEHICLEYEARS_LOG)
                           ,
                           data = frequency_data,
                           family = poisson(log))

# Save the model
saveRDS(pricing_glm, "/Volumes/GoogleDrive/Shared drives/Pricing & Underwriting/Data Science/UK/UK_risk_GLM/frequency_models/iteration_v4/models/pricing_model_iteration_v4_final.rds")


#summary(pricing_glm)
frequency_data$prediction <- predict(object = pricing_glm, newdata = frequency_data, type = "response")
AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$LICENCEGROUP, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)

AvsE(yvar = frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS, xvar = frequency_data$TERMBANDED, exposure = frequency_data$EARNEDVEHICLEYEARS, fit = frequency_data$prediction)

summary(pricing_glm)

















