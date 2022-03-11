# This script will build a LASSO GLM
# The intent is to reduce the number of factirs used in a statistically sound way
# First attempt will reduce us down to 15-20 risk factors

# Load packages
library(glmnet)
library(plyr)
library(dplyr)
library(dbplyr)
library(tidyr)
library(caret)
library(ModelMetrics)

# Load the data in
frequency_data <- readRDS("/Volumes/GoogleDrive/Shared drives/Pricing & Underwriting/Data Science/UK/UK_risk_GLM/base_data/frequency_data.rds")

# Create a matrix of the risk factors -  this is as a formula for GLMNET
risk_factors <- model.matrix(TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS ~ 
                               ACCIDENTQUARTER_2018_Q4 +
                               ACCIDENTQUARTER_2019_Q1 +
                               ACCIDENTQUARTER_2019_Q2 +
                               ACCIDENTQUARTER_2019_Q3 +
                               ACCIDENTQUARTER_2019_Q4 +
                               ACCIDENTQUARTER_2020_Q1 +
                               ACCIDENTQUARTER_2020_Q2 +
                               INCEPTIONMONTH +
                               NB_QTI +
                               TERM +
                               TERMBANDED_a7_13 + 
                               TERMBANDED_b14_29 +
                               TERMBANDED_d31_90 +
                               TERMBANDED_e365 +
                               COVERTYPE_Gold +
                               COVERTYPE_Silver +
                               RENEWALINDEX +
                               CHANNEL_Advertising +
                               CHANNEL_Flyering_Greenlight_Hub +
                               CHANNEL_Google +
                               CHANNEL_Other +
                               CHANNEL_Social_Media +
                               CHANNEL_Uber_Email +
                               CHANNEL_VIP +
                               DRIVERAGE +
                               LICENCEYEARS +
                               TAXILICENCEYEARS +
                               LICENCEYEARS_BEFORETAXI +
                               PREVIOUSCLAIMS +
                               PREVIOUSFAULTCLAIMS +
                               PREVIOUSNONFAULTCLAIMS +
                               PREVIOUSFIREANDTHEFTCLAIMS +
                               POINTS +
                               LASTCALCULATEDDEMERITPOINTS +
                               NOCLAIMSDISCOUNT +
                               NCDUNKNOWN +
                               PROTECTEDNOCLAIMSDISCOUNT +
                               UBERAVERAGEMONTHLYTRIPS +
                               UBERPRO +
                               UBERONLYUBER +
                               UBEREXCLUSIVE +
                               UBERPROTIER_Diamond +
                               UBERPROTIER_Gold +
                               UBERPROTIER_NonUber_Pro +
                               UBERPROTIER_Platinum +
                               UBERPROTIER_Unknown +
                               UBERRATING +
                               UBERTRIPS +
                               UBERTRIPSPASTYEAR +
                               UBERNEWDRIVER +
                               SEATINGCAPACITY +
                               VEHICLEMAKEBANDED_BMW +
                               VEHICLEMAKEBANDED_CITROEN +
                               VEHICLEMAKEBANDED_FORD +
                               VEHICLEMAKEBANDED_HONDA + 
                               VEHICLEMAKEBANDED_HYUNDAI +
                               VEHICLEMAKEBANDED_MERCEDESBENZ +
                               VEHICLEMAKEBANDED_OTHER +
                               VEHICLEMAKEBANDED_SKODA +
                               VEHICLEMAKEBANDED_VAUXHALL +
                               VEHICLEMAKEBANDED_VOLKSWAGEN +
                               VEHICLETYPE_COUPE_SPORT +
                               VEHICLETYPE_ESTATE +
                               VEHICLETYPE_HATCHBACK +
                               VEHICLETYPE_MPV +
                               VEHICLETYPE_SALOON +
                               INSURANCEGROUP +
                               VEHICLEAGE +
                               ENGINESIZE +
                               FUELTYPE_DIESEL +
                               FUELTYPE_ELECTRIC +
                               FUELTYPE_HYBRID_ELECTRIC +
                               FUELTYPE_OTHER +
                               FUELTYPE_PETROL +
                               VEHICLETRANSMISSION_AUTOMATIC +
                               VEHICLETRANSMISSION_MANUAL +
                               VEHICLETRANSMISSION_SEMIAUTO +
                               VEHICLETRANSMISSION_UNKNOWN +
                               VEHICLECOSTNEW +
                               VEHICLEINDICATIVEVALUE +
                               VEHICLEUSEDPRIVATEPARTYVALUE +
                               VEHICLEUSEDTRADEINVALUE +
                               REGION_EAST_ANGLIA +
                               REGION_EAST_MIDLANDS +
                               REGION_NE_ENG +
                               REGION_NW_ENG +
                               REGION_OTHER +
                               REGION_SCOTLAND +
                               REGION_SE_ENG +
                               REGION_SW_ENG +
                               REGION_UNKNOWN +
                               REGION_WALES +
                               REGION_WEST_MIDLANDS +
                               REGION_YORKS_HUMBS +
                               OVERNIGHTLOCATIONAREAGROUP +
                               PHONEOS_IOS +
                               USERCONFIRMED,
                             data = frequency_data)

# Target vafriable needs to be a factor for a binomial GLMNET model
target_variable <- frequency_data$TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS

# We can use glmnet to build multiple models and explore where factors come in
base_model <- glmnet(x = risk_factors,
                     y = target_variable,
                     alpha = 1,
                     family = c("poisson"),
                     standardize = TRUE,
                     lambda = 0.0004,
                     offset = c(frequency_data$EARNEDVEHICLEYEARS_LOG))

# Take a look at the coefficients
coef.glmnet(base_model)

# Now lets build the model using just the coefficients selected by the LASSO model
lasso_frequency_glm <- glm(TOTAL_CLAIMCOUNT_EXCLNILS_EXCLWS ~ 
                            ACCIDENTQUARTER_2018_Q4 +
                            ACCIDENTQUARTER_2019_Q1 +
                            ACCIDENTQUARTER_2019_Q2 +
                            ACCIDENTQUARTER_2019_Q3 +
                            ACCIDENTQUARTER_2019_Q4 +
                            ACCIDENTQUARTER_2020_Q1 +
                            ACCIDENTQUARTER_2020_Q2 +
                            
                            TERM +
                             COVERTYPE_Silver +
                             CHANNEL_Uber_Email +
                             DRIVERAGE +
                             LICENCEYEARS +
                             TAXILICENCEYEARS +
                             PREVIOUSCLAIMS +
                             PREVIOUSFAULTCLAIMS +
                             POINTS +
                             LASTCALCULATEDDEMERITPOINTS +
                             NOCLAIMSDISCOUNT +
                             UBERAVERAGEMONTHLYTRIPS +
                             UBERPROTIER_NonUber_Pro +
                             UBERRATING +
                             UBERTRIPS +
                             VEHICLEMAKEBANDED_HONDA +
                             REGION_NW_ENG +
                             REGION_SCOTLAND +
                             REGION_YORKS_HUMBS +
                             OVERNIGHTLOCATIONAREAGROUP +
                             USERCONFIRMED  +
                             
                            offset(EARNEDVEHICLEYEARS_LOG)
                          ,
                          data = frequency_data,
                          family = poisson(log))

# Save the model
saveRDS(lasso_frequency_glm, "/Volumes/GoogleDrive/Shared drives/Pricing & Underwriting/Data Science/UK/UK_risk_GLM/frequency_models/lasso_model/lasso_model.rds")








