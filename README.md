# conversion_model
##load libraries
library(readr)
library(dplyr)
library(ggplot2)
library(mice)
#library(VIM)
#library(gbm)
library(glmnet)
library(fastDummies)
library(ModelMetrics)
library(caret)
library(pROC)

QUOTE_DATA2 <- read_csv("Conversion_model/final/QUOTE_DATA2.csv")

data <- QUOTE_DATA2 %>% 
  dplyr::select(-c(EXPIRYDATE, NB_QTI, NB_QTP, NB_PTI,INCEPTIONDATE, QUOTEDATE, PURCHASEDATE,
                   STATUS, POLICYNUMBER, 
                   VEHICLEMAKEBANDED, VERSIONNUMBER, 
                   LICENCENUMBER, LICENCETYPE, WEIGHTKG, POSTCODE, ADDRESS, SECTOR, CITY,
                   PREVIOUSCLAIMS, UBEREXCLUSIVE, CTYPE, COVERTYPE, VEHICLEMODEL, REFERRALCODE,
                   DOB, VEHICLEREGISTRATION, KEPTATDIFFERENTADDRESSOVERNIGHT,
                   ###after data analysis the following vars were removed
                   VEHICLEBODYCLASS, TRANSACTIONRETURNING, ROWNUMBERDUPLICATE, PREVIOUSFIREANDTHEFTCLAIMS,
                   POLICYCOUNT, INCEPTIONMONTH, CHANNEL, PURCHASEDAY, GWP, QTP,
                   LICENCEYEARS, LICENCEYEARSBANDED, ##REMOVE DUE TO MISSIG
                   ###remove PREMIUM THAT ARE NOT NEEDED FOR MODEL
                   COMPPREMIUM, COMPPLUSPREMIUM,
                   ###things I've removed for pricing meeting as too complex, look into them later
                   POINTS, LICENCINGAUTHORITY, LICENCEYEARS_BEFORETAXIBANDED,
                   LICENCENUMBER, DECLINEREASON, LICENCEYEARS_BEFORETAXI))

## Data cleaning 
data <- data %>% 
  filter(DUPLICATE == 0,
         DECLINE == 0,
         TRANSACTION == 'NB') %>% 
  select(-c(DUPLICATE, DECLINE, TRANSACTION
  ,GENDER, QUOTEDAY)) %>%  #these were predictive but cant keep (weds & male predictive)
  ##INSURANCENUMBER CHANGE DEFAULT TO -1
  mutate(INSURANCEGROUP = ifelse(INSURANCEGROUP == "Default", -1, INSURANCEGROUP),
         EXPOSURE = 1)

## Imputations of missing values
#Predictive mean matching (PMM)
#Compared to other imputation methods, it usually imputes less implausible values (e.g. negative incomes) 
#and takes heteroscedastic data into account more appropriately.
#https://statisticsglobe.com/predictive-mean-matching-imputation-method/
completeddata <- mice(data, m=5, maxit = 50, method = 'pmm', seed = 500)
data <- complete(completeddata,2)
#colSums(is.na(data))



data <-data %>% 
  mutate(

         OVERNIGHTLOCATIONAREAGROUPBANDED=cut(OVERNIGHTLOCATIONAREAGROUP,breaks=c(19,30,40,50,60,70,80,90,100)),
         UBERPROTIER = ifelse(UBERPROTIER == 'Unknown', 'Open_Market', UBERPROTIER),
         UBERRATINGSPLIT = ifelse(UBERRATING == 5 & UBERNEWDRIVER == 1, 0, UBERRATING),
         VEHICLEYEAROFMANUFACTURE2022 = 
           ifelse(VEHICLEYEAROFMANUFACTURE != '', 
                  paste0(VEHICLEYEAROFMANUFACTURE, "(",
                         2022 - as.numeric(VEHICLEYEAROFMANUFACTURE), "YRS)"), ""),
         VEHICLEAGENEW = ifelse(VEHICLEAGE>=0 & VEHICLEAGE<=2,'0-2', as.character(VEHICLEAGE))
         
         ) %>% 
  group_by(TERM) %>% 
  mutate(        TPOPREMIUMANNUAL= ifelse(TERM==30,TPOPREMIUM*12,TPOPREMIUM),
                 TPOPREMIUMZSCORE=(TPOPREMIUMANNUAL - mean(TPOPREMIUMANNUAL)) / sd(TPOPREMIUMANNUAL),
                 TPOPREMIUMZSCOREBANDEDMORE=cut(TPOPREMIUMZSCORE, breaks=c(-2.5, -2, -1.5, -1, -0.5,0,
                                                                           0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4,
                                                                           4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9 )),
                 TPOPREMIUMZSCOREBANDED=cut(TPOPREMIUMZSCORE, breaks=c(-3, -2, -1, 0
                                                                       , 1, 2, 3, 4
                                                                       , 5, 6, 7, 8, 9 ))) %>% 
  select(-TPOPREMIUMANNUAL) %>% 
  ungroup(TERM)




##plot for tpo distribution
# ggplot(data ,aes(x = TPOPREMIUMZSCORE, fill = as.factor(TERM), line = as.factor(TERM)
# ))  +
#   # geom_area(aes(y = ..count..), stat = "bin", binwidth = 1, alpha = 0.6
#   geom_area(aes(y = ..count..), stat = "bin", binwidth = 1, alpha = 0.6#tocheckpriortostadarsiatio
#             ,size=.5, colour="white"
#   )    +
#   theme_bw() +
#   theme(legend.position = "bottom",
#         #legend.title = element_blank(),
#         legend.background = element_rect(linetype = "solid", color = "black", size = 0.2),
#         panel.border = element_rect(colour = "black", fill=NA, size=0.2),
#         panel.grid.major.y = element_blank())
####TPOPREMIUM
#Annualized the monthly premiums by multiplying tpopremiums where term = 30 by 12. 
#implemented z score standardization to normalize the premiums this was done by subtracting 
#the mean value of the variable and then dividing by the standard deviation of the variable

###OVERNIGHTLOCATIONAREAGROUP
# banded by 10


##Dummy variables for categorical variables
formatted_data <- dummy_cols(data,select_columns = c(
  "ENGINESIZEBANDED",
  "DRIVERAGEBANDED",
  "UBERRATINGBANDED",
  "VEHICLEMAKE",
  "VEHICLETYPERAW", 
  "VEHICLETYPE", 
  "INSURANCEGROUP",
  "REGION",
  "DRIVERAGEBANDED",
  "UBERNEWDRIVER",
  "UBERRATINGSPLIT ",
  "UBERPROTIER",
  "OVERNIGHTLOCATIONGROUP",
  "FUELTYPE",
  "OVERNIGHTLOCATIONAREAGROUPBANDED",
  "TERM"),
  remove_most_frequent_dummy = FALSE,
  remove_selected_columns = FALSE)
#ls(formatted_data)
data <- formatted_data



##Lasso Model
#risk_factors <- model.matrix(SALE ~ ., data = data)
risk_factors <- model.matrix(SALE ~
                               +DRIVERAGEBANDED
                             +DRIVERAGEBANDED_fOver60
                             +ENGINESIZE
                             +ENGINESIZEBANDED_c1801_2000
                             +FUELTYPE                      
                             +LICENCEGROUP
                             +NOCLAIMSDISCOUNT
                             +OVERNIGHTLOCATIONAREAGROUPBANDED
                             +PREVIOUSFAULTCLAIMS           +PREVIOUSNONFAULTCLAIMS
                             +REGION
                             +SEATINGCAPACITY               +TAXILICENCEYEARS
                             +TAXILICENCEYEARSBANDED        
                             +TERM
                             +OVERNIGHTLOCATIONAREAGROUP
                             +OVERNIGHTLOCATIONAREAGROUPBANDED
                             +TPOPREMIUMZSCORE
                             #+TPOPREMIUMANNUAL
                             +TPOPREMIUM
                             +TPOPREMIUMZSCOREBANDED
                             +UBERAVERAGEMONTHLYTRIPSBANDED
                             +UBERNEWDRIVER                 +UBERPROTIER
                             +UBERRATINGSPLIT 
                             +UBERRATING                    +UBERRATINGBANDED
                             +VEHICLETYPERAW_OTHER
                             ,data = data)

# Target variable needs to be a factor for a binomial GLMNET model
target_variable <- data$SALE

###my code#####
cv_model <- cv.glmnet(risk_factors, target_variable, alpha = 1)
#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min

best_lambda 
#plot(cv_model)  

## LASSO MODEL
#find coefficients of best model
best_model <- glmnet(risk_factors, target_variable, alpha = 1, lambda = 0.009)
coef(best_model)
###order coefficients
coeftab <-coef(best_model)
lastvals <- coeftab[,ncol(coeftab)]
coeftab_s <- coeftab[order(abs(lastvals),decreasing=TRUE),]
coeftab_s
## plot, leaving out the intercept
#matplot(t(coeftab_s)[,-1],type="l")


###########################################################
# AvsE 
### Original AvsE function was edited to remove print() to prevent duplicate plots, colours and labels also editted
AvsE <-function(yvar,xvar,exposure,fit,title=NULL,xlabels=NULL,errors=TRUE){
  
  # actual vs expected chart cut by rating factor
  # print(deparse(substitute(xvar)))
  
  # add a tiny bit of exposure to each level to stop divide by zero errors
  exposure_banded <- tapply(exposure,xvar,sum, default=0) + 0.000000001
  actuals_banded <- tapply(yvar,xvar,sum)/exposure_banded
  fitted_banded <- tapply(fit,xvar,sum)/exposure_banded
  
  levels <- names(exposure_banded)
  
  data <- data.frame(levels,exposure_banded,actuals_banded,fitted_banded)
  data['reordered_levels'] <- factor(data$levels, as.character(data$levels))
  
  # remove levels with less than 0.1% of total exposure
  data <- data[exposure_banded>sum(exposure_banded)*0.001,]
  
  actual_max <- max(data$actuals_banded)
  fitted_max <- max(data$fitted_banded)
  exposure_max <- max(data$exposure_banded)
  
  if(fitted_max<1){
    # this is most likely a frequency model
    error_bar <- 1.96 * (data$fitted_banded/data$exposure_banded)^0.5 # works for Poisson only
    sec_axis_text <- 'earned vehicle years'
  } else {
    # this is most likely a severity model
    error_bar <- 1.96 * data$exposure_banded^-0.5 * data$fitted_banded # works for Gamma only    
    sec_axis_text <- 'number of non-nil claims'
  }
  
  scalar = exposure_max/fitted_max*3
  data['exposure_banded'] <- data['exposure_banded']/scalar
  
  p <- ggplot(data, aes(x=reordered_levels, group=1))
  
  #set x axis labels
  # if (is.null(xlab)){
  # p <- p + scale_x_discrete(levels,name=deparse(substitute(xvar)))
  # } else {
  #  p <- p + scale_x_discrete(levels,name=xlabels)
  # }
  
  if(errors){p <- p + geom_errorbar(ymin=data$fitted_banded-error_bar, ymax=  data$fitted_banded+error_bar, size=0.3,width=0.05, color='grey50')} # new
  p <- p + geom_col(aes(y=exposure_banded, fill='grey50')) 
  p <- p + geom_line(aes(y=actuals_banded,color='red')) + geom_point(aes(y=actuals_banded,color='red'))
  p <- p + geom_line(aes(y=fitted_banded,color='blue')) + geom_point(aes(y=fitted_banded,color='blue'))
  #p <- p + scale_y_continuous(sec.axis = sec_axis(~.*scalar, name = sec_axis_text)) 
  p <- p + scale_color_manual(name = NULL, values = c('dodgerblue2','brown1'), labels=c('Model prediction','Actual'))
  p <- p + scale_fill_manual(name = NULL, values = c("cadetblue2"), label = c(cname))
  
  # set y axis label
  p <- p + labs(y='')
  
  # set title
  if (is.null(title)){
    p <- p + ggtitle(paste('Actual vs fitted -',deparse(substitute(xvar))))
  } else {
    p <- p + ggtitle(paste(title,'-',xlabels))
  }
  
  #p <- p + theme(plot.title = element_text(size=22), axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"))
  #p <- p + theme(plot.title = element_text(face='bold'), axis.text.x=element_text(size=10), legend.text=element_text(size=14))
  
  p <- p + theme_bw()
  
  p <- p + theme(legend.position = "bottom",
                 #legend.title = element_blank(),
                 legend.background = element_rect(linetype = "solid", color = "black", size = 0.2),
                 panel.border = element_rect(colour = "black", fill=NA, size=0.2),
                 panel.grid.major.y = element_blank(),
                 axis.text.x = element_text(angle = 45, hjust=1))
  p <- p + scale_x_discrete(name =cname)
  
  #print(p)
  
}



#### old model before feedback

### new model - main change - premium standardised better
glm <- glm(SALE ~ 
             #  +(TPOPREMIUMZSCORE) 
             + poly((pmin(6, pmax(-1, TPOPREMIUMZSCORE))), 2)
           #   + poly(TPOPREMIUMZSCORE, 2)
           
           +(pmin(13, pmax(10, VEHICLEAGE)))
           +ifelse(VEHICLEAGENEW == '0-2', 1, 0) 
           + poly((pmin(22, pmax(3, QUOTEHOUR))), 2)
           
           
           +TERM
           + poly((pmin(58, pmax(23, DRIVERAGE))), 2)
           +pmin(2000, pmax(1800, ENGINESIZE))
           +poly(pmin(18, pmax(10, LICENCEGROUP)))
           #   +FUELTYPE_HYBRID_ELECTRIC
           #   +ifelse(OVERNIGHTLOCATIONAREAGROUP >= 70 & OVERNIGHTLOCATIONAREAGROUP<=80 , 1, 0)
           + ifelse(PREVIOUSFAULTCLAIMS == 2 , 1, 0) 
           +(pmin(5, pmax(1, NOCLAIMSDISCOUNT)) )
           +(pmin(7, pmax(0, QTI)))#ORIGINAL
           +REGION_YORKS_HUMBS
           #  # +REGION_NE_ENG
           +REGION_WALES 
           +pmin(500, pmax(350, UBERAVERAGEMONTHLYTRIPS))
           #+UBERNEWDRIVER_0
           # # +UBERRATINGSPLIT
           +ifelse(UBERRATINGSPLIT >= 4.94 & UBERRATINGSPLIT <= 5, 1, 0)
 
           +UBERPROTIER_Open_Market 
           +UBERPROTIER_NonUber_Pro
           +TAXILICENCEYEARS #better
           +VEHICLETYPERAW_COUPE
           +VEHICLETYPERAW_THREE_DOOR_HATCHBACK
           ,family = binomial
           #,family=poisson(link="log")
           ,data = data)
summary(glm)

#PREDICTION 
data$prediction <- predict(object = glm, newdata = data, type = "response")
#OVERALL PREDICTION FOR OVERALL AVSE PLOT
data$OVERALL <- (round(data$prediction/data$EXPOSURE,1))



##re-order uber protier and create new var UBERPROTIER_ORDER
data$UBERPROTIER_ORDER<- factor(data$UBERPROTIER, levels=c(  
  'NonUber_Pro',
  'Blue',
  'Gold',
  'Platinum',
  'Diamond',
  'Open_Market'
)) 
#levels(data$UBERPROTIER_ORDER)
data$VEHICLEAGENEW<- factor(data$VEHICLEAGENEW, levels=c(  
  '0-2',  '1',  '2',  '3',  '4',  '5',  '6',  '7',  '8',  '9',  '10',  '11',
  '12',  '13'
)) 


#write.csv(data, file="data.csv")
## Extract columns to be analyzed
columndata <- data %>%
  dplyr::select(
    "OVERALL"
    ,"DRIVERAGE"                                
    ,"DRIVERAGEBANDED"                          
    ,"ENGINESIZE"
    ,"ENGINESIZEBANDED"
    ,"FUELTYPE"                                 
    ,"INSURANCEGROUP"  
    ,"LICENCEGROUP" 
    ,"NOCLAIMSDISCOUNT"                         
    ,"OVERNIGHTLOCATIONAREAGROUP"
    ,"OVERNIGHTLOCATIONAREAGROUPBANDED"
    ,"PREVIOUSFAULTCLAIMS"                      
    ,"PREVIOUSNONFAULTCLAIMS"                   
    ,"PRODUCT"                                  
    ,"PTI"                                      
    ,"PURCHASEHOUR"                             
    ,"QTI"                                      
    # ,"QUOTECOUNT"                               
    ,"QUOTEHOUR"                                
    ,"REGION"                                   
    #,"SALE"                                     
    ,"SEATINGCAPACITY"                          
    ,"TAXILICENCEYEARS"                         
    ,"TAXILICENCEYEARSBANDED"                   
    ,"TERM"                                     
    ,"TERMBANDED"                               
    ,"TPOPREMIUMZSCORE"
    ,"TPOPREMIUMZSCOREBANDED"
    ,"TPOPREMIUMZSCOREBANDEDMORE"
    #,"UBERAVERAGEMONTHLYTRIPS"                  
    ,"UBERAVERAGEMONTHLYTRIPSBANDED"            
    ,"UBERNEWDRIVER"
    #,"UBERPROTIER"  
    ,"UBERPROTIER_ORDER"
    ,"UBERRATING"
    ,"UBERRATINGSPLIT"
    #,"UBERTRIPS"                                
    ,"UBERTRIPSBANDED"                          
    ,"VEHICLEAGE"                               
    ,"VEHICLEAGEBANDED"      
    ,"VEHICLEAGENEW"                         
    ,"VEHICLECOSTNEW"                           
    ,"VEHICLECOSTNEWBANDED"                     
    ,"VEHICLEINDICATIVEVALUE"                   
    ,"VEHICLEINDICATIVEVALUEBANDED"             
    ,"VEHICLEMAKE"                              
    ,"VEHICLEOWNERSHIP"                         
    ,"VEHICLETRANSMISSION" 
    # ,"VEHICLETYPE"                              
    ,"VEHICLETYPERAW"                           
    ,"VEHICLEYEAROFMANUFACTURE2022" 
    # ,"ENGINESIZEBANDED_c1801_2000"
    # ,"ENGINESIZEBANDED_b1501_1800"
    #  ,"FUELTYPE_HYBRID_ELECTRIC"
    # ,"DRIVERAGEBANDED_d41_50"
    # ,"DRIVERAGEBANDED_c31_40"
    
  )



#add quotes to list of names
#columndata<-as.character(quote(data))[-1]))


# Make list of variable names to loop over.
var_list = combn(names(columndata)[1:length(columndata)], 2, simplify=FALSE)

# Make plots
plot_list = list()
for (i in 1:length(columndata)) {
  
  cname <- colnames(columndata[c(i)])
  p <- AvsE(yvar = data$SALE, xvar = columndata[c(i)], exposure = data$EXPOSURE, fit = data$prediction) + ggtitle(paste('Actual vs fitted -',cname) )
  print(p)
  plot_list[[i]] = p
}

# create pdf where each page is a separate plot.
pdf("updated_plots.pdf")

for (i in 1:length(columndata)) {
  print(plot_list[[i]])
}

dev.off()

summary(glm)


#### statistical analysis

#### GINI ETC
library(reldist)
gini <- reldist::gini(data$SALE, data$prediction)
gini #41%
# Mean Absolute Error
mae <- Metrics::mae(data$SALE, data$prediction)  #28% good not excellent MAE tells us how big of an error we can expect from the forecast on average.(lower score is better)
mae
# Mean Squared Error
mse <- Metrics::mse(data$SALE, data$prediction) #14 % The mean squared error (MSE) tells you how close a regression line is to a set of points.(lower score is better)
mse
# Root Mean Squared Error
rmse <- Metrics::rmse(data$SALE, data$prediction) #37% Root Mean Square Error (RMSE) is the standard deviation of the residuals (prediction errors). Residuals are a measure of how far from the regression line data points are; RMSE is a measure of how spread out these residuals are. In other words, it tells you how concentrated the data is around the line of best fit.
rmse
# (lower score is better)

# Mean predictions for flagged and unflagged
with_sale <- filter(data, SALE > 0)
mean_prediction_sale <- mean(with_sale$prediction, na.rm=T)
no_sale <- filter(data, SALE == 0)
mean_prediction_nosale <- mean(no_sale$prediction, na.rm=T)
mean_difference <- mean_prediction_sale - mean_prediction_nosale
hist(with_claim$prediction)
hist(no_claim$prediction)

# Residuals
data$residuals <- abs(data$SALE - data$prediction)
residuals <- sum(data$residuals, na.rm=T)

# Create mini row of rasults
model_parameters <- data.frame(gini, mae, mse, rmse, mean_prediction_sale, mean_prediction_nosale, mean_difference, residuals)
writexl::write_xlsx(model_parameters,"model_parameters.xlsx")

# Akaike information criterion (AIC)
# AIC: 9451

### VARIABLE IMPORTANCE
required_model<- glm
var_importance <- as.data.frame(caret::varImp(required_model, scale = FALSE))

var_importance <- data.frame(
                  Factors   = rownames(var_importance),
                  Importance = var_importance$Overall)
var_importance<-var_importance[order(var_importance$overall,decreasing = T),]

# p values
p_values <- as.data.frame(coef(summary(required_model))[,4])
p_values <- tibble::rownames_to_column(p_values, "Factors")
names(p_values) <- c("Factors", "P-values")

# Coefficients
coefficients <- as.data.frame(required_model$coefficients)
coefficients <- tibble::rownames_to_column(coefficients, "Factors")
names(coefficients) <- c("Factors", "Coefficients")

# Combine the above
factor_summary1 <- left_join(var_importance, p_values, coefficients, by = "Factors")
factor_summary <- left_join(factor_summary1, coefficients, by = "Factors")

writexl::write_xlsx(factor_summary,"factor_summary.xlsx")



##### factor and relativities 

#create base_risk
#tail(names(sort(table(data$VEHICLEYEAROFMANUFACTURE))), 1)

base_risk<- data %>% 
  filter(QTI == 7,
         VEHICLEAGEBANDED == "b6_10",
         ENGINESIZEBANDED == "b1501_1800",
         FUELTYPE == "HYBRID_ELECTRIC",
         DRIVERAGEBANDED == "c31_40",
         QUOTEHOUR == 12,
         VEHICLETYPERAW == "FIVE_DOOR_HATCHBACK",
         VEHICLECOSTNEWBANDED == 20000,
         VEHICLEYEAROFMANUFACTURE == 2015,
         REGION == 'LONDON'
  )
###factor strength
factor_strength<-export_factor_strengths(data, glm, base_risk, data$EXPOSURE)
writexl::write_xlsx(factor_strength,"Factor_Strength.xlsx")

# This function takes relativites and writes them to an Excel sheet
model_relativities<-  get_model_relativities(data, glm, base_risk)
factor_coefficients <- factor_summary
factor_strength <- factor_strength
filename  <- (paste0("relativities_file.xlsx"))
write_glm_relativities_to_excel(model_relativities, factor_summary, factor_strength, filename)


#### histogram plot 
### CONFUSION MATRIX AND ROC 

  hist(data$prediction, col = c("cadetblue2"))
  
  ggplot(data, aes(prediction)) + 
    geom_histogram(fill="cadetblue2")#+ 
    #stat_bin(bins = 40)+
    scale_fill_manual(values = c("cadetblue2"))

# let's predict the same data: use type response to have probability as resulthere you decide the cutoff and put as factor, in one line

#data$prediction <- predict(object = glm, newdata = data, type = "response")
    
#data$pred <- as.factor(ifelse(predict(glm, data, type="response")>0.34,"1","0"))
data$pred <- as.factor(ifelse(data$prediction>0.33,"1","0"))
conversionrate <-sum(data$SALE)/nrow(data)
#0.33 


cm<-confusionMatrix(data$pred, as.factor(data$SALE), positive = "1")
cm
as.table(cm)

#0.33 is cutoff value of prediction
#
#The Threshold or Cut-off represents in a binary classification the probability that the prediction is true. It represents the tradeoff between false positives and false negatives.
data$prediction1 <- predict(object = glm, newdata = data, type = "response")
#### ROC



### prediction as categorical 
# g <- pROC::roc(SALE ~ as.numeric(pred), data = data)
# plot(g) 
# auc(g)

### prediction as continuous
roc_obj <- roc(data$SALE, data$prediction) ##the same as above
plot(roc_obj, legacy.axes = TRUE)
auc(roc_obj)


#####
# The Area Under the Curve (AUC) is the measure of the ability of a classifier to distinguish 
# between classes and is used as a summary of the ROC curve. 
# The higher the AUC, the better the performance of the model at distinguishing between 
# the positive and negative classes
# considered good/excellent for AUC values between 0.8-0.9

## Area under the curve: 0.825



##### RESIDUALS

###Binned residual plot to assess the overall fit of the model
arm::binnedplot(fitted(glm), 
                residuals(glm, type = "response"), 
                nclass = NULL, 
                xlab = "Expected Values", 
                ylab = "Average residual", 
                main = "Binned residual plot", 
                cex.pts = 0.8, 
                col.pts = 1, 
                col.int = "gray")

#plot shows no patterns - 
# https://journals.sagepub.com/doi/pdf/10.1177/1536867X1501500219
# The grey lines represent  ±  2 SE bands, which we would expect to contain about 95% of the observations. This model looks reasonable, in that majority of the fitted values seem to fall within the SE bands.
# Only one of the binned residuals is substantially outside of the +/- 2 SE bounds, which is roughly inline with the expectation of 95% of binned residuals falling in the +/- 2 SE range.


#### CROSS VALIDATION: evaluating estimator performance
library(caret)
set.seed(2)

# https://machinelearningmastery.com/how-to-estimate-model-accuracy-in-r-using-the-caret-package/
# Repeated k-fold Cross Validation
# The process of splitting the data into k-folds can be repeated a number of times, 
# this is called Repeated k-fold Cross Validation. 
# The final model accuracy is taken as the mean from the number of repeats.
# Below I've used 5-fold cross validation with 3 repeats to estimate GLM on the dataset.

# define training control
fitCtrl = trainControl(method="repeatedcv", number=5, repeats=3)

# train the model
modelGLM = train(as.factor(SALE)~., 
                 data=data, 
                 method="glm", 
                 family='binomial', 
                 trControl=fitCtrl, 
                 metric="Accuracy",
                 na.action = na.pass)
# summarize results
print(modelGLM)
modelGLM$pred
# 11013 samples
# 190 predictor
# 2 classes: '0', '1' 
# No pre-processing
# Resampling: Cross-Validated (5 fold, repeated 3 times) 
# Summary of sample sizes: 8810, 8811, 8810, 8810, 8811, 8811, ... 
# Resampling results:
#   
#   Accuracy   Kappa    
# 0.9999092  0.9997952

confusionMatrix(modelGLM, positive = '1')


###export all info coefficents, p value, SE
# library(gtsummary)
# results<-as.data.frame(summary(glm)$coefficients)
# writexl::write_xlsx(results, "Results.xlsx")
# 
# results<-summary(glm)$coefficients
# tbl_regression(glm, exponentiate = F)%>%
#   add_global_p() %>%  # add global p-value 
#   #add_nevent() %>%    # add number of events of the outcome
#   add_q() %>%         # adjusts global p-values for multiple testing
#   bold_p() %>%        # bold p-values under a given threshold (default 0.05)
#   bold_p(t = 0.01, q = TRUE) %>% # now bold q-values under the threshold of 0.10
#   bold_labels()






