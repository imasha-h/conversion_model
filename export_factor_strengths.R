# Code gets base strength of each factor within a model
# Particularly good if a factor is split into multiple fits

export_factor_strengths <- function(d, model, base_risk, exposure){
  # dat is the dataset over which to evaluate the factor strengths
  # model is the model you want to evaluate
  # base_risk is the "standard risk" that will be varied to evaluate the factor strengths
  
  # first get a list of all one way factors in the model
  factors_in_base_risk <- as.data.frame(all.vars(model$formula))
  
  # prepare a data table to hold the output
  factor_strength_output <- matrix(NA, nrow = nrow(factors_in_base_risk) - 1, ncol = 2)
  
  # start at 2 because first term is the response variable
  # function will fail if the formula is really strange, but should be fine for standard claims glms/retention models etc
  for (i in 2:nrow(factors_in_base_risk)){
    factor_name <- as.character(factors_in_base_risk[i,1])
    xvar <- d[[factor_name]]
    
    # get the exposure
    exposure_banded <- tapply(exposure,xvar,sum, default=0)
    
    # get the prediction for each level of the factor in question
    expanded_factor <- tidyr::expand(d, xvar)
    dummy_risks <- base_risk[rep(1,each=nrow(expanded_factor)),]
    dummy_risks[,factor_name] <- expanded_factor      
    
    # get the predictions relative to the base level
    predictions <- predict(model, newdata = dummy_risks, type = "response")
    
    # evaluate the factor strength and put it into array
    average_prediction <- sum(exposure_banded*predictions)/sum(exposure_banded)
    factor_strength <- (sum(exposure_banded*(predictions/average_prediction-1)^2) / sum(exposure)) ^ 0.5
    factor_strength_output[i-1,1] <- factor_strength
    
    span <- max(predictions)/min(predictions)
    factor_strength_output[i-1,2] <- span    
    
  }
  
  factor_strength_output <- as.data.table(cbind(as.data.table(factors_in_base_risk[2:nrow(factors_in_base_risk),]), factor_strength_output))
  
  # put in descending order
  factor_strength_output <- factor_strength_output[order(factor_strength_output[,2], decreasing = TRUE),]
  colnames(factor_strength_output)[1] <- 'factor'
  colnames(factor_strength_output)[2] <- 'strength'
  colnames(factor_strength_output)[3] <- 'span'
  
  
  export_factor_strengths <- factor_strength_output
  
}