# Function uses model, dataset and base risk values to create relativity tables

get_model_relativities <- function(dat, model, base_risk, collapse){
  
  # get a list of the factor in the base_risk and whether they are present in the model
  factors_in_base_risk <- as.data.frame(all.vars(model$formula))
  factors_in_base_risk[,1] <- as.character(factors_in_base_risk[,1])
  
  model_terms <- attr(terms(model), 'term.labels')
  model_terms_formulae <- paste('y ~', model_terms)
  
  variable_list <- data.frame('var1'=character(0), 'var2'=character (0))
  for (i in 1:length(model_terms_formulae)){
    variables <- all.vars(as.formula(model_terms_formulae[i]))
    if (length(variables)==2){
      # simple factor    
      variable_list <- rbind(variable_list, data.frame('var1'=variables[2],'var2'=''))
      
    } else if (length(variables)==3) {
      # two way interaction
      variable_list <- rbind(variable_list, data.frame('var1'=variables[2],'var2'=variables[3]))
    }
    
  }
  
  variable_list <- unique(variable_list)
  
  # create dataframes containing the factors and interactions present in the model
  factors <- variable_list[variable_list$var2=='',]
  interactions <- variable_list[variable_list$var2!='',]
  factors[,1] <- as.character(factors[,1])
  interactions[,1] <- as.character(interactions[,1])
  interactions[,2] <- as.character(interactions[,2])
  
  # create a list to contain the model tables, extra 1 for the base level
  number_of_tables <- 1 + nrow(factors) + nrow(interactions)
  table_list <- vector('list', number_of_tables)
  
  # get the base level for the model and put it into the first element of the list
  base_level <- predict(model, newdata = base_risk, type = "response")
  table_list[[1]] <- as.numeric(base_level)
  
  # create one way tables
  for (i in 1:(number_of_tables - nrow(interactions) - 1)){
    factor_name <- factors[i,1]
    expanded_factor <- tidyr::expand(dat, dat[[factor_name]])
    dummy_risks <- base_risk[rep(1,each=nrow(expanded_factor)),]
    dummy_risks[,factor_name] <- expanded_factor
    predictions <- predict(model, newdata = dummy_risks, type = "response") / base_level
    export_table <- cbind(expanded_factor, predictions)
    colnames(export_table)[1] <- factor_name
    colnames(export_table)[2] <- 'value'
    table_list[[i+1]] <- export_table
    
  }
  
  # create two way tables
  # note these tables do not strip out the one-way effects
  # but for a small number of interactions it is easy to achieve in Excel
  
  for (i in 1:nrow(interactions)){
    if (nrow(interactions)==0) next
    factor1_name <- interactions[i,1]
    factor2_name <- interactions[i,2]
    expanded_factor <- tidyr::expand(dat, dat[[factor1_name]], dat[[factor2_name]])
    dummy_risks <- base_risk[rep(1,each=nrow(expanded_factor)),]
    dummy_risks1 <- dummy_risks
    dummy_risks2 <- dummy_risks
    dummy_risks[,factor1_name] <- expanded_factor[,1]
    dummy_risks[,factor2_name] <- expanded_factor[,2]
    dummy_risks1[,factor1_name] <- expanded_factor[,1]
    dummy_risks2[,factor2_name] <- expanded_factor[,2]
    predictions <- predict(model, newdata = dummy_risks, type = "response")
    predictions1 <- predict(model, newdata = dummy_risks1, type = "response")
    predictions2 <- predict(model, newdata = dummy_risks2, type = "response")
    if (collapse){
      predictions <- (base_level * predictions) / (base_level * base_level)        
    } else {
      predictions <- (base_level * predictions) / (predictions1 * predictions2)        
    }
    export_table <- as.data.table(cbind(expanded_factor, predictions))
    colnames(export_table)[1] <- factor1_name
    colnames(export_table)[2] <- factor2_name
    colnames(export_table)[3] <- 'value'
    export_table <- dcast(export_table, export_table[[factor1_name]] ~ export_table[[factor2_name]], value.var = c('value'), fun=sum)
    colnames(export_table)[1] <- paste(factor1_name, '__X__', factor2_name, sep = '')    
    table_list[[1 + nrow(factors) + i]] <- export_table
  }
  
  return(table_list)
  
}
