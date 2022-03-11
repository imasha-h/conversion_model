# AVSE FUNCTION
# Plots A vs E for given input

### for my one
##exposure - how many rows - (number of quotes)
## xvar - whatever factor
## yvar - SALE

data$prediction <- predict(object = base_frequency_glm, newdata = data, type = "response")
data$predictiongbm <- predict(object = gbm1, newdata = data, type = "response")
##GLM
AvsE(yvar = as.numeric(data$SALE), xvar = data$PRODUCT, exposure = data$EXPOSURE, fit = data$prediction)  

##GBM
AvsE(yvar = as.numeric(data$SALE), xvar = data$VEHICLECOSTNEW, exposure = data$EXPOSURE, fit = data$predictiongbm, errors=TRUE)  

#keep SALE as numeric (1,0)
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
  
  # set x axis labels
  #if (is.null(xlab)){
  p <- p + scale_x_discrete(levels,name=deparse(substitute(xvar)))
  #} else {
  #  p <- p + scale_x_discrete(levels,name=xlabels)    
  #}
  
  if(errors){p <- p + geom_errorbar(ymin=data$fitted_banded-error_bar, ymax=  data$fitted_banded+error_bar, size=0.3,width=0.05, color='grey50')} # new
  p <- p + geom_col(aes(y=exposure_banded, fill='grey50')) 
  p <- p + geom_line(aes(y=actuals_banded,color='red')) + geom_point(aes(y=actuals_banded,color='red'))
  p <- p + geom_line(aes(y=fitted_banded,color='blue')) + geom_point(aes(y=fitted_banded,color='blue'))
  p <- p + scale_y_continuous(sec.axis = sec_axis(~.*scalar, name = sec_axis_text))
  p <- p + scale_color_manual(name = NULL, values = c('blue','red'), labels=c('model prediction','actual'))
  p <- p + scale_fill_manual(name = NULL, values = c('grey50'), label = c(sec_axis_text))
  
  # set y axis label
  p <- p + labs(y='')
  
  # set title
  if (is.null(title)){
    p <- p + ggtitle(paste('Actual vs fitted -',deparse(substitute(xvar))))
  } else {
    p <- p + ggtitle(paste(title,'-',xlabels))
  }
  
  p <- p + theme(plot.title = element_text(size=22), axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"))
  p <- p + theme(plot.title = element_text(face='bold'), axis.text.x=element_text(size=10), legend.text=element_text(size=14))
  
  #print(p)
  
}
