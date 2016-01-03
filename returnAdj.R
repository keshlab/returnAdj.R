# returnAdj returns a data.frame with values adjusted for designated covariates. It takes
# the following parameters:
# - data: The overarching data.frame
# - measnames: A vector holding the names of the measures of interest
# - covars: A vector holding the names of the covariates to be used (but not in an interaction term)
# - interacts: A vector holding the names of covariates to be used in an interaction term
# - display: A boolean indicating that the original-adjusted value correlations
#       are to be printed. This defaults to true.
# - method: A string identifying the mean adjusting methods. This defaults to 'smean'. The other options include 'iadj' and 'null'.
#           This feature provides you with the option to fetch sample means adjusted based on: Intercept adjusted (iadj), sample mean (smean) or 
#           'null' adjusted.
#
#
# Unfortunately, the way that returnAdj presently works, FACTORS CANNOT BE NUMERIC. Sorry
# for the inconvenience.

returnAdj <- function(data, measnames, covars=c(), interacts=c(), display=F, method='smean') {
  
  # "Sanity checks"
  if(length(c(covars,interacts))==0) {
    print('Please retry with covariates/interaction-terms included in your
          returnAdj function.')
    break
  }
  
  interStr <- ''
  if(length(interacts)>0) {
    interStr <- interacts[1]
    for(interact in interacts) {
      if(is.numeric(data[,interact])) data[interact] <- scale(data[interact])
      if(interact != interacts[1]) interStr <- paste(interStr,'*',interact)
    }
  }
  if(interStr!='') interStr <- paste0(interStr,'+')
  
  covarStr <- covars[1]
  for(covar in covars) {
    if(is.numeric(data[,covar])) data[covar] <- scale(data[covar])  
    if(covar != covars[1]) covarStr <- paste(covarStr,'+',covar)  
  }
  
  data.new <- data.frame(matrix(nrow=nrow(data), ncol=length(measnames)))   
  col_counter <- 1
  
  for(meas in measnames) {
    mod <- lm(paste(meas,'~',interStr,covarStr),data=data)
    names(data.new)[col_counter] <- meas
    if(method=='iadj') data.new[, col_counter] <- mod$residuals+mod$coefficients[1]
    if(method=='smean') data.new[,col_counter] <- mod$residuals+mean(data[,meas])
    if(method=='null') data.new[,col_counter] <- mod$residuals
    if(display) print(paste0(paste(meas,'~',interStr,covarStr),': Orig-Adj r=',signif(cor(cbind(data[meas],data.new[meas]))[1,2],digits=4)))
    col_counter = col_counter + 1
  }
  
  
  return(data.new)
}
