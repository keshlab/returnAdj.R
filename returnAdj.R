# returnAdj returns a data.frame with values adjusted for designated covariates. It takes
# the following parameters:
# - data: The overarching data.frame
# - measure: A string for the name of the vector to be adjusted
# - covars: A vector holding the names of the covariates to be used (but not in an interaction term)
# - interacts: A vector holding the names of covariates to be used in an interaction term
# - display: A boolean indicating that the original-adjusted value correlations
#       are to be printed. This defaults to true.
# - method: A string identifying the mean adjusting methods. This defaults to 'smean'. The other options include 'iadj' and 'null'.
#           This feature provides you with the option to fetch sample means adjusted based on: Intercept adjusted (iadj), sample mean (smean) or 
#           'null' adjusted.
# - subset: A conditional expression that returns a vector of booleans the same length as the vector to be adjusted. This can be used to generate the model on only a subset of the data (e.g. Controls only)
#
#
# Unfortunately, the way that returnAdj presently works, FACTORS CANNOT BE NUMERIC. Sorry
# for the inconvenience.

returnAdj <- function(data, measure, covars=c(), interacts=c(), display=F, method='smean', subset=NULL)  {
  
  # "Sanity checks"
  if(length(c(covars,interacts))==0) {
    print('Please retry with covariates/interaction-terms included in your
          returnAdj function.')
    break
  }
 # "Apply Subset if requested"	
	if(is.null(subset)){
		r <- rep(TRUE, nrow(data))
	}
	else{
		r <- subset
	}
# "Build interaction string"
  interStr <- ''
  if(length(interacts)>0) {
    interStr <- interacts[1]
    for(interact in interacts) {
      if(is.numeric(data[,interact])) data[interact] <- scale(data[interact])
      if(interact != interacts[1]) interStr <- paste(interStr,'*',interact)
    }
  }
  if(interStr!='') interStr <- paste0(interStr,'+')

 # "Build covariate string" 
  covarStr <- covars[1]
  for(covar in covars) {
    if(is.numeric(data[,covar])) data[covar] <- scale(data[covar])  
    if(covar != covars[1]) covarStr <- paste(covarStr,'+',covar)  
  }
  
 # "Apply adjustment"	
    mod <- lm(paste(measure,'~',interStr,covarStr),data=data[r, ])
		mod.residuals <- data[, measure] - predict.lm(mod, data)
    if(method=='iadj') data.new <- mod.residuals + mod$coefficients[1]
    if(method=='smean') data.new <- mod.residuals + mean(data[,measure])#, na.rm = TRUE)
    if(method=='null') data.new <- mod.residuals
    names(data.new) <- measure
    if(display) print(paste0(paste(measure,'~',interStr,covarStr),': Orig-Adj r=',signif(cor(cbind(data[measure],data.new[measure]))[1,2],digits=4)))
  
  
  return(data.new)
}
