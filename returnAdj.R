# returnAdj returns a data.frame with values adjusted for designated covariates. It takes
# the following parameters:
# - data: The overarching data.frame
# - measnames: A vector holding the names of the measures of interest
# - covars: A vector holding the names of the covariates to be used (but not in an interaction term)
# - interacts: A vector holding the names of covariates to be used in an interaction term
# - id: A string identifying the variable holding IDs. This defaults to 'SUBJID'
# - display: A boolean indicating that the original-adjusted value correlations
#       are to be printed. This defaults to true.
# - groups: A vector of strings identifying the variables holding the grouping information
#       in the data.frame data. If a variable is designated, then it will be included
#       in data.adj. Otherwise, as is the default, no grouping information will be
#       included.
#
#
# Unfortunately, the way that returnAdj presently works, FACTORS CANNOT BE NUMERIC. Sorry
# for the inconvenience.

returnAdj <- function(data, measnames, covars=c(), interacts=c(), id='SUBJID', display=T, groups=NULL) {
  
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
  
  rownames(data) <- data[,id]
  data.new <- as.data.frame(data[,id]); names(data.new) <- id
  for(meas in measnames) {
    mod <- lm(paste(meas,'~',interStr,covarStr),data=data)
    data.new[meas] <- merge(data.new[id],mod$residuals+mod$coefficients[1],by.x=1,by.y=0,all.x=T)[,2]
    if(display) print(paste0(paste(meas,'~',interStr,covarStr),': Orig-Adj r=',signif(cor(cbind(data[meas],data.new[meas]))[1,2],digits=4)))
  }
  if(!is.null(groups)) for(group in groups) data.new[group] <- data[group]
  
  return(data.new)
}
