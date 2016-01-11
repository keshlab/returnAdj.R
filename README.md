# returnAdj.R
Legendary R function to return a data.frame with values adjusted for covariates

returnAdj returns a data.frame with values adjusted for designated covariates. It takes                                                                               
the following parameters:                                                         
- data: The overarching data.frame                                                
- measnames: A vector holding the names of the measures of interest               
- covars: A vector holding the names of the covariates to be used (but not in an interaction term)                                                                    
- interacts: A vector holding the names of covariates to be used in an interaction term                                                                               
- display: A boolean indicating that the original-adjusted value correlations     
      are to be printed. This defaults to true.                                   
- method: A string identifying the mean adjusting methods. This defaults to 'smean'. The other options include 'iadj' and 'null'.
