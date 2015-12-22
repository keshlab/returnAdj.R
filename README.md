# returnAdj.R
Legendary R function to return a data.frame with values adjusted for covariates

returnAdj returns a data.frame with values adjusted for designated covariates. It takes                                                                               
the following parameters:                                                         
- data: The overarching data.frame                                                
- measnames: A vector holding the names of the measures of interest               
- covars: A vector holding the names of the covariates to be used (but not in an interaction term)                                                                    
- interacts: A vector holding the names of covariates to be used in an interaction term                                                                               
- id: A string identifying the variable holding IDs. This defaults to 'SUBJID'    
- display: A boolean indicating that the original-adjusted value correlations     
      are to be printed. This defaults to true.                                   
- groups: A vector of strings identifying the variables holding the grouping information                                                                              
       in the data.frame data. If a variable is designated, then it will be included                                                                                   
       in data.adj. Otherwise, as is the default, no grouping information will be  
       included.                                                                   
                                                                                  
                                                                                  
 Unfortunately, the way that returnAdj presently works, FACTORS CANNOT BE NUMERIC. Sorry                                                                               
 for the inconvenience.              
