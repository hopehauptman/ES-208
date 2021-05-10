summ_stat_vis <- function(x){

  #This stops the function is the inputs are not numeric and prints an error message:
  if(!as.numeric(x)) stop('Sorry! x MUST be numeric... Please try again with a numeric vector!')
  
  #This stops the function if the inputs are negative and prints an error message:
  if(x < 0) stop('Sorry, all inputs must be positive. Please try again but this time ONLY use positive numbers!')
 
  #the different summary statistic and basic visualization of data
  st1 <- mean(x, na.rm = TRUE)
  st2 <- median(x, na.rm = TRUE)
  st3 <- sd(x, na.rm = TRUE)
  st4 <- var(x, na.rm = TRUE)
  st5 <- IQR(x, na.rm = TRUE)
  st6 <- moments::skewness(x, na.rm = TRUE)
  st7 <- hist(x)
  st8 <- boxplot(x)

  #returns the outputs in a series
  return(c(Mean=st1, Median=st2, Standard_dev=st3, Varience=st4, Interquartile_range=st5, Skew=st6, histogram = st7, boxplot=st8))
  }
