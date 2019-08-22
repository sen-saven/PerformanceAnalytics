#'@title Minimum Track Record Length
#'
#'@description
#'Minimum Track Record Length tells us "How long should a track record be in 
#'order to have statistical confidence that its Sharpe ratio is above a given 
#'threshold? ". If a track record is shorter than MinTRL, we do not have enough
#'confidence that the observed Sharpe Ratio is above the designated threshold.
#'The reference Sharpe Ratio should be less than the observed Sharpe Ratio and 
#'the Values should be given in non-annualized terms, in the same periodicity as
#'the return series. The Minimum Track Record Length is also given in the same 
#'Periodicity as the Return Series.
#'
#'\deqn{MinTRL = n^\ast = 1+\biggl[1-\hat{\gamma_3}\hat{SR}+\frac{\hat{\gamma_4}}{4}\hat{SR^2}\biggr]\biggl(\frac{Z_\alpha}{\hat{SR}-SR^\ast}\biggr)^2}
#'
#'\eqn{\gamma{_3}} and \eqn{\gamma{_4}} are the skewness and kurtosis respectively. 
#'It is important to note that MinTRL is expressed in terms of number of observations,
#'not annual or calendar terms.
#'
#'The sharpe ratio , skewness and kurtosis can be directly given if the return series 
#'is not available using the input parameters sr,sk and kr. If the return series 
#'is available these parameters can be left.
#'
#'weights will be needed to be entered if a portfolio's MinTRL is to be calculated
#'else weight can be left as NULL.
#'
#'@aliases MinTrackRecord
#'
#'@param R an xts, vector, matrix, data frame, timeSeries or zoo object of asset return 
#'@param Rf the risk free rate of return
#'@param refSR the reference Sharpe Ratio, can be a single value or a vector for a multicolumn
#'  return series.Should be non-annualized , in the same periodicity as the returns.
#'@param p the confidence level
#'@param weights the weights for the portfolio
#'@param sr Sharpe Ratio,in the same periodicity as the returns(non-annualized).
#'To be given in case the return series is not given.
#'@param sk Skewness, in the same periodicity as the returns(non-annualized).
#'To be given in case the return series is not given.
#'@param kr Kurtosis, in the same periodicity as the returns(non-annualized).
#'To be given in case the return series is not given.
#'@param \dots any other passthru variable
#'
#'@author Pulkit Mehrotra
#'@seealso \code{\link{ProbSharpeRatio}} \code{\link{PsrPortfolio}} \code{\link{table.PSR}}
#'@references Bailey, David H. and Lopez de Prado, Marcos, \emph{The Sharpe Ratio 
#'Efficient Frontier} (July 1, 2012). Journal of Risk, Vol. 15, No. 2, Winter
#' 2012/13
#'@keywords ts multivariate distribution models
#'@examples
#'
#'data(edhec)
#'MinTrackRecord(edhec[,1],refSR=0.1,Rf = 0.04/12)
#'MinTrackRecord(refSR = 1/12^0.5,Rf = 0,p=0.95,sr = 2/12^0.5,sk=-0.72,kr=5.78)
#'MinTrackRecord(edhec[,1:2],refSR = c(0.28,0.24))
#'
#'data(managers)
#'MinTrackRecord(managers,refSR = 0)
#'@export
#'
MinTrackRecord<-function(R = NULL, refSR,Rf=0,p = 0.95, weights = NULL,sr = NULL,sk = NULL, kr = NULL,ignore_skewness = FALSE, ignore_kyrtosis = TRUE ...){
  
  num_of_cols = 1
  column_names = NULL
  #Error handling if R is not NULL
  if(!is.null(R)){
    x = checkData(R)
    num_of_cols = ncol(x)
    n = nrow(x)
    #Checking if the weights are provided or not
    if(!is.null(weights)){
      if(length(weights)!=num_of_cols){
        stop("number of items in weights is not equal to the number of columns in R")
      }
      else{
        # A potfolio is constructed by applying the weights
        x = Return.portfolio(R,weights)
      }
    }
    
    sr = SharpeRatio(x, Rf, p, "StdDev")
    sk = skewness(x)
    kr = kurtosis(x,method='moment')
  }
  
  column_names = colnames(x)
  if(length(refSR)!=num_of_cols){      stop("Reference Sharpe Ratio should be given for each series")}
  
  
}
# If R is passed as null checking for sharpe ratio , skewness and kurtosis 
else{
  
  if(is.null(sr) | is.null(sk) | is.null(kr) | is.null(n)){
    stop("You must either pass R or the Sharpe ratio, Skewness, Kurtosis,n etc")
  }
}

if(!is.null(dim(Rf))){
  Rf = checkData(Rf)
}
#If the Reference Sharpe Ratio is greater than the Observred Sharpe Ratio an error is displayed
index_of_higher_tr = which(refSR>sr)
if(length(index_of_higher_tr)!=0){
  if(length(index_of_higher_tr)==num_of_cols){
    stop("The reference Sharpe Ratio greater than the Observed Sharpe ratio for all the timeseries")
  }
  sr = sr[-index_of_higher_tr]
  refSR = refSR[-index_of_higher_tr]
  sk = sk[-index_of_higher_tr]
  kr = kr[-index_of_higher_tr]
  column_names = column_names[-index_of_higher_tr]
  warning(paste("The Reference Sharpe Ratio greater than the Observed Sharpe Ratio for the returns of: ",column_names[index_of_higher_tr],"\n"))
  
}

if(ignore_skewness) sk = rep(0,num_of_cols)
if(ignore_kyrtosis) kr = rep(3,num_of_cols)

min_tr = 1 + (1 - sk*sr + ((kr-1)/4)*sr^2)*(qnorm(p)/(sr-refSR))^2
if(!is.null(dim(result))){ 
  colnames(result) = paste(columnnames,"(SR >",round(refSR,2),")") 
  rownames(result) = paste("Probabilistic Sharpe Ratio(p=",round(p*100,1),"%):")
}
return(result)
}
