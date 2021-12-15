#' Jaccard function
#'
#' jaccard index (measure of agreement between two cases on suites of binary variables) M.11 / (M.11 + M.10 + M.01)
#' @param x case 1 (the dataset columns in this case should contain only those used for matching)
#' @param y case 2 (the dataset columns in this case should contain only those used for matching)
#' @keywords Jaccard binary distance
#' @export
#' @examples
#' Jaccard_function()

Jaccard <- function (x, y) {
  M.11 <- sum(x == 1 & y == 1)
  M.10 <- sum(x == 1 & y == 0)
  M.01 <- sum(x == 0 & y == 1)
  if (M.11==0){
    return(0)
  }
  else{
    return (M.11 / (M.11 + M.10 + M.01))
  }
}

#' dice function
#'
#' Dice coefficient (measure of agreement between two cases on suites of binary variables) (2(M.11)) / ((2(M.11)) + M.10 + M.01)
#' @param x case 1 (the dataset columns in this case should contain only those used for matching)
#' @param y case 2 (the dataset columns in this case should contain only those used for matching)
#' @keywords dice binary distance
#' @export
#' @examples
#' dice_function()

dice <- function (x, y) {
  M.11 <- sum(x == 1 & y == 1)
  M.10 <- sum(x == 1 & y == 0)
  M.01 <- sum(x == 0 & y == 1)
  if (M.11==0){
    return(0)
  }
  else{
    return ((2*M.11) / ((2*M.11) + M.10 + M.01))
  }
}

#' smc function
#'
#' simple matching coefficient index (measure of agreement between two cases on suites of binary variables) (M.11 + M.00) / (M.11 + M.10 + M.01 + M.00)
#' @param x case 1 (the dataset columns in this case should contain only those used for matching)
#' @param y case 2 (the dataset columns in this case should contain only those used for matching)
#' @keywords smc binary distance simple matching
#' @export
#' @examples
#' smc_function()

smc <- function (x, y) {
  M.11 <- sum(x == 1 & y == 1)
  M.10 <- sum(x == 1 & y == 0)
  M.01 <- sum(x == 0 & y == 1)
  M.00 <- sum(x == 0 & y == 0)
  return ((M.11 + M.00) / (M.11 + M.10 + M.01 + M.00))
}

#' smc_dist function
#'
#' simple matching coefficient distance (measure of agreement between two cases on suites of binary variables) 1-((M.11 + M.00) / (M.11 + M.10 + M.01 + M.00))
#' @param x case 1 (the dataset columns in this case should contain only those used for matching)
#' @param y case 2 (the dataset columns in this case should contain only those used for matching)
#' @keywords smc binary distance simple matching smc_dist
#' @export
#' @examples
#' smc_function()

smc_dist <- function (x, y) {
  M.11 <- sum(x == 1 & y == 1)
  M.10 <- sum(x == 1 & y == 0)
  M.01 <- sum(x == 0 & y == 1)
  M.00 <- sum(x == 0 & y == 0)
  return (1-((M.11 + M.00) / (M.11 + M.10 + M.01 + M.00)))
}

#' ICD function
#'
#' computes average inter-cluster distance for a solution (i.e. average distance between the clusters)
#' @param compgrp the final cluster solution in the form of a mean aggregation of the variables used in the clustering.
#' @param method the method to be used in the ASED function. default is 'eucludean'
#' @keywords ICD cluster outlier inter-cluster distance
#' @export
#' @examples
#' ICD_function()

ICD<-function(compgrp,method="euclidean"){
  icd<-vector()
  for (i in 1:(nrow(compgrp)-1)){
    for (j in (i+1):nrow(compgrp)){
      icd<-c(icd,ASED(compgrp[i,],compgrp[j,],method))
    }
  }
  return(mean(icd))
}


#' ASED function
#'
#' computes average euclidean or squared euclidean distance
#' @param x case 1 (must have the same number of variables as y)
#' @param y case 2 (must have the same number of variables as x)
#' @param method the method to be used in the ASED function. default is 'eucludean'
#' @keywords ASED cluster outlier inter-cluster distance
#' @export
#' @examples
#' ASED_function()

ASED<-function(x=x,y=y,method="euclidean"){
  if (length(x)!=length(y)){
    stop("x and y must be of the same length")
  }

  val<-vector()
  if (method=="euclidean"){
    val<-sqrt(sum(((x-y)^2)))
  }
  if (method=="squared_euclidean"){
    val<-sum(((x-y)^2))
  }
  return(val)
}

#' confint.geeglm function
#'
#' confidence intervals for generalized estimating equation models
#' @param object gee model for which to create confidence intervals
#' @param parm not sure but it came with the function
#' @param level level of confidence needed. defaults to '.95'
#' @param ... extra arguments
#' @keywords confint.geeglm x2 chisquare
#' @export
#' @examples
#' confint.geeglm_function()

confint.geeglm <- function(object, parm, level = 0.95, ...) {
  cc <- as.data.frame(coef(summary(object)))
  mult <- qnorm((1+level)/2)
  citab <-cbind(cc,
                lwr=cc$Estimate-mult*cc$`Robust S.E.`,
                upr=cc$Estimate+mult*cc$`Robust S.E.`)
  return(citab)
}

#' ci.dif function
#'
#' creates a matrix of
#' @param x a vector of frequencies of multiple categories for which a binomial confidence interval should be created
#' @param n number of cases used to create the frequencies
#' @param rownm name of the first varaible in the returned dataframe (name of the frequency categories). defaults to 'categories'
#' @param t100 should the values be multiplied by 100 (used if they are percentages/proportions). defaults to TRUE
#' @keywords ci.dif confidence interval
#' @export
#' @examples
#' ci.dif_function()

ci.dif<-function(x,n,rownm="categories",t100=TRUE){
  d<-data.frame()
  for (i in 1:length(x)){
    t<-prop.test(x[i],n)
    d[i,1]<-names(x[i])
    d[i,2]<-t$estimate-t$conf.int[1]
    d[i,3]<-t$conf.int[2]-t$estimate
    d[i,4]<-t$estimate
    d[i,5]<-t$conf.int[1]
    d[i,6]<-t$conf.int[2]
  }
  colnames(d)<-c(rownm,"lbound_diff","ubound_diff","estimate","LB","UB")
  if (t100==TRUE){d[,2:6]<-d[,2:6]*100}
  return(d)
}

#' h.oneprop function
#'
#' given one proportion and the effect size this function calculated the other proportion (i.e. so instead of having two proportions and calculating the effect size between the two)
#' @param prop1 the stable proportion that can be estimated or expected
#' @param h the variable to aggregate by (i.e. the classes)
#' @keywords h.oneprop power analysis pwr chi sqaure proportion
#' @export
#' @examples
#' h.oneprop_function()

h.oneprop<-function(prop1,h){
  otherprop<-sin((asin(sqrt(prop1))+abs(h/2)))^2
  return(otherprop)
}


#' pct_incr_time function
#'
#' given one proportion and the effect size this function calculated the other proportion (i.e. so instead of having two proportions and calculating the effect size between the two)
#' @param init the initial or starting value
#' @param yr the number of years or iterations to run through
#' @param inc the percent increase or decrease formatted as a number with 1 returning the same value, less than 1 reducing the value, greater than one increasing the value.
#' @keywords pct_incr_time percent increase over time compound
#' @export
#' @examples
#' pct_incr_time_function()
#'
pct_incr_time<-function(init,yr,inc){
  for(i in 1:yr){
    init<-init*inc
  }
  return(init)
}
