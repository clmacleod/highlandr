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



#' sf12_v2 function
#'
#' sf12 version 2 scoring function adapted from SAS code found here https://labs.dgsom.ucla.edu/hays/pages/programs_utilities
#' @param x data frame containing the 12 item sf12 version 2 (must be the version 2)
#' @param revcode_sf2 boolean indicating if sf12 variable sf2 should be reverse coded. it appears that it should be but was not in the sas file used in the above mentioned link. default is FALSE meaning the variable will NOT be recoded
#' @keywords sf12_v2 sf12 sf12v2
#' @export
#' @examples
#' sf12_v2_function()
#'
sf12_v2<-function(x,revcode_sf2=FALSE){
  y<-x
  if ((!(is.data.frame(x) | is.matrix(x))) | (ncol(x) != 12))
    stop("x must be a data.frame (or matrix) with 12 columns")
  x <- as.data.frame(lapply(as.data.frame(x), as.integer))
  names(x) <- c("gh1", "pf02", "pf04", "rp2", "rp3", "re2",
                "re3", "bp2", "mh3", "vt2", "mh4", "sf2")
  threept <- c("pf02", "pf04")
  fivept <- c("gh1","rp2", "rp3", "re2","re3", "bp2", "mh3", "vt2", "mh4", "sf2")

  x[,threept][x[,threept]<1|x[,threept]>3]<-NA
  x[,fivept][x[,fivept]<1|x[,fivept]>5]<-NA

  #c("n_gen_health","n_pain_limit_work","mtg_gen_calm", "mtg_gen_energy", "n_phys_emo_social")

  x$gh1<-dplyr::recode(x$gh1,'1'=5,'2'=4.4,'3'=3.4,'4'=2,'5'=1)
  x$bp2<-6-x$bp2
  x$mh3<-6-x$mh3
  x$vt2<-6-x$vt2
  if(revcode_sf2){
    x$sf2<-6-x$sf2
  }

  PF<-x$pf02+x$pf04
  RP<-x$rp2+x$rp3
  BP<-x$bp2
  GH<-x$gh1
  VT<-x$vt2
  SF<-x$sf2
  RE<-x$re2+x$re3
  MH<-x$mh3+x$mh4

  x$PF<-100*(PF-2)/4
  x$RP<-100*(RP-2)/8
  x$BP<-100*(BP-1)/4
  x$GH<-100*(GH-1)/4
  x$VT<-100*(VT-1)/4
  x$SF<-100*(SF-1)/4
  x$RE<-100*(RE-2)/8
  x$MH<-100*(MH-2)/8

  x$PF_Z = (x$PF - 81.18122) / 29.10588
  x$RP_Z = (x$RP - 80.52856) / 27.13526
  x$BP_Z = (x$BP - 81.74015) / 24.53019
  x$GH_Z = (x$GH - 72.19795) / 23.19041
  x$VT_Z = (x$VT - 55.59090) / 24.84380
  x$SF_Z = (x$SF - 83.73973) / 24.75775
  x$RE_Z = (x$RE - 86.41051) / 22.35543
  x$MH_Z = (x$MH - 70.18217) / 20.50597

  x$AGG_PHYS_raw = (x$PF_Z * 0.42402) +
    (x$RP_Z * 0.35119) +
    (x$BP_Z * 0.31754) +
    (x$GH_Z * 0.24954) +
    (x$VT_Z * 0.02877) +
    (x$SF_Z * -.00753) +
    (x$RE_Z * -.19206) +
    (x$MH_Z * -.22069)

  x$AGG_MENT_raw = (x$PF_Z * -.22999) +
    (x$RP_Z * -.12329) +
    (x$BP_Z * -.09731) +
    (x$GH_Z * -.01571) +
    (x$VT_Z * 0.23534) +
    (x$SF_Z * 0.26876) +
    (x$RE_Z * 0.43407) +
    (x$MH_Z * 0.48581)

  x$AGG_PHYS_T = 50 + (x$AGG_PHYS * 10);
  x$AGG_MENT_T = 50 + (x$AGG_MENT * 10);

  x$PF_T = 50 + (x$PF_Z * 10)
  x$RP_T = 50 + (x$RP_Z * 10)
  x$BP_T = 50 + (x$BP_Z * 10)
  x$GH_T = 50 + (x$GH_Z * 10)
  x$VT_T = 50 + (x$VT_Z * 10)
  x$RE_T = 50 + (x$RE_Z * 10)
  x$SF_T = 50 + (x$SF_Z * 10)
  x$MH_T = 50 + (x$MH_Z * 10)

  scales<-c("PF_T","RP_T","BP_T","GH_T","VT_T","RE_T","SF_T","MH_T")
  aggs<-c("AGG_PHYS_T","AGG_MENT_T")

  return(list("ogvars"=y,"allvars"=x,"scales"=x[,scales],"aggs"=x[,aggs]))
}




#' all_missing_dich function
#'
#' given a vector of variable names and a dataset this function will find all of the variables in the vector that are present in the dataset and then of those determine (using sumisna) if all of them are NA.
#' @param vars vector of variables to evaluate
#' @param data dataset containing the variables
#' @param allmissyes what to return if all of the variables are NA. Default is 1
#' @param allmissno what to return if all of the variables are not NA. Defalt is 0
#' @param sumisna_fun what function to pass to the sumisna function. default is 'is.na'
#' @keywords all_missing_dich missing sumisna missingness all dich any
#' @export
#' @examples
#' all_missing_dich_function()
#'
all_missing_dich<-function(vars,data,allmissyes=1,allmissno=0,sumisna_fun="is.na"){
  vars_pres<-vars[vars %in% names(data)]
  num_vars<-length(vars_pres)

  if(num_vars>1){
    sum_of_nas<-unlist(apply(data[,vars_pres],1,highlandr::sumisna,funct=sumisna_fun))
    ret<-ifelse(sum_of_nas<num_vars,allmissyes,allmissno)
  } else if(num_vars==1){
    ret<-ifelse(is.na(data[,vars_pres]),allmissyes,allmissno)
    print("only one variable found, sumisna_fun will be ignored")
  } else{
    print("no variables in vars found in data")
  }

  return(ret)
}
