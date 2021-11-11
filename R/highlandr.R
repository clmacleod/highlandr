#' time conversion function
#'
#' This function turns a timestamp into a time variable in secs and turns it into hours.
#' @param x is the timestamp argument to be converted.
#' @keywords mint time
#' @export
#' @examples
#' mint_function()

mint<-function(x){
  x<-round(60 * 24 * as.numeric(times(x)),2)
  return(x)
}

#' tween function
#'
#' This function checks whether a number is betwee two other numbers
#' @param x is the number to check
#' @param bot the lower (bottom) number to check against
#' @param top the higher (top) number to check against
#' @keywords tween between
#' @export
#' @examples
#' tween_function()

tween<-function(x,bot,top){
  if (typeof(x)=="character"){x<-as.numeric(as.character(x))}
  result<-ifelse(x>=bot & x<=top,TRUE,FALSE)
  return(result)
}

#' cton conversion function
#'
#' This function changes characters to numbers
#' @param x is the character argument to be converted.
#' @keywords cton character number
#' @export
#' @examples
#' cton_function()

cton<-function(x){
  x<-as.numeric(as.character(x))
  return(x)
}

#' percent conversion function
#'
#' takes a decimal number and turns it into a character with specified digits and '%' following
#' @param x is the numeric argument to be converted
#' @param digits is the number of digits the number should be rounded to
#' @param format is the format that the character should be
#' @param ... additional arguments to pass to format or paste0
#' @keywords cton character number
#' @export
#' @examples
#' percent_function()

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

#' freqtbl_basic function
#'
#' creates frequency tables the way I like them. with freq, valid %, and in descending order of frequency. this function allows you to change the name of the category columns, and whether or not totals and NAs are displayed
#' @param variable the variable from which a frequency table will be created
#' @param name the name to appear in the table for the different categories. defaults to 'Category'
#' @param total boolean argument determining if a total should be added to the bottom of the table. defaults to FALSE
#' @param countna boolean argument determining if na's should be omitted (FALSE) or added as a frequency category (TRUE). defaults to TRUE
#' @param orderbycat boolean argument determining if the table sould be sorted by its categories (TRUE) instead of its frequencies (FALSE). defaulst to FALSE
#' @param reverseorder boolean argument determining if the table should be sorted from lowest to highest frequency (FALSE). defaults to TRUE
#' @keywords freqtbl frequency table freqtbl_basic
#' @export
#' @examples
#' freqtbl_basic_function()

freqtbl_basic<-function(variable,name="Category",total=FALSE,countna=TRUE,orderbycat=FALSE,reverseorder=TRUE){
  if (countna==FALSE){
    variable<-na.omit(variable)
  }
  cnts<-plyr::count(variable)
  cnts[,1]<-as.character(cnts[,1])
  names(cnts)[1]<-name
  if (orderbycat == TRUE){
    cnts<-cnts[order((cnts[1]),decreasing = reverseorder),]
  }
  else{
    cnts<-cnts[order((cnts$freq),decreasing = reverseorder),]
  }
  cnts$perc<-percent(cnts$freq/(sum(cnts$freq)))

  if (total==TRUE){
    freqsum<-sum(cnts$freq)
    totrow<-c("Total",freqsum,"100.00%")
    cnts<-rbind(cnts,totrow)
  }

  return(cnts)
}


#' freqtbl_superbasic function
#'
#' creates frequency tables the way I like them. with freq, valid %, and in descending order of frequency. this function allows you to change the name of the category columns, and whether or not totals and NAs are displayed
#' @param variable the variable from which a frequency table will be created
#' @param name the name to appear in the table for the different categories. defaults to 'Category'
#' @param total boolean argument determining if a total should be added to the bottom of the table. defaults to FALSE
#' @param countna boolean argument determining if na's should be omitted (FALSE) or added as a frequency category (TRUE). defaults to TRUE
#' @keywords freqtbl frequency table freqtbl_superbasic
#' @export
#' @examples
#' freqtbl_superbasic_function()

freqtbl_superbasic<-function(variable,name="Category",total=FALSE,countna=TRUE){
  if (countna==FALSE){
    variable<-na.omit(variable)
  }
  cnts<-plyr::count(variable)
  cnts[,1]<-as.character(cnts[,1])
  names(cnts)[1]<-name
  cnts<-cnts[order((cnts$freq),decreasing = TRUE),]
  cnts$perc<-percent(cnts$freq/(sum(cnts$freq)))

  if (total==TRUE){
    freqsum<-sum(cnts$freq)
    totrow<-c("Total",freqsum,"100.00%")
    cnts<-rbind(cnts,totrow)
  }

  return(cnts)
}


#' freqtbl function
#'
#' creates frequency tables the way I like them. with freq, valid %, and in descending order of frequency. this function allows you to change the name of the category columns, and whether or not totals and NAs are displayed. this funtion can also add standard binomial and multinomial confidence intervals if desired.
#' @param variable the variable from which a frequency table will be created
#' @param name the name to appear in the table for the different categories. defaults to 'Category'
#' @param total boolean argument determining if a total should be added to the bottom of the table. defaults to FALSE
#' @param countna boolean argument determining if na's should be omitted (FALSE) or added as a frequency category (TRUE). defaults to TRUE
#' @param orderbycat boolean argument determining if the table sould be sorted by its categories (TRUE) instead of its frequencies (FALSE). defaulst to FALSE
#' @param reverseorder boolean argument determining if the table should be sorted from lowest to highest frequency (FALSE). defaults to TRUE
#' @param mci optional argument to compute multinomial (more than 2 categories) confidence intervals using MultinomCI. can take any of the method inputs for MultinomCI ("sisonglaz", "cplus1", "goodman", "wald", "waldcc", "wilson"). defaults to 'none' which means mutli cis will not be included.
#' @param bci optional argument to compute standard binomial (2 categories) confidence intervals using BinomCI. can take any of the method inputs for BinomCI ("wald", "wilson", "wilsoncc", "agresti-coull", "jeffreys", "modified wilson", "modified jeffreys", "clopper-pearson", "arcsine", "logit", "witting" or "pratt"). defaults to 'none' which means binom cis will not be included.
#' @param rnd number of decimals to round all calculations to
#' @keywords freqtbl frequency table
#' @export
#' @examples
#' freqtbl_function()

freqtbl<-function(variable,name="Category",total=FALSE,countna=TRUE,orderbycat=FALSE,reverseorder=TRUE,mci="none",bci="none",rnd=2){
  if (countna==FALSE){
    variable<-na.omit(variable)
  }
  cnts<-plyr::count(variable)
  cnts[,1]<-as.character(cnts[,1])
  names(cnts)[1]<-name
  if (orderbycat == TRUE){
    cnts<-cnts[order((cnts[1]),decreasing = reverseorder),]
  }
  else{
    cnts<-cnts[order((cnts$freq),decreasing = reverseorder),]
  }
  cnts$perc<-percent(cnts$freq/(sum(cnts$freq)),digits = rnd)

  if (total==TRUE){
    freqsum<-sum(cnts$freq)
    totrow<-c("Total",freqsum,"100.00%")
    cnts<-rbind(cnts,totrow)
  }
  if (bci!="none"&mci!="none"){warning("choose only bci or mci not both")}
  if (bci!="none"){
    bis<-sum(cton(variable))
    bin<-length(variable)
    conf<-data.frame(BinomCI(bis,bin,sides = "two.sided",method = bci))
    conf<-rownames_to_column(conf,var = name)
    conf<-conf[,-match("est",colnames(conf))]
    conf[,c("lwr.ci")]<-percent(conf[,c("lwr.ci")],digits = rnd)
    conf[,c("upr.ci")]<-percent(conf[,c("upr.ci")],digits = rnd)
    cnts<-merge(cnts,conf,by=name,all.x=TRUE,sort=FALSE)
  }
  if (mci!="none"){
    conf<-MultinomCI(table(variable,useNA = 'always'),sides = "two.sided",method = mci)
    rn<-rownames(conf)
    conf<-as.data.frame(conf)
    conf[,name]<-rn
    conf<-conf[,-match("est",colnames(conf))]
    conf[,c("lwr.ci")]<-percent(conf[,c("lwr.ci")],digits = rnd)
    conf[,c("upr.ci")]<-percent(conf[,c("upr.ci")],digits = rnd)
    cnts<-merge(cnts,conf,by=name,all.x=TRUE,sort=FALSE)
  }
  return(cnts)
}

#' MCsmry function
#'
#' multiple choice summary summarizes variables that have multiple choice options listed with a delimeter this function gives a table with the count & % of all cases containing each element in the list
#' @param elementlist the list of multiple choice elements in the variable to look for and count
#' @param variable the variable you wish to summarize
#' @param total boolean argument determining if a total should be added to the bottom of the table. defaults to FALSE
#' @keywords freqtbl frequency table MCsmry multiple choice
#' @export
#' @examples
#' MCsmry_function()

MCsmry<-function(elementlist,variable,total=FALSE){
  retfreq<-vector()
  for (m in 1:length(elementlist)){
    retfreq<-c(retfreq,sum(grepl(elementlist[[m]],variable)))
  }
  perc<-percent((retfreq/length(variable)))
  retfreq<-as.data.frame(cbind(option=elementlist,freq=retfreq,percent=perc))
  retfreq$freq<-as.numeric(as.character(retfreq$freq))
  retfreq<-retfreq[order((retfreq$freq),decreasing = TRUE),]

  if (total==TRUE){
    freqsum<-sum(retfreq$freq)
    totrow<-c("Total",freqsum,"100%")
    retfreq<-rbind(retfreq,totrow)
  }
  return(retfreq)
}


#' dfsmry function
#'
#' summarize a data frame in a table giving ncases, nvar, and average missing
#' @param x the data frame you wish to summarize
#' @param rowname the name you would like to give the summary rows. defaults to 'Values'
#' @keywords dfsmry frequency table summary dataframe
#' @export
#' @examples
#' dfsmry_function()

dfsmry<-function(x,rowname="Values"){
  ret<-cbind(Ncases=round(nrow(x),digits=0),Nvars=round(ncol(x),digits=0),AvgMis=round(mean(apply(apply(x,2,is.na),2,sum)),2))
  rownames(ret)[1]<-rowname
  return(ret)
}

#' cbplist function
#'
#' Boxplot using a list dataset
#' @param x the list you wish to summarize
#' @param txt the text scale you wish to use
#' @param ttext boolean argument determining label orientation and overlap. this argument forces the labels to appear if TRUE even if they overlap. defaults to FALSE
#' @param code boolean argument determining if a numeric code of 1:nrow should be used as the names in the plot (TRUE) or if the values in the first column should be used as the name argument (FALSE). defaults to FALSE
#' @keywords cbplist boxplot
#' @export
#' @examples
#' cbplist_function()

cbplist<-function(x,txt=1,ttext=FALSE,code=FALSE){
  ttext<-ifelse(ttext==TRUE,2,0)
  chain<-list()
  for (i in 1:length(x)){
    if (code==TRUE){cd<-1:nrow(x[[i]])}else{cd<-x[[i]][,1]}
    chain[[i]]<-barplot(x[[i]][,2],names.arg = cd,xlab=colnames(x[[i]][1]),ylab="Frequency",cex.names = txt,ylim = c(0,(max(x[[i]][,2]+5))),las=ttext)
  }
  return(chain)
}


#' cbp function
#'
#' Colin boxplot using regular data (i.e. cases as rows)
#' @param x the dataset you wish to summarize
#' @param txt the text scale you wish to use
#' @param ttext boolean argument determining label orientation and overlap. this argument forces the labels to appear if TRUE even if they overlap. defaults to FALSE
#' @param code boolean argument determining if a numeric code of 1:nrow should be used as the names in the plot (TRUE) or if the values in the first column should be used as the name argument (FALSE). defaults to FALSE
#' @keywords cbp boxplot
#' @export
#' @examples
#' cbp_function()

cbp<-function(x,txt=.5,ttext=FALSE,code=FALSE){
  ttext<-ifelse(ttext==TRUE,2,0)
  if (code==TRUE){cd<-1:nrow(x)}else{cd<-x[,1]}
  barplot(x[,2],main=paste(colnames(x[1]),"Barplot"),names.arg = cd,xlab=colnames(x[1]),ylab="Frequency",cex.names = txt,ylim = c(0,(max(x[,2]+5))),las=ttext)
  #return(chain)
}


#' varlist function
#'
#' turn the list of variables into a presentable and readable data frame by breaking the list of variables across columns based on what the set is divisible by
#' @param x the dataset you wish to summarize
#' @keywords varlist variable var
#' @export
#' @examples
#' varlist_function()

varlist<-function(x){
  cc<-with(x,ifelse(ncol(x)<10,1,ifelse(((ncol(x)%%5)==0),5,ifelse(((ncol(x)%%4)==0),4,ifelse(((ncol(x)%%3)==0),3,ifelse(((ncol(x)%%2)==0),2,1))))))
  return(as.data.frame(matrix(colnames(x),ncol = cc)))
}


#' wssplot function
#'
#' scree plot for heirarchical clustering with built in kmeans relocation
#' @param data the data you wish to create the plot for. must be only the data that is clustered
#' @param centers the centers resulting from a hierarchical cluster analysis
#' @param nc number of clusters you want to take the plot out to. defaults to '15'
#' @keywords wssplot hierarchical cluster scree plot
#' @export
#' @examples
#' wssplot_function()

wssplot <- function(data,centers, nc=15){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    wss[i] <- sum(kmeans(data, centers=centers[[i]])$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}


#' wssplot_km function
#'
#' scree plot for kmeans clustering
#' @param data the data you wish to create the plot for. must be only the data that is clustered
#' @param nc number of clusters you want to take the plot out to. defaults to '15'
#' @param seed lets you set the random seed for the kmeans algorithm for reproducability
#' @keywords wssplot kmeans cluster scree plot
#' @export
#' @examples
#' wssplot_km_function()

wssplot_km <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}


#' clustplot_old function
#'
#' multicluster barplot for summarizing cluster solutions (deprecated I believe)
#' @param dframe the data frame you wish to create the plot for. the dataframe must contain 3 elements. 1.) first column must contain the unique identifiers. 2.) there must be one or more of the raw data variables. 3.) there must be at least one cluster variable of the format 'hkclust' followed by a number (e.g. 'hkclust5' would be a 5 cluster solution).
#' @param clusters number of clusters you want to create the plot for. there must be a variable corresponding to this number with the format hkclust (e.g. if you put '5' here there must be a variable named 'hkclust5'- element 3 above)
#' @param varlist the list of variable used for the clustering (this is the raw data-element 2 above)
#' @keywords cluster plot barchart bargraph clustplot
#' @export
#' @examples
#' clustplot_old_function()

clustplot_old<-function(dframe,clusters,varlist){
  solcenters<-list()
  name<-paste("hkclust", clusters , sep = "")

  solcentmat<-aggregate(dframe[,varlist],by=list(solution = dframe[,name]),mean)
  solcent<-as.matrix(solcentmat[,2:(ncol(solcentmat))])
  solcenters[[name]]<-solcent

  mycols<-topo.colors((ncol(solcentmat))-1)
  barplots<-barplot(t(solcent),beside = TRUE,names.arg = rep(paste("cluster",1:clusters)),xpd = FALSE,xlab = "Cluster Number",ylab = "Value",main = paste(clusters,"Cluster Solution"),col=mycols,ylim = c(round(((min(solcent)-sd(solcent))-.5),.5),round(((max(solcent)+sd(solcent))+.5),.5)))
  text(barplots, t(solcent),pos = ifneg(solcent), round(t(solcent)[,1:clusters],2),cex=1)
}


#' clustplot function
#'
#' multicluster barplot for summarizing cluster solutions
#' @param dframe the data frame you wish to create the plot for. the dataframe must contain 3 elements. 1.) first column must contain the unique identifiers. 2.) there must be one or more of the raw data variables. 3.) there must be at least one cluster variable of the format 'hkclust' followed by a number (e.g. 'hkclust5' would be a 5 cluster solution).
#' @param clusters number of clusters you want to create the plot for. there must be a variable corresponding to this number with the format hkclust (e.g. if you put '5' here there must be a variable named 'hkclust5'- element 3 above)
#' @param varlist the list of variable used for the clustering (this is the raw data-element 2 above)
#' @param txtcex scale variable for text
#' @keywords cluster plot barchart bargraph clustplot
#' @export
#' @examples
#' clustplot_function()

clustplot<-function(dframe,clusters,varlist,txtcex=1){
  solcenters<-list()
  name<-paste("hkclust", clusters , sep = "")

  solcentmat<-aggregate(dframe[,varlist],by=list(solution = dframe[,name]),mean)
  solcent<-as.matrix(solcentmat[,2:(ncol(solcentmat))])
  solcenters[[name]]<-solcent
  clust_n<-table(dframe[,name])

  mycols<-topo.colors((ncol(solcentmat))-1)
  barplots<-barplot(t(solcent),beside = TRUE,names.arg = rep(paste("cluster ",1:clusters,"\n(n=",clust_n,")",sep = "")),xpd = FALSE,xlab = "Cluster Number",ylab = "Value",main = paste(clusters,"Cluster Solution"),col=mycols,ylim = c(round(((min(solcent)-sd(solcent))-.5),.5),round(((max(solcent)+sd(solcent))+.5),.5)))
  text(barplots, t(solcent),pos = ifneg(solcent), round(t(solcent)[,1:clusters],2),cex=txtcex)
}


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


#' dissm_jaccard function
#'
#' returns a pairwise dissimilarity matrix using jaccard distance (i.e. 1-jaccard index) to be used for dichotmous clustering
#' @param y the dataset for which a pairwise dissimilarity matrix should be created (the dataset columns should contain only those used for clustering - i.e. the raw data no identifiers or other variables)
#' @keywords jaccard distance matrix dissm_jaccard binary dichotomous cluster
#' @export
#' @examples
#' dissm_jaccard_function()

dissm_jaccard<-function(y){
diss_m_jc = matrix(data = NA, nrow = nrow(y), ncol = nrow(y))
for (r in 1:nrow(y)) {
  for (c in 1:nrow(y)) {
    diss_m_jc[r,c] = 1-Jaccard(y[r,], y[c,])
  }
}
return(diss_m_jc)
}

#' dissm_dice function
#'
#' returns a pairwise dissimilarity matrix using dice distance (i.e. 1-dice coefficient) to be used for dichotmous clustering
#' @param y the dataset for which a pairwise dissimilarity matrix should be created (the dataset columns should contain only those used for clustering - i.e. the raw data no identifiers or other variables)
#' @keywords dice distance matrix dissm_dice binary dichotomous cluster
#' @export
#' @examples
#' dissm_dice_function()

dissm_dice<-function(y){
diss_m_dc = matrix(data = NA, nrow = nrow(y), ncol = nrow(y))
for (r in 1:nrow(y)) {
  for (c in 1:nrow(y)) {
    diss_m_dc[r,c] = 1-dice(y[r,], y[c,])
  }
}
return(diss_m_dc)
}

#' dissm_smc function
#'
#' returns a pairwise dissimilarity matrix using simple matching distance (i.e. 1-simple matching coefficient) to be used for dichotmous clustering
#' @param y the dataset for which a pairwise dissimilarity matrix should be created (the dataset columns should contain only those used for clustering - i.e. the raw data no identifiers or other variables)
#' @keywords smc simple matching distance matrix dissm_smc binary dichotomous cluster
#' @export
#' @examples
#' dissm_smc_function()

dissm_smc<-function(y){
diss_m_sm = matrix(data = NA, nrow = nrow(y), ncol = nrow(y))
for (r in 1:nrow(y)) {
  for (c in 1:nrow(y)) {
    diss_m_sm[r,c] = 1-smc(y[r,], y[c,])
  }
}
return(diss_m_sm)
}


#' distoon function
#'
#' allows a function (particularly a self defined function) to be used in the quoted call of another function. an example of this is the heatmaply function which takes an argument 'distfun' which expects a quoted distance function such as "spearman" which cannot be substituted for a regular function name. instead use distoon to set the name and call distoon in it's place in the functions argument
#' @param x the data used
#' @param meth the distance function (method) used
#' @keywords distoon proxy function
#' @export
#' @examples
#' distoon_function()

distoon<-function(x,meth){
  mart<-proxy::dist(x,method = meth)
  return(mart)
}

#' ifneg function
#'
#' checks if the transposed values of a matrix are negative and maps different values to it if they are or are not
#' @param mat matrix to be used (should not already be transposed as the matrix does this-if the martix is in the orientation needed for a barplot without transposing you may need to transpose it when feeding into this function)
#' @param t the value to map the negative values to. defaults to 1
#' @param f the value to map the positive values to. defaults to 3
#' @keywords ifneg check barplot
#' @export
#' @examples
#' ifneg_function()

ifneg<-function(mat,t=1,f=3){
  tf<-t(mat)<0
  reps<-mapvalues(tf,c(TRUE,FALSE),c(t,f))
  return(reps)
}


#' reassign function
#'
#' reassign function for reassigning outlier cases to clusters after a solution has been made using lowest average euclidean distance (ASED)
#' @param reassignobj case to be reassigned to a cluster. this must take the form of a row with as many columns as there were variables used in the clustering- the variables must also come in the same order
#' @param compgrp the final cluster solution in the form of a mean aggregation of the variables used in the clustering. variables must be in the same amount and order as reassignobj
#' @param method the method to be used in the ASED function. default is 'eucludean'
#' @keywords reassign cluster outlier
#' @export
#' @examples
#' reassign_function()

reassign<-function(reassignobj,compgrp,method="euclidean"){
  if (length(reassignobj)!=ncol(compgrp)){
    stop("the reassigned object and comparison group must be of the same length")
  }

  ncdistance<-vector()
  icd<-ICD(compgrp)

  for (i in 1:nrow(compgrp)){
    ncdistance<-c(ncdistance,ASED(reassignobj,compgrp[i,],method))
  }

  mindist<-min(ncdistance)
  cluster<-match(mindist,ncdistance)
  icdexceed<-ifelse(mindist>icd,"exceeds","does not exceed")
  dupval<-ifelse(sum(ncdistance==mindist)>1,"multiple matches","one match")

  if(icdexceed=="exceeds"){warning("The distance from the match object to the nearest cluster exceeded the average inter-cluster distance")}
  if(dupval=="multiple matches"){warning("The case had multiple cluster matches, cluster match may not be optimal")}

  return(list(cluster=cluster,mindist=mindist,ncdistance=ncdistance,icd=icd,icdexceed=icdexceed,dupval=dupval))
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



#' maxclustL function
#'
#' computes and returns the number of the cluster solution that has the maximum difference to the cluster at the left side in an ascending cluster list of fit values
#' @param cindex a vector of fit index values representing cluster fit and requiring a max distance to the left solution as coming from the clustIndex function in the cclust package. Andreas Weingessel, Evgenia Dimitriadou and Sara Dolnicar, (An Examination Of Indexes For Determining The Number Of Clusters In Binary Data Sets, http://epub.wu.ac.at/1542/)
#' @keywords maxclustL cluster outlier inter-cluster distance fit solution
#' @export
#' @examples
#' maxclustL_function()

maxclustL<-function(cindex){
  for (s in 2:length(cindex)){
    Lval<-cindex[s]-cindex[s-1]
  }
  maxLlist<-list(rec=(match(max(Lval),Lval)+1),allvals=Lval)
  return(maxLlist)
}


#' maxclustR function
#'
#' computes and returns the number of the cluster solution that has the maximum difference to the cluster at the right side in an ascending cluster list of fit values
#' @param cindex a vector of fit index values representing cluster fit and requiring a max distance to the right solution as coming from the clustIndex function in the cclust package. Andreas Weingessel, Evgenia Dimitriadou and Sara Dolnicar, (An Examination Of Indexes For Determining The Number Of Clusters In Binary Data Sets, http://epub.wu.ac.at/1542/)
#' @keywords maxclustR cluster outlier inter-cluster distance fit solution
#' @export
#' @examples
#' maxclustR_function()

maxclustR<-function(cindex){
  for (t in 1:(length(cindex)-1)){
    Rval<-cindex[t]-cindex[t+1]
  }
  maxRlist<-list(rec=(match(max(Rval),Rval)+1),allvals=Rval)
  return(maxRlist)
}


#' maxclustSD function
#'
#' computes and returns the number of the cluster solution that has the maximum of the second differences in an ascending cluster list of fit values
#' @param cindex a vector of fit index values representing cluster fit and requiring a minimum of the second differences solution as coming from the clustIndex function in the cclust package. Andreas Weingessel, Evgenia Dimitriadou and Sara Dolnicar, (An Examination Of Indexes For Determining The Number Of Clusters In Binary Data Sets, http://epub.wu.ac.at/1542/)
#' @keywords maxclustSD cluster outlier inter-cluster distance fit solution
#' @export
#' @examples
#' maxclustSD_function()

maxclustSD<-function(cindex){
  for (u in 3:length(cindex)-1){
    maxSDval<-(cindex[u+1]-cindex[u])-(cindex[u]-cindex[u-1])
  }
  maxSDlist<-list(rec=(match(max(maxSDval),maxSDval)+1),allvals=maxSDval)
  return(maxSDlist)
}


#' minclustSD function
#'
#' computes and returns the number of the cluster solution that has the minimum of the second differences in an ascending cluster list of fit values
#' @param cindex a vector of fit index values representing cluster fit and requiring a minimum of the second differences solution as coming from the clustIndex function in the cclust package. Andreas Weingessel, Evgenia Dimitriadou and Sara Dolnicar, (An Examination Of Indexes For Determining The Number Of Clusters In Binary Data Sets, http://epub.wu.ac.at/1542/)
#' @keywords minclustSD cluster outlier inter-cluster distance fit solution
#' @export
#' @examples
#' minclustSD_function()

minclustSD<-function(cindex){
  for (v in 3:length(cindex)-1){
    minSDval<-(cindex[v+1]-cindex[v])-(cindex[v]-cindex[v-1])
  }
  minSDlist<-list(rec=(match(min(minSDval),minSDval)+1),allvals=minSDval)
  return(minSDlist)
}


#' clustrecs function
#'
#' for calculating optimal cluster numbers based on clustIndex (from An Examination Of Indexes For Determining The Number Of Clusters In Binary Data Sets http://epub.wu.ac.at/1542/  xuindex from https://www.researchgate.net/figure/Example-of-the-Xu-index-applied-to-synthetic-data-Data-classified-into-5-classes-left_fig1_221472151)
#' @param indxdf a dataframe of all indexes from clustIndex for which the best fit solution for each index will be calculated
#' @keywords clustrecs cluster outlier inter-cluster distance fit solution
#' @export
#' @examples
#' clustrecs_function()

clustrecs<-function(indxdf){
  indxmat<-indxdf[,2:ncol(indxdf)]
  cr<-vector()
  criteria<-list()
  recs<-list()

  for (g in 1:nrow(indxmat)){
    indxlist<-indxmat[g,]

    if (indxdf[g,1]=="scott"| indxdf[g,1]=="ball"| indxdf[g,1]=="friedman"){
      crec<-maxclustL(indxlist)
      criteria[[g]]<-crec[[2]]
      cr<-c(cr,crec[[1]])
    }
    if(indxdf[g,1]=="ratkowsky"){
      crec<-maxclustR(indxlist)
      criteria[[g]]<-crec[[2]]
      cr<-c(cr,crec[[1]])
    }
    if (indxdf[g,1]=="ssi"| indxdf[g,1]=="xuindex"){
      crec<-match(max(indxlist),indxlist)
      criteria[[g]]<-indxlist
      cr<-c(cr,crec)
    }
    # if (indxdf[g,1]=="trcovw"){
    #   crec<-minclustSDL(indxlist)
    #   criteria[[g]]<-crec[[2]]
    #   cr<-c(cr,crec[[1]])
    # }
    if (indxdf[g,1]=="db"){
      crec<-match(min(indxlist),indxlist)
      criteria[[g]]<-indxlist
      cr<-c(cr,crec)
    }
    if (indxdf[g,1]=="calinski"| indxdf[g,1]=="hartigan"| indxdf[g,1]=="rubin"){
      crec<-minclustSD(indxlist)
      criteria[[g]]<-crec[[2]]
      cr<-c(cr,crec[[1]])
    }
    if (indxdf[g,1]=="cindex"| indxdf[g,1]=="marriot"| indxdf[g,1]=="tracew"| indxdf[g,1]=="likelihood"){
      crec<-maxclustSD(indxlist)
      criteria[[g]]<-crec[[2]]
      cr<-c(cr,crec[[1]])
    }


  }
  recs<-list(recs=cr,criteria=criteria)
  return(recs)
}


#' indxDF function
#'
#' creates the proper dataframe format and inputs it into the clustrecs function (http://epub.wu.ac.at/1542/)
#' @param dat a dataframe of the raw data to be clustered
#' @param cslist cluster solution list is a list of different hierarchical cluster solutions to be evaluated for model fit
#' @param mxc maximum cluster solution to check. default is set to all of the solutions in 'cslist' but can be fewer if desired
#' @keywords indxDF cluster outlier inter-cluster distance fit solution
#' @export
#' @examples
#' indxDF_function()

indxDF<-function(dat,cslist,mxc=length(cslist)){

  retdf<-data.frame(index=c("calinski","cindex","db","hartigan","ratkowsky","scott","marriot","ball","trcovw","tracew","friedman","rubin","ssi","likelihood","xuindex"))

  for (h in 2:mxc){
    retdf[,paste(h,"clust")]<-clustIndex(cslist[[h]],dat,index="all")
  }

  retdfo<-na.omit(retdf)
  retmat<-as.matrix(retdfo[,2:mxc])
  recs<-clustrecs(retdfo)


  retlist<-list(df=retdf,dfomit=retdfo,matrix=retmat,recd=recs)
  #retlist<-list(recommed,reclist,reccriteria,retdf,retdfo,retmat)
  return(retlist)
}


#' plot_data_column function
#'
#' plotting cluster fit for simple matching coefficient
#' @param data a dataframe of the raw data to be clustered
#' @param column the column containing the data to be plotted
#' @keywords plot_data_column cluster outlier inter-cluster distance fit solution
#' @export
#' @examples
#' plot_data_column_function()

plot_data_column <- function (data, column) {
  ggplot() +
    geom_line(data = data, aes(x = data[,1], y = data[,column]),na.rm = TRUE) +
    xlab('Clusters') +
    ylab('Value')+
    ggtitle(colnames(data[column]))+
    theme(plot.title = element_text(hjust = 0.5))
}


#' dist_measure_explore function
#'
#' massive function for exploratory hierachical cluster analysis varying distance measures,
#' @param data a dataframe containing only the identifier and  raw data to be clustered
#' @param cvars vector of names in order for all variables to be used in the clustering
#' @param mes vector of distance measures to use. default is distance measures from
#' @param metric metric to use in the agnes cluster analysis. default is 'euclidean'
#' @param method method of hierarchical clustering to use. default is 'ward'
#' @param diss whether dissimilarity matrix is being passed (TRUE) or raw data (FALSE). default is TRUE
#' @param maxclust maximum cluster solutions to produce. default is 15
#' @param bindata is the data being analyzed binary data (TRUE) or not (FALSE). default is FASLE
#' @keywords dist_measure_explore cluster outlier inter-cluster distance fit solution
#' @export
#' @examples
#' dist_measure_explore_function()

dist_measure_explore<-function(data,cvars,mes="rawdata",metric="euclidean",method="ward",diss=TRUE,maxclust=15,bindata=FALSE){

  x<-data
  y<-data[,cvars]

  #calculate dissimilarity matrices using different distance metrics and then save the hcl models for each of them
  mes<-ifelse(mes!="rawdata",mes,c("eskin", "good1", "good2", "good3", "good4", "iof", "lin", "lin1", "morlini", "of", "sm","jaccard","dice") )
  distances<-list()
  dist_models<-list()
  for (i in 1:length(mes)){
    distances[[i]]<-do.call(eval(parse(text=mes[i])),list(y))
    dist_models[[i]]<-agnes(distances[[i]],diss=diss,metric=metric,stand=FALSE,method=method)
  }

  eval_clusts<-list()

  for (k in 1:length(distances)){
    #run a hierarchical cluster analysis using euclidean distance and ward's method
    ag<-dist_models[[k]]

    #declare the lists of results that will capture our outputs
    centers<-list()
    hkclust<-list()
    kclust<-list()
    fitind<-matrix(nrow = maxclust, ncol = 0)
    clustname<-vector()

    #for loop to produce cluster solutions
    for (i in 1:maxclust){

      #declare what we need inside
      clustname<-c(clustname,paste("cluster", i))
      hcl<-paste("hclust", i , sep = "")
      hkcl<-paste("hkclust", i , sep = "")
      kcl<-paste("kclust", i , sep = "")

      #cut the hierarchical solution into i clusters
      x[,hcl]<-cutree(ag,k=i)

      #run a kmeans relocation using the hierarchical cluster centers
      centmat<-aggregate(y,by=list(solution = x[,hcl]),mean)
      cent<-as.matrix(centmat[,1:(length(centmat))])
      centers[[hcl]]<-cent
      hkclust[[hkcl]]<-kmeans(z,cent,iter.max = 1000)
      x[,hkcl]<-hkclust[[i]]$cluster

      #run a regular kmeans
      kclust[[kcl]]<-kmeans(z,i,iter.max = 1000)
      x[,kcl]<-kclust[[i]]$cluster

      tryCatch({
        #collect the fit indices for each kmeans relocated hclust
        indx<-clustIndex(hkclust[[i]],z,index='all')
        fitind<-cbind(fitind,indx)
        colnames(fitind)[colnames(fitind)=="indx"] <- i}
        , error=function(e){cat("ERROR :",conditionMessage(e), "\n")
        })
    }
    tryCatch({
      hmm<-cbind(x[,1:(length(cvars)+1)],x[,paste("hkclust",2:maxclust,sep = "")])
      eval_clusts[[k]]<-evalclust(hmm,length(cvars),clu_low = 2,clu_high = maxclust)
    })
  }

  return(list(x,distances,dist_models,centers,hkclust,fitind))
}


#' hc_explore function
#'
#' massive function for exploratory hierachical cluster analysis. computes hierarchical, kmeans, and hierarchical with kmeans relocation cluster analyses, adds them to the working dataset and returns other working information
#' @param data a dataframe containing only the identifier and  raw data to be clustered
#' @param mes vector of distance measures to use. default is distance measures from
#' @param cvars vector of names in order for all variables to be used in the clustering
#' @param metric metric to use in the agnes cluster analysis. default is 'euclidean'
#' @param method method of hierarchical clustering to use. default is 'ward'
#' @param diss whether dissimilarity matrix is being passed (TRUE) or raw data (FALSE). default is FALSE
#' @param maxclust maximum cluster solutions to produce. default is 15
#' @param bindata is the data being analyzed binary data (TRUE) or not (FALSE). default is FASLE
#' @keywords hc_explore cluster outlier inter-cluster distance fit solution
#' @export
#' @examples
#' hc_explore_function()

hc_explore<-function(data,cvars,mes="rawdata",metric="euclidean",method="ward",diss=FALSE,maxclust=15,bindata=FALSE){

x<-data
y<-ifelse(diss==FALSE,data[,cvars],data)

  #run a hierarchical cluster analysis using euclidean distance and ward's method
  ag<-agnes(y,diss=diss,metric=metric,stand=FALSE,method="ward")

  #declare the lists of results that will capture our outputs
  centers<-list()
  hkclust<-list()
  kclust<-list()
  fitind<-matrix(nrow = maxclust, ncol = 0)
  clustname<-vector()

  #for loop to produce cluster solutions
  for (i in 1:maxclust){

    #declare what we need inside
    clustname<-c(clustname,paste("cluster", i))
    hcl<-paste("hclust", i , sep = "")
    hkcl<-paste("hkclust", i , sep = "")
    kcl<-paste("kclust", i , sep = "")

    #cut the hierarchical solution into i clusters
    x[,hcl]<-cutree(ag,k=i)

    #run a kmeans relocation using the hierarchical cluster centers
    centmat<-aggregate(x[,cvars],by=list(solution = x[,hcl]),mean)
    cent<-as.matrix(centmat[,1:(length(centmat))])
    centers[[hcl]]<-cent
    hkclust[[hkcl]]<-kmeans(z,cent,iter.max = 1000)
    x[,hkcl]<-hkclust[[i]]$cluster

    #run a regular kmeans
    kclust[[kcl]]<-kmeans(z,i,iter.max = 1000)
    x[,kcl]<-kclust[[i]]$cluster
  }

return(list(x,distances,dist_models,centers,hkclust,kclust))
}


#' pct function
#'
#' takes a dichotomous vector (0/1) and turns it into a percent value with '%' following
#' @param x a vector of dichotomous values (0/1) in dec, num, or char format.
#' @param digits number of digits to round the percent to
#' @param format the format that the vector x is in. can be 'dec', 'num', or 'char'
#' @keywords pct percent
#' @export
#' @examples
#' pct_function()

pct <- function(x, digits = 2, format = "dec") {
  if (format == "dec"){
    val<-((sum(as.integer(as.character(x))))/(length(x)))
  }
  if (format == "num"){
    val<-((sum(as.integer(as.character(x))))/(length(x)))*100
  }
  if (format == "char"){
    y<-((sum(as.integer(as.character(x))))/(length(x)))
    val<-paste0(formatC(100 * y, format = "f", digits = digits, ...), "%")
  }
  return(val)
}


#' x2bundle function
#'
#' creates a crosstab of two variables and returns the crosstabs, the chi square value, the chi square significance, and the effect size
#' @param d1 variable 1
#' @param d2 variable 2
#' @keywords x2bundle  x2 chisquare
#' @export
#' @examples
#' x2bundle_function()

x2bundle<-function(d1,d2){
  ctabs<-table(d1,d2)
  x2vals<-chisq.test(ctabs)
  x2es<-ES.chisq.assoc(ct=ctabs,chisq=x2vals,p=x2vals$p.value,df=x2vals$parameter)
  sig<-ifelse(x2vals$p.value<.05,1,0)

  results<-list(ctabs=ctabs,x2vals=x2vals,x2es=x2es,sig=sig)
  return(results)
}


#' x2multibundle function
#'
#' creates a crosstab of two variables and returns the crosstabs, the chi square value, the chi square significance, and the effect size
#' @param indvar the single variable to check against all dependent variables
#' @param data the dataset from which indvar and depvar_vector variables come
#' @param depvar_vector vector of names of the dependent variables to check against
#' @keywords x2multibundle x2 chisquare
#' @export
#' @examples
#' x2multibundle_function()

x2multibundle<-function(indvar,data,depvar_vector=NULL){
  ctabs<-list()
  x2vals<-list()
  x2es<-list()
  sig<-vector()
  sigval<-vector()
  esval<-vector()

  for (i in 1:length(depvar_vector)){
    modname<-paste("model",i,"...",depvar_vector[i],"_BY_",indvar,sep="")
    ctabs[[modname]]<-table(data[,depvar_vector[i]],data[,indvar])
    x2vals[[modname]]<-chisq.test(ctabs[[modname]])
    x2es[[modname]]<-ES.chisq.assoc(ct=ctabs[[modname]],chisq=x2vals[[modname]],p=x2vals[[modname]]$p.value,df=x2vals[[modname]]$parameter)
    sig[modname]<-ifelse(x2vals[[modname]]$p.value<.05,1,0)
    sigval[modname]<-x2vals[[modname]]$p.value
    esval[modname]<-x2es[[modname]]$phi
  }
  results<-list(ctabs=ctabs,x2vals=x2vals,x2es=x2es,sig=sig,sigval=sigval,esval=esval)
  return(results)
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



#' pairwise_table_comp function
#'
#' creates a list of information for pairwise comparisons of categorical or binomial variables. returns the individual tables, the individual the chi square models, a matrix of the chi square values, a matrix of the chi square p values, a matrix of the effect sizes, and a matrix of the starred pvalues
#' @param x the data frame that the variables are to be pulled from
#' @param v vector of variable names. all elements in the vector will have a pairwise comparison to all others
#' @param diagonal boolean argument that returns the full symmetric matrix if FALSE and only the lower triangle if TRUE. default is TRUE
#' @keywords pairwise_table_comp x2 chisquare pairwise
#' @export
#' @examples
#' pairwise_table_comp_function()

pairwise_table_comp<-function(x=x,v=v,diagonal=TRUE){
  tables<-list()
  x2mods<-list()
  x2<-data.frame(matrix(0,ncol=length(v),nrow=length(v),dimnames = list(v,v)))
  pval<-data.frame(matrix(0,ncol=length(v),nrow=length(v),dimnames = list(v,v)))
  ES<-data.frame(matrix(0,ncol=length(v),nrow=length(v),dimnames = list(v,v)))
  pval_star<-data.frame(matrix(0,ncol=length(v),nrow=length(v),dimnames = list(v,v)))

  for (i in 1:length(v)){
    for (j in 1:length(v)){
      name<-paste(v[i],"_by_",v[j],sep="")

      tables[[name]]<-table(x[,v[i]],x[,v[j]])
      x2mods[[name]]<-chisq.test(tables[[name]])

      x2[i,j]<-round(x2mods[[name]]$statistic,2)
      pval[i,j]<-round(x2mods[[name]]$p.value,2)
      ES[i,j]<-round(ES.chisq.assoc(tables[[name]])$phi,2)
      pval_star[i,j]<-paste(pval[i,j],stars.pval(pval[i,j]),sep = "")
    }
  }
  if (diagonal==TRUE){
    x2[!lower.tri(x2,diag = FALSE)]<-""
    pval[!lower.tri(pval,diag = FALSE)]<-""
    ES[!lower.tri(ES,diag = FALSE)]<-""
    pval_star[!lower.tri(pval_star,diag = FALSE)]<-""
  }

  output<-list(tables=tables,x2mods=x2mods,x2=x2,
               pval=pval,ES=ES,pval_star=pval_star)
  return(output)
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


#' MCsmry_list function
#'
#' multiple choice summary list summarizes variables that have multiple choice options listed with a delimeter this function gives a table with the count & % of all cases containing each element in the list. this function should be used over regular mcsmry when the delimiter is essential for distinguishing the list elements as is sometimes the case with multiple choice number values.
#' @param elementlist the list of multiple choice elements in the variable to look for and count
#' @param variable the variable you wish to summarize
#' @param total boolean argument determining if a total should be added to the bottom of the table. defaults to FALSE
#' @param delimiter the delimiter you to use to separate your data into a list format
#' @keywords freqtbl frequency table MCsmry_list multiple choice
#' @export
#' @examples
#' MCsmry_list_function()

MCsmry_list<-function(elementlist,variable,total=FALSE,delimiter=", "){
  nums<-strsplit(variable,delimiter)
  retfreq<-vector()
  for (m in 1:length(elementlist)){
    present<-vector()
    for (n in 1:length(nums)){
      present<-c(present,!is.na(match(elementlist[[m]],nums[[n]])))
    }
    retfreq<-c(retfreq,sum(present))
  }
  perc<-percent((retfreq/length(nums)))
  retfreq<-as.data.frame(cbind(option=elementlist,freq=retfreq,percent=perc))
  retfreq$freq<-as.numeric(as.character(retfreq$freq))
  retfreq<-retfreq[order((retfreq$freq),decreasing = TRUE),]

  if (total==TRUE){
    freqsum<-sum(retfreq$freq)
    totrow<-c("Total",freqsum,"100%")
    retfreq<-rbind(retfreq,totrow)
  }
  return(retfreq)
}


#' inlist function
#'
#' searches a multifaceted variable (i.e. a single variable that corresponds to a series of check boxes-so boxes 1,2,6,7 checked would look like "1,2,6,7" depending on delimiter) and checks to see if the match pattern (i.e. the category) is present and returns a '1' if found and a '0' if not
#' @param srch_var the variable in which you want to search for the matching pattern/value
#' @param match_var the pattern for which you are seeking matches (e.g. if you want to know if someone checked box 1 and that corresponds to "1" in the variable then "1" would be your variable to match)
#' @param delimiter the delimiter you to use to separate each cases data into a list of vectorized values (e.g. a search variable that contains 3 cases "1,2,3", "1,3,5", and "2,3,4" would be turned into a list of 3 vectors of c("1","2","3"), c("1","3","5"), c("2","3","4") if the delimiter was set to ","). the default for this argument is ", " which corresponds to a comma delimited string with a space after the comma.
#' @keywords inlist frequency table inlist multiple choice match
#' @export
#' @examples
#' inlist_function()

inlist<-function(srch_var,match_var,delimiter=", "){
  nums<-strsplit(srch_var,delimiter)
  present<-vector()
  for (i in 1:length(nums)){
    if (sum(match(cton(nums[[i]]),cton(match_var)),na.rm = TRUE)>0){
      present<-c(present,1)}
    else{
      present<-c(present,0)
    }
  }
  return(present)
}


#' inlist_cnt function
#'
#' searches a multifaceted variable (i.e. a single variable that corresponds to a series of check boxes-so boxes 1,2,6,7 checked would look like "1,2,6,7" depending on delimiter) and checks to see if any of the vector of match patterns (i.e. the category) are present and returns the count of the number of matches in the search variable for each case
#' @param srch_var the variable in which you want to search for the matching pattern/value
#' @param match_var a vector of patterns for which you are seeking matches (e.g. if you want to know if someone checked box 1 and that corresponds to "1" in the variable then "1" would be your variable to match)
#' @param delimiter the delimiter you to use to separate each cases data into a list of vectorized values (e.g. a search variable that contains 3 cases "1,2,3", "1,3,5", and "2,3,4" would be turned into a list of 3 vectors of c("1","2","3"), c("1","3","5"), c("2","3","4") if the delimiter was set to ","). the default for this argument is ", " which corresponds to a comma delimited string with a space after the comma.
#' @param threshold a threshold value for how many counts are needed to be considered 'present' which when used in conjunction with binary return can be used to create a 'presence/absence' variable based on the amount found. this is useful when trying to determine if a case has for example greater than three of the search attributes etc.
#' @param binary_return boolean argument that determines if the raw count is returned (FALSE) or if a binary 'present/absent' varaible is returned (TRUE). binary_return with an atomic vector match_var and a threshold of 1 makes this function the same as the base 'inlist' function. the default for this argument is TRUE
#' @keywords inlist_cnt frequency table inlist multiple choice match
#' @export
#' @examples
#' inlist_cnt_function()

inlist_cnt<-function(srch_var,match_var,delimiter=", ",threshold=1,binary_return=TRUE){
  nums<-strsplit(srch_var,delimiter)
  present<-vector()
  cnt<-vector()
  for (i in 1:length(nums)){
    cnt<-c(cnt,sum(!is.na(match(cton(nums[[i]]),cton(match_var)))))
    if (sum(!is.na(match(cton(nums[[i]]),cton(match_var))))>=threshold){
      present<-c(present,1)
    }
    else{
      present<-c(present,0)
    }
  }
  if (binary_return==TRUE){
    return(present)
  }
  else{
    return(cnt)
  }
}

#' inlist2 function
#'
#' searches a multifaceted variable (i.e. a single variable that corresponds to a series of check boxes-so boxes 1,2,6,7 checked would look like "1,2,6,7" depending on delimiter) and checks to see if the match pattern (i.e. the category) is present and returns a '1' if found and a '0' if not. inlist2 differs from inlist in that it allows you to add extra arguments in the form of a list of binary vectors the same length as the search variable. these will modify the output to be '1' if the matchvar is found OR if any of the extra vectors are '1' for that case, and '0' otherwise
#' @param srch_var the variable in which you want to search for the matching pattern/value
#' @param match_var the pattern for which you are seeking matches (e.g. if you want to know if someone checked box 1 and that corresponds to "1" in the variable then "1" would be your variable to match)
#' @param delimiter the delimiter you to use to separate each cases data into a list of vectorized values (e.g. a search variable that contains 3 cases "1,2,3", "1,3,5", and "2,3,4" would be turned into a list of 3 vectors of c("1","2","3"), c("1","3","5"), c("2","3","4") if the delimiter was set to ","). the default for this argument is ", " which corresponds to a comma delimited string with a space after the comma.
#' @param extra_vecs_aslist a list of binary vectors the same length as the search variable. these will modify the output to be '1' if the matchvar is found OR if any of these extra vectors are '1' for that case, and '0' otherwise. leaving this argument as default makes this function the same as the base 'inlist' function. the default is a list of 1 vector containing only 0's thereby not modifying the output beyond what is found with matchvar.
#' @keywords inlist2 frequency table inlist multiple choice match
#' @export
#' @examples
#' inlist2_function()

inlist2<-function(srch_var,match_var,delimiter=", ",extra_vecs_aslist=list(rep(0,length(srch_var)))){
  if (typeof(srch_var)!="character"){print("srch_Var needs to be of type char")}
  nums<-strsplit(srch_var,delimiter)
  present<-vector()

  if (typeof(extra_vecs_aslist)!="list"){print("extra_vecs_aslist needs to be of type list")}
  if (sum((lengths(extra_vecs_aslist)==length(srch_var)))!=length(extra_vecs_aslist)){print("all elements of extra_vecs_aslist needs to be the same length as search_var")}
  ev<-apply(list.cbind(extra_vecs_aslist),1,sum)

  for (i in 1:length(nums)){
    if ((sum(match(cton(nums[[i]]),cton(match_var)),na.rm = TRUE)>0)|ev[i]>0){
      present<-c(present,1)}
    else{
      present<-c(present,0)
    }
  }
  return(present)
}


#' meantbl function
#'
#' creates a table from one or more binary variables with each row corresponding to a different variable and the columns representing variable name and a summary function (e.g. mean which would be proportion of reponses that were '1')
#' @param x the variables for which you would like the summary information
#' @param digs the number of digits to round the summary column to. the default is 2
#' @param ftouse which function you would like to use in the summary. the default is 'mean'
#' @param rwnames a vector of names to use as the rownames. this will most often be the list of the variable names. the default is 'norowname' which simply returns the row number as usual
#' @param rwn_asvar boolean argument determining if the rownames should become a variable in the dataframe(TRUE) or remain simply as the row names (FALSE). this is useful if you need to save the varaible names and is typically used in conjunction with rwnames. the default is FALSE
#' @param rwn_asvar_lab the name of the variable column created from rwn_asvar==TRUE. the default is 'variable'
#' @keywords meantbl frequency table multiple choice match binary
#' @export
#' @examples
#' meantbl_function()

meantbl<-function(x,digs=2,ftouse=mean,rwnames='norowname',rwn_asvar=FALSE,rwn_asvar_lab="variable"){
  meantbl<-data.frame(apply(x,2,ftouse))
  if((rwnames!='norowname')&&(typeof(rwnames)=="character")&&(length(rwnames)==nrow(meantbl))){row.names(meantbl)<-rwnames}
  meantbl[1]<-round(meantbl[1],digs)
  names(meantbl)<-as.character(substitute(ftouse))
  if(rwn_asvar==TRUE){
    setDT(meantbl, keep.rownames = TRUE)[]
    colnames(meantbl)[1]<-rwn_asvar_lab
  }
  return(meantbl)
}


#' class_by_dich function
#'
#' summarizes and returns a table (crosstab style) of a function of each class from a suite of binary variables
#' @param aggvars te binary variables to aggregate and summarize
#' @param byvar the variable to aggregate by (i.e. the classes)
#' @param funct which function you would like to use in the summary. the default is 'mean'
#' @param catname the category name to use in the table. the default is 'Class'
#' @param fieldnames a vector of the name of the variables (fields) to be summarized. the default is 'none' which uses the variable names as the fieldnames
#' @param colns boolean argument determining if the n values for the columns should also be returned (TRUE). the default is TRUE
#' @param pct boolean argument determining if the returned values for the function should be returned formatted as a percent (TRUE). the defualt for this is TRUE
#' @keywords class_by_dich frequency table multiple choice match binary
#' @export
#' @examples
#' class_by_dich_function()

class_by_dich<-function(aggvars,byvar,funct=mean,catname="Class",fieldnames="none",colns=TRUE,pct=TRUE){
  fieldnames<-ifelse(fieldnames=="none",names(aggvars),fieldnames)
  bd<-as.data.frame(levels(as.factor(byvar)))
  colnames(bd)<-catname

  for (i in 1:length(aggvars)){
    if (pct==TRUE){t<-percent(aggregate(aggvars[,i],by=list(byvar),FUN=funct)[,2])}
    else{t<-aggregate(aggvars[,i],by=list(byvar),FUN=funct)[,2]}

    bd<-cbind(bd,t)
  }
  if (colns==TRUE){colnames(bd)[2:length(bd)]<-names(aggvars)}
  else{colnames(bd)[2:length(bd)]<-1:length(aggvars)}
  return(bd)
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

#' prop_diff_detectable function
#'
#' creates a table with sample size, effect size, proportion 2 and the minimum detectable change in proportion at the given levels in order to see how the values change and to plot
#' @param from the lowest n value to check
#' @param to the highest n value to check
#' @param by the interval between the lowest and highest n values t check (e.g. from n=100 to n=500 by 50 will return a table with 9 rows)
#' @param sig the significance value to be used in the power analysis. the default for this is 0.05
#' @param pwr the expected statistical power for the power analysis. the default for this is 0.80
#' @param prop1 the stable proportion that can be estimated or expected
#' @keywords prop_diff_detectable power analysis pwr chi sqaure proportion sample size
#' @export
#' @examples
#' prop_diff_detectable_function()

prop_diff_detectable<-function(from,to,by,sig=0.05,pwr=.80,prop1){
nsize<-vector()
eslist<-vector()
proport<-vector()

for(i in seq(from=from,to=to,by=by)){
  es<-pwr.2p.test(sig.level = sig, power = pwr,n=i)$h
  nsize<-c(nsize,i)
  eslist<-c(eslist,es)
  proport<-c(proport,h.oneprop(prop1,es))
}

esdf<-as.data.frame(cbind(nsize,eslist,proport))
esdf$propdiff<-percent(x=(esdf$proport-.41))

return(esdf)
}


#' imgproc function
#'
#' searches a list of image files in the pixel ranges given and returns the difference in pixel darkness averages, dark pix proportion (pixper), x gradient, y gradient, and correlation between pixel range matrices (EXPERIMENTAL<NEEDS UPDATING)
#' @param imgfilelist the list of image files (all must be the same size and the pixel ranges must correspond to the ranges selected from the comparison image)
#' @param dfile the datafile that contains the pixel ranges and avg, pixper, grx, gry, and tmat values for the comparison (typically blank) document/image
#' @keywords imgproc image processing picture search
#' @export
#' @examples
#' imgproc_function()

imgproc<-function(imgfilelist,dfile){

  #initialize the test pixct, pixper, and avgs vectors
  # tpixct<-vector()
  # tpixper<-vector()
  # tavgs<-vector()
  # tgrx<-vector()
  # tgry<-vector()
  tmat<-list()


  for (j in 1:length(imgfilelist)){
    tgrx<-paste("tgrx_",j,sep = "")
    tgry<-paste("tgry_",j,sep = "")
    tpixct<-paste("tpixct_",j,sep = "")
    tpixper<-paste("tpixper_",j,sep = "")
    tavgs<-paste("tavgs_",j,sep = "")

    testfile<-imgfilelist[[j]]
    testfile<-testfile %>% grayscale

    for (i in 1:nrow(dfile)){
      trngview <- (Xc(testfile) %inr% c((dfile$xstart[i]),(dfile$xend[i])) & (Yc(testfile) %inr% c((dfile$ystart[i]),(dfile$yend[i]))))
      trng<-imsub(testfile,x %inr% c((dfile$xstart[i]),(dfile$xend[i])), y %inr% c((dfile$ystart[i]),(dfile$yend[i])))
      tgr<-imgradient(trng,"xy")
      #highlight(trngview)

      tmat[[i]]<-as.matrix(trng)
      dfile[i,tgrx]<-mean(tgr$x)
      dfile[i,tgry]<-mean(tgr$y)
      dfile[i,tpixct]<-sum(trng<.5)
      dfile[i,tpixper]<-(sum(trng<.5)/sum(trng))
      dfile[i,tavgs]<-mean(trng)


      dfile[i,paste("diff_avgs",j)]<-round(((dfile$avgs[i])-(dfile[i,tavgs])),digits = 3)
      dfile[i,paste("diff_pixper",j)]<-round(((dfile[i,tpixper])-(dfile$pixper[i])),digits = 3)
      dfile[i,paste("diff_grx",j)]<-round(((dfile[i,tgrx])-(dfile$grx[i])),digits = 3)
      dfile[i,paste("diff_gry",j)]<-round(((dfile[i,tgry])-(dfile$gry[i])),digits = 3)
      dfile[i,paste("diff_tmat_corr",j)]<-cor(c(mat[[i]]),c(tmat[[i]]))
    }
  }

  #lr<-glm(tp1~testdiff_pixper+testdiff_avgs+testdiff_mat+testdiff_grx+testdiff_gry,dfile,family = "binomial")
  rets<-list(dfile,testfile,tmat,lr)
  return(rets)
}
