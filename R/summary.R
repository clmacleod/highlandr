#' freqtbl_basic function
#'
#' creates frequency tables the way I like them. with freq, valid pct, and in descending order of frequency. this function allows you to change the name of the category columns, and whether or not totals and NAs are displayed
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
    cnts<-cnts[order((cnts[,1]),decreasing = reverseorder),]
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
#' creates frequency tables the way I like them. with freq, valid pct, and in descending order of frequency. this function allows you to change the name of the category columns, and whether or not totals and NAs are displayed
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
#' creates frequency tables the way I like them. with freq, valid pct, and in descending order of frequency. this function allows you to change the name of the category columns, and whether or not totals and NAs are displayed. this funtion can also add standard binomial and multinomial confidence intervals if desired.
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
    cnts<-cnts[order((cnts[,1]),decreasing = reverseorder),]
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
    conf<-data.frame(DescTools::BinomCI(bis,bin,sides = "two.sided",method = bci))
    conf<-rownames_to_column(conf,var = name)
    conf<-conf[,-match("est",colnames(conf))]
    conf[,c("lwr.ci")]<-percent(conf[,c("lwr.ci")],digits = rnd)
    conf[,c("upr.ci")]<-percent(conf[,c("upr.ci")],digits = rnd)
    cnts<-merge(cnts,conf,by=name,all.x=TRUE,sort=FALSE)
  }
  if (mci!="none"){
    conf<-DescTools::MultinomCI(table(variable,useNA = 'always'),sides = "two.sided",method = mci)
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
#' multiple choice summary summarizes variables that have multiple choice options listed with a delimeter this function gives a table with the count & pct of all cases containing each element in the list
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


#' MCsmry list function
#'
#' this function gives a table with the count & pct of all cases containing each element in the list
#' @param elementlist the list of multiple choice elements in the variable to look for and count
#' @param variable the variable you wish to summarize
#' @param total boolean argument determining if a total should be added to the bottom of the table. defaults to FALSE
#' @param delimiter character that delimits the data. default is ", "
#' @keywords freqtbl frequency table MCsmry multiple choice list
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
  ctabs_w_props<-highlandr::fp_msd_class2(d1,d2)
  x2vals<-stats::chisq.test(ctabs)
  x2es<-powerAnalysis::ES.chisq.assoc(ct=ctabs,chisq=x2vals,p=x2vals$p.value,df=x2vals$parameter)
  sig<-ifelse(x2vals$p.value<.05,1,0)
  posthoc<-chisq.posthoc.test::chisq.posthoc.test(table(d1,d2))
  ctabs_w_props_star<-ctabs_w_props
  ctabs_w_props_star[,3:ncol(ctabs_w_props_star)]<-highlandr::pval_star(posthoc[posthoc$Value=="p values",3:ncol(ctabs_w_props_star)],ctabs_w_props_star[,3:ncol(ctabs_w_props_star)])

  results<-list(ctabs=ctabs,ctabs_w_props=ctabs_w_props,x2vals=x2vals,x2es=x2es,sig=sig,posthoc=posthoc)
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





#' MCsmry_list function
#'
#' multiple choice summary list summarizes variables that have multiple choice options listed with a delimeter this function gives a table with the count & pct of all cases containing each element in the list. this function should be used over regular mcsmry when the delimiter is essential for distinguishing the list elements as is sometimes the case with multiple choice number values.
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
#' base building function for inlist suite but recommend using inlist2 as it is a more advanced function. This function determines if the character of interest (i.e. the category) has been checked in mutiple choice questions bound as vectors of delimited numeric characters e.g. inlist(c("1, 2","2, 3, 4","0, 1","1, 9"),1) would return 1, 0, 1, 1 as it searched for '1' in a list and only the 1st, 3rd, and 4th elements contained '1'
#' @param srch_var a vector of characters each a delimited list of items. for example c("1, 2, 3", "4, 5", "1, 4, 5"). This is common output to mutliple choice questions. in the example given there would be 3 records, the first having selected options 1, 2, and 3, the second options 4 and 5, and the third options 1, 4, and 5
#' @param match_var an atomic or vector or values to match against each element. for example if you wanted to know who selected options 1, 3, and 5 then this argument would be c(1, 3, 5)
#' @param delimiter the delimiter used for the data. default is set to ", "
#' @keywords inlist multiple choice binary dichotomous in list
#' @export
#' @examples
#' inlist_function()

inlist<-function(srch_var,match_var,delimiter=", "){
  if(typeof(srch_var)=="list"){srch_var<-as.character(collapse_list(srch_var))}
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


#' inlist string function
#'
#' Works the same as inlist but for strings. It determines if the character of interest (i.e. the category) has been checked in mutiple choice questions bound as vectors of delimited string characters e.g. inlist_str(c("peas, carrots","peas, beans, apple","cheese, nuts","peas, pineapple"),"peas") would return 1, 0, 1, 1 as it searched for 'peas' in a list and only the 1st, 3rd, and 4th elements contained 'peas'. this is similar to grepl
#' @param srch_var a vector of characters each a delimited list of string items, see inlist. This is common output to mutliple choice questions.
#' @param match_var an atomic or vector or values to match against each element. for example if you wanted to know who selected options "peas" and "carrots" then this argument would be c("peas","carrots")
#' @param delimiter the delimiter used for the data. default is set to ", "
#' @keywords inlist multiple choice binary dichotomous in list string
#' @export
#' @examples
#' inlist_str_function()

inlist_str<-function(srch_var,match_var,delimiter="@@#@@",threshold=1,binary_return=TRUE){
  if(typeof(srch_var)=="list"){srch_var<-as.character(collapse_list(srch_var))}
  nums<-strsplit(srch_var,delimiter)
  present<-vector()
  cnt2<-vector()
  for (i in 1:length(nums)){
    cnt1<-vector()
    for (j in 1:length(nums[i])){
      for (k in 1:length(match_var)){
        cnt1<-c(cnt1,grepl(match_var[k],nums[[i]]))
      }
    }
    cnt2<-c(cnt2,sum(cnt1))
    if (sum(cnt1)>=threshold){
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
    return(cnt2)
  }
}




#' inlist count function
#'
#' Works the same as inlist but returns the count of the match_vars matched instead of simply '0' or '1'. e.g. inlist_cnt(c("1, 2","2, 3, 4","0, 1","1, 9"),c(2,3,4)) would return 1, 3, 0, 0
#' @param srch_var a vector of characters each a delimited list of numeric items, see inlist. This is common output to mutliple choice questions.
#' @param match_var an atomic or vector or values to match against each element. for example if you wanted to know who selected options 1, 3, and 5 then this argument would be c(1, 3, 5)
#' @param delimiter the delimiter used for the data. default is set to ", "
#' @param threshold allows the user to set a threshold amount of attributes that must be met in order to be returned. This only works with binary return=TRUE. e.g. for the above example if threshold = 2 then the binary result would be 0, 1, 0, 0. default value is 1
#' @param binary_return choose if the results returned are the counts (default) or a vector of 0/1. with threshold = 1, binary_return returns the same result as inlist. by increasing threshold the user can determine the reuqired amount of attributes to be '1' vs '0'
#' @keywords inlist multiple choice binary dichotomous in list cnt count
#' @export
#' @examples
#' inlist_cnt_function()

inlist_cnt<-function(srch_var,match_var,delimiter=", ",threshold=1,binary_return=FALSE){
  if(typeof(srch_var)=="list"){srch_var<-as.character(collapse_list(srch_var))}
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




#' inlist 2 function
#'
#' Works the same as inlist but allows for the incorporation of additional binary vectors which can change the 0/1 designation. e.g. inlist2(c("1, 2","2, 3, 4","0, 1","1, 9"),c(2),extra_vecs_aslist=list(c(1,0,0,1),c(0,0,1,0))) would return 1, 1, 1, 1 because the main search would find '2' in the 1st and 2nd elements, the first additional vector indicates that the 1st and 4th, and the second additional vector indicates the third.
#' @param srch_var a vector of characters each a delimited list of numeric items, see inlist. This is common output to mutliple choice questions.
#' @param match_var an atomic or vector or values to match against each element. for example if you wanted to know who selected options 1, 3, and 5 then this argument would be c(1, 3, 5)
#' @param delimiter the delimiter used for the data. default is set to ", "
#' @param extra_vecs_aslist a list of extra vectors that will be incorporated into the data. for example, if searching c("1, 2","2, 3, 4","0, 1","1, 9") for '2' it would restult in 1, 1, 0, 0 however, if we wanted to inforporate some other variable in an 'or' format like age over 45 saved as a binary variable then incorporating an extra vector that will turn the result to 1, if not already, for the case. each vector in the list must be the same length as the search variable.
#' @keywords inlist inlist2 multiple choice binary dichotomous in list extra vectors
#' @export
#' @examples
#' inlist2_function()

inlist2<-function(srch_var,match_var,delimiter=", ",extra_vecs_aslist=list(rep(0,length(srch_var)))){
  if(typeof(srch_var)=="list"){srch_var<-as.character(collapse_list(srch_var))}
  if(typeof(srch_var)!="character"){print("srch_Var needs to be of type char")}
  nums<-strsplit(srch_var,delimiter)
  present<-vector()

  if (typeof(extra_vecs_aslist)!="list"){print("extra_vecs_aslist needs to be of type list")}
  if (sum((lengths(extra_vecs_aslist)==length(srch_var)))!=length(extra_vecs_aslist)){print("all elements of extra_vecs_aslist needs to be the same length as search_var")}
  ev<-apply(rlist::list.cbind(extra_vecs_aslist),1,sum)

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
#' creates mean tables from binary suites
#' @param x dataset containing only binary variables
#' @param digs digits to round the result to. default is 2
#' @param ftouse function to use for calculations. default is mean
#' @param rwnames vector of row names to use. default is none
#' @param rwn_asvar boolean indicating whether to make the row names the first variable in the returned dataset. default is FALSE
#' @param rwn_asvar_lab the variable name for the row name variable. to see this rwn_asvar should be TRUE. default is 'variable'
#' @param perc boolean indicating if the results should be formatted as a percent
#' @param cnt should an extra column be added that provides the counts as well as the percentages
#' @keywords meantbl binary dichotomous dich
#' @export
#' @examples
#' meantbl_function()

meantbl<-function(x,digs=2,ftouse=mean,rwnames='norowname',rwn_asvar=FALSE,rwn_asvar_lab="variable",perc=FALSE,cnt='none'){
  meantbl<-data.frame(apply(x,2,ftouse))
  cnttble<-data.frame(apply(x,2,sum))
  colnames(cnttble)[1]<-cnt
  if(perc==TRUE){
    meantbl[,1]<-percent(meantbl[,1],digs)
  }
  else{
    meantbl[1]<-round(meantbl[1],digs)
  }
  if((rwnames!='norowname')&&(typeof(rwnames)=="character")&&(length(rwnames)==nrow(meantbl))){row.names(meantbl)<-rwnames}
  names(meantbl)<-as.character(substitute(ftouse))
  if(rwn_asvar==TRUE){
    setDT(meantbl, keep.rownames = TRUE)[]
    colnames(meantbl)[1]<-rwn_asvar_lab
  }
  if(cnt!='none'){
    meantbl<-cbind(meantbl,cnttble)
    meantbl<-meantbl[,c(1,3,2)]
  }
  return(meantbl)
}



#' class_by_dich function
#'
#' base building function for the class_by_dich suite, recommend using class_by_dich_fp. given a suite of binary variables and a class variable creates a data frame with rows as class variable elements and columns as variables and columns
#' @param aggvars a data frame of variable names for those variables that you want aggregated. these must all be 0/1 binary variables. e.g. data[],c("var1","var2")]
#' @param byvar a data frame of the class variable you want to use to split the data. the unique values of this variable will be the dataframe columns
#' @param funct function to use for calculations. default is mean
#' @param catname the name given to the first column which contains the names of the variables supplied. default is 'Class'
#' @param fieldnames vector of names to replace variable names supplied above in aggvars. if agvars = c("var1","var2","var3") and fiednames = c("do","ray","me") then the first variable in the returned dataset will be "do","ray","me"
#' @param colns boolean make the rows the same name as aggvars. default is FALSE
#' @param pct switch indicating if the results should be formatted as a percent. changing this variable to anything other than 'none' will activate the switch and rename the column to that value. default is 'none'
#' @param cnt switch indicating if the results should be formatted as a percent. changing this variable to anything other than 'none' will activate the switch and rename the column to that value. default is 'none'
#' @param rnd_digs the number of digits to which the results will be rounded
#' @keywords class_by_dich binary dichotomous dich
#' @export
#' @examples
#' class_by_dich_function()

class_by_dich<-function(aggvars,byvar,funct=mean,catname="Class",fieldnames="none",colns=FALSE,pct='none',cnt='none',rnd_digs = 2){
  fieldnames<-ifelse(fieldnames=="none",names(aggvars),fieldnames)
  bd<-as.data.frame(levels(as.factor(byvar)))
  colnames(bd)<-catname

  for (i in 1:length(aggvars)){
    if (pct!='none'){t<-percent(aggregate(aggvars[,i],by=list(byvar),FUN=funct)[,2],digits = rnd_digs)}
    else{t<-aggregate(aggvars[,i],by=list(byvar),FUN=funct)[,2]
    t<-round(t,rnd_digs)
    }
    bd<-cbind(bd,t)
  }
  if (colns==TRUE){
    colnames(bd)[2:length(bd)]<-names(aggvars)
  } else if (pct=='multi_name'){
    colnames(bd)[2:length(bd)]<-names(aggvars)
  } else if (pct=='multi_num'){
    colnames(bd)[2:length(bd)]<-1:length(aggvars)
  } else if (pct!='none'){
    colnames(bd)[length(bd)]<-pct
  } else{colnames(bd)[2:length(bd)]<-1:length(aggvars)}

  if (cnt!='none'){
    l<-ncol(bd)+1
    for (i in 1:length(aggvars)){
      bd[,l]<-aggregate(aggvars[,i],by=list(byvar),FUN=sum)[,2]
    }
    colnames(bd)[length(bd)]<-cnt
    bd<-bd[,c(1,ncol(bd),2:(ncol(bd)-1))]
  }
  return(bd)
}




#' class_by_dich_fp function
#'
#' given a suite of binary variables and a class variable creates a data frame of frequencies and percents formatted as n(pct) with rows as variables and columns as class variable elements using the function supplied
#' @param aggvars a vector of variable names for those variables that you want aggregated. these must all be 0/1 binary variables
#' @param byvar the class variable you want to use to split the data. the unique values of this variable will be the dataframe columns
#' @param data_to_use the data frame than contains the aggvars and byvar
#' @param cntfunc the function to use for the calculation outside of the parentheses. default is sum
#' @param pctfunc the function to use inside the parentheses. default is mean
#' @param catname the name given to the first column which contains the names of the variables supplied. default is 'Class'
#' @param fieldnames vector of names to replace variable names supplied above in aggvars. if agvars = c("var1","var2","var3") and fiednames = c("do","ray","me") then the first variable in the returned dataset will be "do","ray","me"
#' @param cols boolean make the rows the same name as aggvars. default is TRUE
#' @param pct_t switch indicating if the results should be formatted as a percent. changing this variable to anything other than 'none' will activate the switch and rename the column to that value. default is 'none'
#' @param cnt_t switch indicating if the results should be formatted as a percent. changing this variable to anything other than 'none' will activate the switch and rename the column to that value. default is 'none'
#' @param transpose_result transposes the result so that rows are variables and columns are class variabe values. default is TRUE
#' @param varlist_name the name given to the first column of the data frame which contains the list of variables in the aggvars binary suite. default is 'variable'
#' @param total boolean indicating if a total column should be produced. default is TRUE
#' @param digs the number of digits to which the results will be rounded
#' @keywords class_by_dich binary dichotomous dich class_by_dich_fp fp
#' @export
#' @examples
#' class_by_dich_fp_function()

class_by_dich_fp<-function(aggvars,byvar,data_to_use,cntfunc=sum,pctfunc=mean,catname="Class",fieldnames="none",cols=TRUE,pct_t='pct',cnt_t='none',transpose_result=TRUE,varlist_name="Variable",total=TRUE,digs = 2){
  cbd_cnt<-class_by_dich(data_to_use[,aggvars],data_to_use[,byvar],cnt=cnt_t,colns = cols,funct = cntfunc,catname = catname,rnd_digs = digs)
  cbd_pct<-class_by_dich(data_to_use[,aggvars],data_to_use[,byvar],cnt=cnt_t,colns = cols,funct = pctfunc,pct = pct_t,rnd_digs = digs)
  class_freq<-table(data_to_use[,byvar])
  for(i in 2:ncol(cbd_cnt)){
    cbd_cnt[,i]<-fp(cbd_cnt[,i],cbd_pct[,i])
  }
  if(transpose_result==TRUE){
    cbd_cnt<-row_to_var(rown_to_var(data.frame(t(cbd_cnt))))
    names(cbd_cnt)<-c(varlist_name,paste(rep(catname,nrow(cbd_pct)),cbd_pct[,1],"n",class_freq,sep = "_"))
    if(total==TRUE){
      cbd_cnt<-cbind(cbd_cnt,
                     "Total"=fp(
                       t(class_by_dich(data_to_use[,aggvars],rep("all",nrow(data_to_use)),cnt=cnt_t,colns = cols,funct = cntfunc,catname = catname,rnd_digs = digs)),
                       t(class_by_dich(data_to_use[,aggvars],rep("all",nrow(data_to_use)),cnt=cnt_t,colns = cols,funct = pctfunc,pct = pct_t,rnd_digs = digs)))[-1])
      names(cbd_cnt)[ncol(cbd_cnt)]<-paste("Total_n_",sum(class_freq),sep = "")
    }
  }
  cbd_cnt<-data.frame(cbd_cnt)
  if(sum(grepl("none",fieldnames))!=length(fieldnames)){
    cbd_cnt[,1]<-fieldnames
  }
  rownames(cbd_cnt)<-NULL
  return(cbd_cnt)
}



#' noclass_by_dich_fp function
#'
#' given a suite of binary variables only this function creates a data frame of frequencies and percents formatted as n(pct) with rows as variables
#' @param aggvars a vector of variable names for those variables that you want aggregated. these must all be 0/1 binary variables
#' @param data_to_use the data frame than contains the aggvars and byvar
#' @param cntfunc the function to use for the calculation outside of the parentheses. default is sum
#' @param pctfunc the function to use inside the parentheses. default is mean
#' @param catname the name given to the first column which contains the names of the variables supplied. default is 'Total'
#' @param fieldnames vector of names to replace variable names supplied above in aggvars. if agvars = c("var1","var2","var3") and fiednames = c("do","ray","me") then the first variable in the returned dataset will be "do","ray","me"
#' @param cols boolean make the rows the same name as aggvars. default is TRUE
#' @param pct switch indicating if the results should be formatted as a percent. changing this variable to anything other than 'none' will activate the switch and rename the column to that value. default is 'none'
#' @param cnt switch indicating if the results should be formatted as a percent. changing this variable to anything other than 'none' will activate the switch and rename the column to that value. default is 'none'
#' @param transpose_result transposes the result so that rows are variables and columns are class variabe values. default is TRUE
#' @param varlist_name the name given to the first column of the data frame which contains the list of variables in the aggvars binary suite. default is 'variable'
#' @param total boolean indicating if a total column should be produced. default is TRUE
#' @param digs the number of digits to which the results will be rounded
#' @keywords class_by_dich binary dichotomous dich noclass_by_dich_fp fp
#' @export
#' @examples
#' class_by_dich_fp_function()

noclass_by_dich_fp<-function(aggvars,data_to_use,cntfunc=sum,pctfunc=mean,catname="Total",fieldnames="none",cols=TRUE,pct='none',cnt='none',transpose_result=TRUE,varlist_name="Variable",total=TRUE,digs=2){
  data_to_use[,"bvar"]<-""
  ret<-class_by_dich_fp(aggvars=aggvars,
                        byvar="bvar",
                        data_to_use=data_to_use,
                        cntfunc=cntfunc,
                        pctfunc=pctfunc,
                        catname=catname,
                        fieldnames=fieldnames,
                        cols=cols,
                        pct=pct,
                        cnt=cnt,
                        transpose_result=transpose_result,
                        varlist_name=varlist_name,
                        total=total,
                        digs=digs)
  ret<-ret[,1:2]
  return(ret)
}






#' lr_model_output function
#'
#' creates a table of coefficients, std errors, z vals, pvals (with stars), ORs, and 95pct confints (only tested on LR models though should work with most others)
#' @param model the output from a logistic regression model e.g. lrmodel<-lm(outcome~vars,data,family='binomial')
#' @param staror boolean indicating whether to apply star characters to odds ratios. default is TRUE
#' @param starpval boolean indicating whether to apply star characters to p values. default is TRUE
#' @param digs the number of digits to round the values to. default is 3
#' @param knit_cis boolean indicating if the confidence intervals should be knit into an extra variable in the format "[lwr-upr]"
#' @keywords lr_model_output lm logistic odds ratio or
#' @export
#' @examples
#' lr_model_output_function()
#'
lr_model_output<-function(model,staror=TRUE,starpval=TRUE,digs=3,knit_cis=TRUE){
  coefi<-as.data.frame(summary(model)$coefficients)
  suppressMessages(coefi<-cbind(coefi,highlandr::rename.variables(data.frame(matrix(confint(model),ncol = 2)),c("2.5%","97.5%"))))
  coefi$or<-exp(coefi$Estimate)
  suppressMessages(coefi<-cbind(coefi,highlandr::rename.variables(data.frame(matrix(exp(confint(model)),ncol = 2)),c("or 2.5%","or 97.5%"))))
  coefi<-round(coefi,digs)
  if(staror==TRUE){
    coefi$or<-pval_star(coefi$`Pr(>|z|)`,coefi$or,vec_return = TRUE)
  }
  if(starpval==TRUE){
    coefi$`Pr(>|z|)`<-pval_star(coefi$`Pr(>|z|)`)[[1]]
  }
  coefi<-coefi[,c(1,5,6,2:4,7:9)]
  if(knit_cis){coefi$or_cis<-paste("[",format(coefi$'or 2.5%',nsmall=digs),"-",format(coefi$'or 97.5%',nsmall=digs),"]",sep="")}
  coefi<-rown_to_var(coefi)
  return(coefi)
}



#' gee_model_output function
#'
#' creates a table of coefficients, std errors, z vals, pvals (with stars), ORs, and 95pct confints (only tested on LR models though should work with most others)
#' @param model the output from a logistic regression model e.g. lrmodel<-lm(outcome~vars,data,family='binomial')
#' @param staror boolean indicating whether to apply star characters to odds ratios. default is TRUE
#' @param starpval boolean indicating whether to apply star characters to p values. default is TRUE
#' @param digs the number of digits to round the values to. default is 3
#' @param knit_cis boolean indicating if the confidence intervals should be knit into an extra variable in the format "[lwr-upr]"
#' @keywords lr_model_output lm logistic odds ratio or
#' @export
#' @examples
#' gee_model_output()
#'
gee_model_output<-function(model,staror=TRUE,starpval=TRUE,digs=3,knit_cis=TRUE){
  coefi<-confint.geeglm(model)
  coefi$or<-exp(coefi$Estimate)
  suppressMessages(coefi<-cbind(coefi,highlandr::rename.variables(exp(coefi[,c("lwr","upr")]),c("or 2.5%","or 97.5%"))))
  coefi<-round_if_num(coefi,digs)
  if(staror==TRUE){
    coefi$or<-pval_star(coefi$`Pr(>|W|)`,coefi$or,vec_return = TRUE)
  }
  if(starpval==TRUE){
    coefi$`Pr(>|W|)`<-pval_star(coefi$`Pr(>|W|)`)[[1]]
  }
  coefi<-coefi[,c(1,5,6,2:4,7:9)]
  if(knit_cis){coefi$or_cis<-paste("[",format(coefi$'or 2.5%',nsmall=digs),"-",format(coefi$'or 97.5%',nsmall=digs),"]",sep="")}
  coefi<-rown_to_var(coefi)
  return(coefi)
}


#' unadjusted_ors function
#'
#' Given a vector of variable names, and outcome variable, and a data frame, this function will produce the unadjusted odds ratios and model output by using the lr_model_output function and running each variable independently against the outcome
#' @param vars the vector of variable names to use for piecemeal comparison to the outcome variable
#' @param outcome the outcome variable to use in the unadjusted models. note, this should be in quotes (e.g. "outcome1")
#' @param x the dataframe containing the outcome and covariate variables
#' @param staror option passed to the lr_model_output function. Boolean indicating whether to apply star characters to odds ratios. default is TRUE
#' @param starpval option passed to the lr_model_output function. Boolean indicating whether to apply star characters to p values. default is TRUE
#' @param digs option passed to the lr_model_output function. The number of digits to round the values to. default is 3
#' @keywords unadjusted_ors unadjusted adjust lr_model_output lm logistic odds ratio or
#' @export
#' @examples
#' unadjusted_ors_function()
#'
unadjusted_ors<-function(vars,outcome,x,digs=3,staror=TRUE,starpval=TRUE){
  full_list<-list()
  full_list[["intercept"]]<-highlandr::lr_model_output(stats::glm(highlandr::formula_maker(outcome,1), data=x,family = 'binomial'),digs = digs,staror = staror,starpval = starpval)
  full_df<-full_list[["intercept"]]

  for(i in 1:length(vars)){
    full_list[[vars[i]]]<-highlandr::lr_model_output(stats::glm(highlandr::formula_maker(outcome,vars[i]), data=x,family = 'binomial'),digs = digs,staror = staror,starpval = starpval)
    full_df<-rbind(full_df,full_list[[vars[i]]][-1,])
  }

  or_ci_df<-full_df[,c("terms","or","or 2.5%","or 97.5%")]
  or_df<-full_df[,c("terms","or")]
  or_only<-full_df$or

  return(list("full_list"=full_list,"full_df"=full_df,"or_ci_df"=or_ci_df,"or_df"=or_df,"or_only"=or_only))
}



#' model_fit_compare function
#'
#' Creates a table of fit statistics for a list of models and will sort by the chosen fit statistic. lr tests are also calculated with between the model and the next model in line, this happens prior to sorting so be careful as the relevant 'next model in line' will not be what the lr test was comparing to before sorting.
#' @param model_list list of models to calculate fit statistics for and compare
#' @param models_names vector of model names that will be added into the table (makes it easier to konw which model is which)
#' @param family statistical family used for the models (e.g. 'binomial', 'gaussian' etc). currently may only handle those two
#' @param sort_by the variable that you would like the dataset sorted by. Default is 'none' which makes the lr tests relevant to the next model, if you sort by something note that the statistical tests between models will no longer be in order.
#' @param formula_inc boolean indicating if the model formula should be included as a string variable in the table. Default is FALSE as formulae are long and this can end up being a cumbersome variable
#' @keywords model_fit_compare regression model fit compare lm glm
#' @export
#' @examples
#' model_fit_compare_function()
#'
model_fit_compare<-function(models_list,models_names=NULL,family='binomial',sortby="none",formula_inc=FALSE){

  if(formula_inc && !is.null(models_names)){
    oput<-data.frame("model"=NA,"model_name"=NA,"formula"=NA,"AIC"=NA,"BIC"=NA,"deviance"=NA,"R2"=NA,"loglik_w_next_mod"=NA,"LRpval_w_next_mod"=NA)
  } else if(formula_inc){
    oput<-data.frame("model"=NA,"formula"=NA,"AIC"=NA,"BIC"=NA,"deviance"=NA,"R2"=NA,"loglik_w_next_mod"=NA,"LRpval_w_next_mod"=NA)
  } else if(!is.null(models_names)){
    oput<-data.frame("model"=NA,"model_name"=NA,"AIC"=NA,"BIC"=NA,"deviance"=NA,"R2"=NA,"loglik_w_next_mod"=NA,"LRpval_w_next_mod"=NA)
  } else {
    oput<-data.frame("model"=NA,"AIC"=NA,"BIC"=NA,"deviance"=NA,"R2"=NA,"loglik_w_next_mod"=NA,"LRpval_w_next_mod"=NA)
  }

  for(i in 1:length(models_list)){
    oput[i,"model"]<-paste("model_",i,sep="")
    if(formula_inc){oput[i,"formula"]<-Reduce(paste, deparse(models_list[[i]]$formula))}
    if(!is.null(models_names)){oput[i,"model_name"]<-models_names[i]}
    oput[i,"AIC"]<-stats::AIC(models_list[[i]])
    oput[i,"BIC"]<-stats::BIC(models_list[[i]])
    oput[i,"deviance"]<-models_list[[i]]$deviance

    if(family=="binomial"){
      oput[i,"R2"]<-DescTools::PseudoR2(models_list[[i]])
    } else if(family=="gaussian"){
      oput[i,"R2"]<-summary(models_list[[i]])$r.squared
    }

    if(i!=length(models_list)){
      oput[i,"loglik_w_next_mod"]<-paste(round(lmtest::lrtest(models_list[[i]],models_list[[i+1]])$LogLik,2),collapse=" ")
      oput[i,"LRpval_w_next_mod"]<-highlandr::pval_star(lmtest::lrtest(models_list[[i]],models_list[[i+1]])$`Pr(>Chisq)`[2],digs=3)
    }
  }
  if(sortby!="none"){
    if(!sortby %in% names(oput)){
      print("must sort by an output variable")
    } else {
      oput<-oput[order(oput[,sortby]),]
    }
  }

  return(oput)
}






#' varcompare function
#'
#' create function to create a matrix that compares all variable types with quazi correlation measures to assess multicollinearity using the following criteria. vartypes must be "dochotomous" - d , "nominal" - n, or "continuous" - c and the permutation determines what is used to compare. the following are the correlation metric and significance types used and the associated permuations. #phi, x2 (dd); #cramer's v, x2 (dn,nd,nn); #point biserial, t (dc,cd); #omega2, anova (nc,cn); #pearson or spearman (depending on normality-shapirowilks), correlation sigs (cc)
#' @param d data frame containing all variables to be compared
#' @param vars the names of all the variables in the dataset
#' @param vartypes a vector of types of the variables. types can be 'd', 'n', or 'c' which are dichotomous, nominal, and continuous respectively
#' @keywords varcompare variables correlation multicollinear collinear
#' @export
#' @examples
#' varcompare_function()
#'
varcompare<-function(d,vars,vartypes,force_pc=FALSE){
  x<-d[,vars]
  bimat<-data.frame(matrix(nrow = length(vars),ncol = length(vars),dimnames = list(vars,vars)))
  testmap<-bimat<-data.frame(matrix(nrow = length(vars),ncol = length(vars),dimnames = list(vars,vars)))

  if(nrow(x)>nrow(x[complete.cases(x),])){print("some cases have missing data, associations will be computed without the missing values")}

  for (i in 1:length(vars)){
    for (j in 1:length(vars)){
      #if both variables are dichotomous (i.e. d,d)
      if (vartypes[i]=="d"&vartypes[j]=="d"){
        bimat[i,j]<-pval_star(stats::chisq.test(d[,vars[i]],d[,vars[j]])$p.value,DescTools::Phi(d[,vars[i]],d[,vars[j]]),vec_return = TRUE,digs = 3)
        testmap[i,j]<-"Phi"
      }
      #if the variables are dichotomous and nominal or both nominal (i.e. d,n; n,d; n,n)
      if ((vartypes[i]=="d"&vartypes[j]=="n")|(vartypes[i]=="n"&vartypes[j]=="d")|(vartypes[i]=="n"&vartypes[j]=="n")){
        bimat[i,j]<-pval_star(stats::chisq.test(d[,vars[i]],d[,vars[j]])$p.value,DescTools::cramersV(d[,vars[i]],d[,vars[j]]),vec_return = TRUE,digs = 3)
        testmap[i,j]<-"Cramer's V"
      }
      #if variables are continuous and dichotomousl (i.e. c,d; d,c)
      if ((vartypes[i]=="d"&vartypes[j]=="c")|(vartypes[i]=="c"&vartypes[j]=="d")){
        bimat[i,j]<-pval_star(stats::cor.test(d[,vars[i]],d[,vars[j]])$p.value,stats::cor.test(d[,vars[i]],d[,vars[j]])$estimate,vec_return = TRUE,digs = 3)
        testmap[i,j]<-"PBS Corr"
      }
      #if variables are and nominal (i.e. n,c; c,n)
      if ((vartypes[i]=="n"&vartypes[j]=="c")|(vartypes[i]=="c"&vartypes[j]=="n")){
        if (vartypes[i]=="n"){
          amod<-stats::anova(stats::lm(d[,vars[j]]~d[,vars[i]]))
        }
        if (vartypes[i]=="c"){
          amod<-stats::anova(stats::lm(d[,vars[i]]~d[,vars[j]]))
        }
        bimat[i,j]<-pval_star(amod$`Pr(>F)`[1],effectsize::omega_sq(amod)$omegasq,vec_return = TRUE,digs = 3)
        testmap[i,j]<-"Omega Squared"
      }
      #if variables are both continuous (i.e. c,c)
      if (vartypes[i]=="c"&vartypes[j]=="c"){
        if ((stats::shapiro.test(d[,vars[i]])$p.value>.05|stats::shapiro.test(d[,vars[j]])$p.value>.05)|force_pc){
          bimat[i,j]<-pval_star(stats::cor.test(d[,vars[i]],d[,vars[j]])$p.value,stats::cor.test(d[,vars[i]],d[,vars[j]])$estimate,vec_return = TRUE,digs = 3)
          testmap[i,j]<-"Pearson Corr"
        }
        else{
          bimat[i,j]<-pval_star(stats::cor.test(d[,vars[i]],d[,vars[j]],method="spearman")$p.value,stats::cor.test(d[,vars[i]],d[,vars[j]],method="spearman")$estimate,vec_return = TRUE,digs = 3)
          testmap[i,j]<-"Spearman Rho"

        }
      }
    }
  }
  return(list("comparisons"=bimat,"test_types"=testmap))
}




#' fp_msd_class function
#'
#' base function for the fp_msd_class suite, recommend using fp_msd_class2. given an independent variable and a class variable creates a dataframe that is a cross tabulation of the two with values being 'out of' the class variable columns
#' @param indvar independent variable to be split across the class variable. usually presented in the form data$variable
#' @param classvar class variable used to split the indvar. usually presented in the form data$variable
#' @param fp boolean indicating if the requested result should be frequency(percent). Default is TRUE
#' @param funct1 the first function to use (i.e. outside the parentheses) if fp='FALSE'. default is 'mean'
#' @param funct2 the second function to use (i.e. inside the parentheses) if fp='FALSE'. default is 'sd'
#' @keywords fp_msd_class variables crosstab cross summary
#' @export
#' @examples
#' fp_msd_class_function()
#'
fp_msd_class<-function(indvar,classvar,fp=TRUE,funct1='mean',funct2='sd'){
  nanum<-sum(is.na(indvar))
  if(fp==TRUE){
    init_table<-as.data.frame.matrix(table(indvar,classvar))
    tsums<-apply(init_table,2,sum)
    for(i in 1:ncol(init_table)){
      init_table[,i]<-fp(init_table[,i],percent(init_table[,i]/tsums[i]))
    }
  }
  if(fp==FALSE){
    init_table<-round(aggregate(indvar,by=list(classvar),funct1,na.rm=TRUE),2)
    sd_table<-round(aggregate(indvar,by=list(classvar),funct2,na.rm=TRUE),2)
    init_table[,2]<-fp(init_table[,2],sd_table[,2])
    row.names(init_table)<-init_table[,1]
    init_table<-t(init_table)
    init_table<-init_table[2,]

  }
  if(nanum>0){print(paste(nanum,"cases were removed due to NA values"))}
  return(init_table)
}




#' fp_msd_class2 function
#'
#' given an independent variable and a class variable creates a dataframe that is a cross tabulation of the two with values being 'out of' the class variable columns
#' @param indvar independent variable to be split across the class variable. usually presented in the form data$variable
#' @param classvar class variable used to split the indvar. usually presented in the form data$variable. if left blank (i.e. no class) then the simple frequencies will be returned without any splitting
#' @param fp boolean indicating if the requested result should be frequency(percent). Default is TRUE
#' @param funct1 the first function to use (i.e. outside the parentheses) if fp='FALSE'. default is 'mean'. if supplying different functions be sure to quote e.g. "IQR"
#' @param funct2 the second function to use (i.e. inside the parentheses) if fp='FALSE'. default is 'sd'. if supplying different functions be sure to quote e.g. "IQR"
#' @param shownval boolean indicating if the n value used after na removal should be displayed in non fp cases. default is TRUE
#' @param total boolean indicating if a 'total' column should be added to the data frame. default is TRUE
#' @param rnd_digs the number of digits to round results to. default is 2
#' @param rownvar name of the first column which contains either indvar categories or function names. default is 'Class'
#' @param count_miss switch determining if NA values should be added to the indvar frequencies as it's own category. if this is 'ifmiss' then missing will be added. if this is 'none' then missing will be excluded and a message will show number of records removed. default is 'ifmiss'
#' @param count_miss_lab if count_miss='ifmiss' meaning we want NA values included this argument determines what they are labeled as. default is 'Missing'
#' @keywords fp_msd_class2 variables crosstab cross summary
#' @export
#' @examples
#' fp_msd_class2_function()
#'
fp_msd_class2<-function(indvar,classvar=NULL,fp=TRUE,funct1='mean',funct2='sd',shownval=TRUE,total=TRUE,rnd_digs=2,rownvar='Class',count_miss="ifmiss",count_miss_lab="Missing"){
  ind<-0
  if(is.null(classvar)){
    classvar<-rep("temp",length(indvar))
    ind<-1
  }
  if(count_miss!="none"&is.factor(indvar)&fp==TRUE){
    if((count_miss=="ifmiss"&sum(is.na(indvar))>0)|count_miss!="ifmiss"){
      indvar<-factor(indvar, levels = c(levels(indvar), NA), labels = c(levels(indvar), count_miss_lab), exclude = NULL)
    }
  } else if(count_miss!='none'&fp==TRUE){
    if((count_miss=="ifmiss"&sum(is.na(indvar))>0)|count_miss!="ifmiss"){
      indvar[is.na(indvar)]<-count_miss_lab
    }
  }
  nanum<-sum(is.na(indvar))
  if(fp==TRUE){
    init_table<-as.data.frame.matrix(table(indvar,classvar))
    tsums<-apply(init_table,2,sum)
    totes<-fp(apply(init_table,1,sum),
              percent(apply(init_table,1,sum)/sum(tsums)))
    for(i in 1:ncol(init_table)){
      init_table[,i]<-fp(init_table[,i],percent(init_table[,i]/tsums[i]))
    }
    if(total==TRUE){
      init_table[,"Total"]<-totes
      col_idx <- grep("Total", names(init_table))
      init_table <- init_table[,c(col_idx, (1:ncol(init_table))[-col_idx])]
    }
  }
  if(fp==FALSE){
    init_table<-stats::aggregate(indvar,by=list(classvar),funct1,na.rm=TRUE)
    sd_table<-stats::aggregate(indvar,by=list(classvar),funct2,na.rm=TRUE)
    init_table[,2]<-format(round(init_table[,2],rnd_digs),nsmall=rnd_digs)
    sd_table[,2]<-format(round(sd_table[,2],rnd_digs),nsmall=rnd_digs)
    init_table[,2]<-fp(init_table[,2],sd_table[,2])
    row.names(init_table)<-init_table[,1]
    init_table<-row_to_var(data.frame(t(init_table)))
    if(total==TRUE){
      init_table<-as.data.frame(init_table)
      init_table[,"Total"]<-fp(format(round(do.call(funct1,list(indvar,na.rm=TRUE)),rnd_digs),nsmall=rnd_digs),
                               format(round(do.call(funct2,list(indvar,na.rm=TRUE)),rnd_digs),nsmall=rnd_digs))
      col_idx <- grep("Total", names(init_table))
      init_table <- init_table[,c(col_idx, (1:length(init_table))[-col_idx])]
    }
    init_table<-as.data.frame(init_table)
    if(shownval==TRUE){
      row.names(init_table)<-paste(highlandr::nequals(length(indvar)-highlandr::sumisna(indvar))," ",funct1,"(",funct2,")",sep = "")
    } else{
      row.names(init_table)<-paste(funct1,"(",funct2,")",sep = "")
    }
  }
  if(nanum>0){print(paste(nanum,"cases were removed due to NA values"))}
  if(rownvar!='none'){init_table<-rown_to_var(init_table,varname = rownvar)}
  if(ind==1){init_table<-init_table[,1:2]}
  if(fp==FALSE){init_table[1,]<-gsub("  NA| NaN|NA",format(round(0,rnd_digs),nsmall=rnd_digs),init_table[1,])}
  return(init_table)
}




#' fp_msd_class2_msdmulti function
#'
#' This function is a wrapper for fp_msd_class2 when there are multiple variables to analyze using functions outside of fp. Given a data frame and a vector of variable names this function will apply fp_msd_class2 to each one and rbind the results into a dataframe
#' @param x dataframe containing the data to be summarized
#' @param vars vector of variable names for the varaibles that are t be summarized. all variables must be numeric and present in the data frame
#' @param col1names vector of names to change the rows in column 1 to. This is typically what you want the individual variables that the rows represent called in the table. this must be the same length as vars. default is NULL which simply returns vars.
#' @param varnames vector of names to switch the first and second columns of the dataframe to. default is "Variables" and "Function(s)" respectively
#' @param classvar optional class variable to cross with the independent variables. this is is typically presented as a slice (e.g. data$variable)
#' @param funct1 the first function to use (i.e. outside the parentheses) if fp='FALSE'. default is 'mean'. if supplying different functions be sure to quote e.g. "IQR"
#' @param funct2 the second function to use (i.e. inside the parentheses) if fp='FALSE'. default is 'sd'. if supplying different functions be sure to quote e.g. "IQR"
#' @param shownval boolean indicating if the n value used after na removal should be displayed in non fp cases. default is TRUE
#' @param sepnval boolean indicating if the n value created using shownval should appear as a separate column in numeric form. default is TRUE meaning it will show up as a separate column. NOTE: if 'smashfirstcols' is set to TRUE this argument will automatically be changed to FALSE
#' @param total boolean indicating if a 'total' column should be added to the data frame. default is TRUE
#' @param smashfirstcols boolean indicating if the first columns (i.e. 'Variables', 'Function(s)) should be combined into a single first column. Default is FALSE meaning they will not be combined. NOTE: if this is TRUE sepnval will be set to FALSE
#' @param rnd_digs the number of digits to round results to. default is 2
#' @keywords fp_msd_class2_msdmulti variables fp_msd_class2 multiple multi class fp_msd summary
#' @export
#' @examples
#' fp_msd_class2_msdmulti_function()
#'
fp_msd_class2_msdmulti<-function(x,vars,col1names=NULL,varnames=c("Variables","Function(s)"),classvar=NULL,funct1='mean',funct2='sd',shownval=TRUE,sepnval=TRUE,total=TRUE,smashfirstcols=FALSE,rnd_digs=2){
  if(smashfirstcols){sepnval=FALSE}
  xapply<-lapply(x[,vars],highlandr::fp_msd_class2,fp=FALSE,shownval=shownval,funct1=funct1,funct2=funct2,classvar=classvar,total=total,rnd_digs=rnd_digs)
  databind<-rown_to_var(do.call(rbind,xapply))
  oldnames<-names(databind)
  newnames<-c(varnames,names(databind)[!names(databind) %in% c("terms","Class")])
  databind<-highlandr::rename.variables(databind,newname = c(varnames,names(databind)[!names(databind) %in% c("terms","Class")]))
  if(sum(grepl("=",databind[,2]))>0&sepnval){
    databind<-tidyr::separate(databind,2,into=c("N","Function(s)")," ")
    databind[,"N"]<-as.numeric(gsub("\\(n=|\\)","",databind[,"N"]))
  }
  if(!is.null(col1names)){
    databind[,1]<-col1names
  }
  if(smashfirstcols){
    databind<-cbind(data.frame("Variables"=apply(databind[,1:2],1,paste,collapse=" ")),
                    databind[3:length(names(databind))])
  }
  return(databind)
}







#' fp_msd_class2_fp_msd_multi function
#'
#' This function is a wrapper for fp_msd_class2 when there are multiple variables to analyze using both fp and non-fp functions. This function will give a rough cut table 1. Given a data frame and a vector of variable names this function will apply fp_msd_class2 to each one and rbind the results into a dataframe. Additionally to adjust other parameters you can pass arguments to fp_msd_class2 as either single values (e.g. total) or as a vector the same length as the number of variables being analyzed (e.g. fp_msd_vector_tf)
#' @param data data frame containing the data to be summarized
#' @param indvars vector of variable names for the variables that are t be summarized. all variables must be present in the data frame
#' @param classvar optional class variable to cross with the independent variables. this is is typically presented as a text (e.g. data$variable)vector of names to change the rows in column 1 to. This is typically what you want the individual variables that the rows represent called in the table. this must be the same length as vars. default is NULL which simply returns vars.
#' @param fp_msd_vector_tf vector of boolean values indicating whether the variable should be 'fp' (true) or custom/msd (false). default is true
#' @param funct1_vector a vector of the first function to use (i.e. outside the parentheses) if fp='FALSE'. default is 'mean'. if supplying different functions be sure to quote e.g. "IQR"
#' @param funct2_vector a vector of the second function to use (i.e. inside the parentheses) if fp='FALSE'. default is 'sd'. if supplying different functions be sure to quote e.g. "IQR"
#' @param var_name_vec a vector of variable names to use in the table. If left as null the table will contain the variable names as listed in the data frame
#' @param sep_var_level boolean value indicating whether the variable name should be added to the table (TRUE) or not (FALSE). default is TRUE
#' @param remove_var_dups boolean indicating whether duplicate values of the variable name should be removed (TRUE) or included (FALSE). default is TRUE
#' @param shownval boolean indicating if the n value used after na removal should be displayed in non fp cases. this will only appear when fp=FASLE. default is TRUE
#' @param total boolean indicating if a 'total' column should be added to the data frame. default is TRUE
#' @param rnd_digs_vector the number of digits to round results to when fp=FALSE. default is 2
#' @param rownvar name of the first column (second column if sep_var_level=TRUE) which contains either indvar categories or function names. default is 'Level'
#' @param count_miss switch determining if NA values should be added to the indvar frequencies as it's own category. if this is 'ifmiss' then missing will be added. if this is 'none' then missing will be excluded and a message will show number of records removed. default is 'ifmiss'
#' @param count_miss_lab if count_miss='ifmiss' meaning we want NA values included this argument determines what they are labeled as. default is 'Missing'
#' @keywords fp_msd_class2_msdmulti variables fp_msd_class2 multiple multi class fp_msd summary table1 table2
#' @export
#' @examples
#' fp_msd_class2_fp_msd_multi_function()
#'
fp_msd_class2_fp_msd_multi<-function(data,indvars,classvar=NULL,fp_msd_vector_tf=NULL,funct1_vector=NULL,funct2_vector=NULL,var_name_vec=NULL,sep_var_level=TRUE,remove_var_dups=TRUE,shownval=TRUE,total=TRUE,rnd_digs_vector=NULL,rownvar = "Level",count_miss = "ifmiss",count_miss_lab = "Missing"){
  vnv_null<-is.null(var_name_vec)
  
  if(!is.null(classvar)){classvar<-data[,classvar]}
  if(is.null(rnd_digs_vector)){rnd_digs_vector<-rep(2,length(indvars))}
  if(is.null(funct1_vector)){funct1_vector<-rep("mean",length(indvars))}
  if(is.null(funct2_vector)){funct2_vector<-rep("sd",length(indvars))}
  if(is.null(fp_msd_vector_tf)){fp_msd_vector_tf<-ifelse(unlist(lapply(data[,indvars],typeof))=="double",FALSE,TRUE)}

  #print(fp_msd_vector_tf)

  for(i in 1:length(indvars)){

    tbl<-fp_msd_class2(data[,indvars[i]],
                       #classvar = data[,classvar],
                       classvar = classvar,
                       fp = fp_msd_vector_tf[i],
                       funct1 = funct1_vector[i],
                       funct2 = funct2_vector[i],
                       shownval = shownval,
                       total = total,
                       rnd_digs = rnd_digs_vector[i],
                       rownvar = rownvar,
                       count_miss = count_miss,
                       count_miss_lab = count_miss_lab
    )

    if(sep_var_level){
      if(vnv_null){

        var_name<-rep(indvars[i],nrow(tbl))

      } else {

        var_name<-rep(var_name_vec[i],nrow(tbl))

      }

      if(remove_var_dups&length(var_name)>1){
        var_name[2:length(var_name)]<-NA
      }

      tbl<-cbind("Variable"=var_name,tbl)
    }


    #print(tbl)
    if(i==1){
      res<-tbl
    } else {
      res<-rbind(res,tbl)
    }
  }
  return(res)
}









#' sig_val_auto function
#'
#' significance values creator with adjustment option testtypes and crossvar type can take the form (b,f,c) for binary, factor, or continuous
#' @param variables a vector of variable names for variables that you want to cross
#' @param crossvar the variable to cross against all in the 'variables' argument
#' @param data the data frame containing the variables in 'variables' and 'crossvar'
#' @param crossvartype the type of the cross variable. types can be 'b', 'f' which are binary, and factor respectively
#' @param testtypes a vector of types of the variables. types can be 'b', 'f', or 'c' which are binary, factor, and continuous respectively
#' @param reporttest boolean indicating if the type of test run should be added as a variable, default is yes
#' @param correction_type type of p value correction to apply for multiple comparisons. default is 'none'. this uses the p.adjust from stats package so any adjustment name there will work here
#' @param sig the significance value to use. default is .05
#' @param stars boolean indicating whether to add stars to significance values. default is TRUE
#' @param digs number of digits to round values to. default is 3
#' @param force_fisher boolean indicating if fisher's exact test should be used with two binary variables even if other assumptions are violated (e.g. <20pct expected values below 5)
#' @param force_anova boolean indicating if anova should be used with a factor and continuous variable even normality (shapiro wilk) is violated
#' @param force_t boolean indicating if t test should be used with a binary and continuous variable even normality (shapiro wilk) is violated
#' @keywords sig_val_auto significance pvalue p value crosstab
#' @export
#' @examples
#' sig_val_auto()
#'
sig_val_auto<-function(variables,crossvar,data,crossvartype,testtypes,reporttest=TRUE,correction_type='none',sig=.05,stars=TRUE,digs=3,force_fisher=FALSE,force_anova=FALSE,force_t=FALSE){
  if(reporttest){
    sigtable<-data.frame("Variables"=variables,"Unadjusted Sigs"=NA,"Test Type"=NA)
  } else{
    sigtable<-data.frame("Variables"=variables,"Unadjusted Sigs"=NA)
  }

  for(i in 1:length(variables)){

    if(crossvartype=="b"){
      if(testtypes[i]=="b"){
        # when to run fishers https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5426219/#:~:text=The%20chi%2Dsquared%20test%20applies,especially%20for%20small%2Dsized%20samples.
        # sample size and 2x2 http://www.biostathandbook.com/fishers.html#:~:text=Fisher's%20exact%20test%20is%20more,test%20for%20larger%20sample%20sizes.
        prex2<-stats::chisq.test(data[,variables[i]],data[,crossvar])

        if((sum(prex2$observed)<1000&
            nrow(prex2$observed)==2&
            ncol(prex2$observed)==2&
            (sum(prex2$expected<5)/(ncol(ttt$observed)*nrow(ttt$observed))<.2))
           |force_fisher){
          sigtable[i,2]<-stats::fisher.test(data[,variables[i]],data[,crossvar])$p.value
          sigtable[i,3]<-"Fisher's Exact"

        } else{
          sigtable[i,2]<-prex2$p.value
          sigtable[i,3]<-"Chi Square"
        }
      } else if(testtypes[i]=="f"){
        sigtable[i,2]<-stats::chisq.test(data[,variables[i]],data[,crossvar])$p.value
        sigtable[i,3]<-"Chi Square"

      } else if(stats::shapiro.test(data[,variables[i]])$p.value>sig|force_t){
        sigtable[i,2]<-stats::t.test(data[,variables[i]]~data[,crossvar])$p.value
        sigtable[i,3]<-"Student's T"

      } else{
        sigtable[i,2]<-stats::wilcox.test(data[,variables[i]]~data[,crossvar])$p.value
        sigtable[i,3]<-"Wilcoxon Rank Sum"
      }

    } else if(crossvartype=="f"){
      if(testtypes[i]=="b"|testtypes[i]=="f"){
        sigtable[i,2]<-stats::chisq.test(data[,variables[i]],data[,crossvar])$p.value
        sigtable[i,3]<-"Chi Square"

      } else if(stats::shapiro.test(data[,variables[i]])$p.value>sig|force_anova){
        sigtable[i,2]<-stats::anova(lm(data[,variables[i]]~data[,crossvar]))$`Pr(>F)`[1]
        sigtable[i,3]<-"ANOVA"

      } else{
        sigtable[i,2]<-stats::kruskal.test(data[,variables[i]],data[,crossvar])$p.value
        sigtable[i,3]<-"Kruskal Wallis"

      }
    }
    #else{sigtable[i,2]<-"something's up"}
  }

  if(correction_type!='none'){
    sigtable[,"Adjusted Sigs"]<-stats::p.adjust(sigtable[,2],method = correction_type)
    if(reporttest){sigtable<-sigtable[,c(1,3,2,4)]}
  } else{
    sigtable<-sigtable[,c(1,3,2)]
  }

  sigtable[,3:ncol(sigtable)]<-round(sigtable[,3:ncol(sigtable)],digs)

  if(stars==TRUE){
    sigtable[,3:ncol(sigtable)]<-pval_star(sigtable[,3:ncol(sigtable)])
  }

  return(sigtable)

}






