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


#' count na's in a vector
#'
#' This function counts the number of na's in a vector.
#' @param x the vector containing the na values.
#' @param funct the function to use to count the na's, theoretically can be changed to a different function to count something other than na's
#' @keywords sum na
#' @export
#' @examples
#' sumisna_function()

sumisna<-function(x,funct="is.na"){
  ret<-apply(data.frame(lapply(x,funct)),1,sum)
  return(ret)
}



#' formula_maker function
#'
#' This function takes as it's input an outcome variable and a vector of covariates and creates a formula object with the covariates separated by '+'. Note this is only for linear models
#' @param outcome the outcome variable name. note, this should be in quotes (e.g. "outcome1")
#' @param covars the vector of covariates to collapse
#' @keywords formula_maker formula maker linear model
#' @export
#' @examples
#' formula_maker_function()
formula_maker <- function(outcome,covars){
  fml<-stats::as.formula(paste(outcome, "~", paste(covars, collapse=" + ")))
  return(fml)
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

cton<-function(x,na.change='none'){
  if(na.change!='none'){
    x[is.na(x)]<-na.change
  }
  x<-as.numeric(as.character(x))
  return(x)
}


#' nequals format function
#'
#' This function formats a number as "(n=x)". formats n equals, mimic n equals in VBA
#' @param x is the number to put into n equals format.
#' @param cap boolean, determining whether to capitalize 'n'. Default is FALSE meaning lower case 'n'.
#' @keywords n equals nequals format
#' @export
#' @examples
#' nequals_function()

nequals<-function(x,cap=FALSE){
  if(cap){
    return(paste("(N=",x,")",sep = ""))
  } else {
    return(paste("(n=",x,")",sep = ""))
  }
}


#' na to miss format function
#'
#' converts na values in vector to "Missing" or any other value. if the variable is a factor then the value is added as a factor level. if 'missing' already exists as a factor level then it simply adds more 'missing'.
#' @param x vector containing the na values
#' @param fill character or numeric value to replace na with. default is 'Missing'.
#' @param na_null string determining what type of value to replace (i.e. na, null, nan). Options are "na", "null", and "nan", any other value will replace all of these. default is "na".
#' @keywords na miss na_to_miss
#' @export
#' @examples
#' na_to_miss_function()

na_to_miss<-function(x,fill="Missing",na_null="na"){
  if(is.factor(x)&sum(is.na(x))>0&!fill %in% levels(x)){
    levels <- levels(x)
    levels[length(levels) + 1] <- as.character(fill)
    x <- factor(x, levels = levels)
  }
  if(na_null=="na"){
    x[is.na(x)]<-fill
  } else if(na_null=="null"){
    x[is.null(x)]<-fill
  } else if(na_null=="nan"){
    x[is.nan(x)]<-fill
  } else{
    x[gtools::invalid(x)]<-fill
  }
  return(x)
}


#' collapse list function
#'
#' collapses a list to a vector, useful for when a list is hiding in a dataframe and you need to convert it to a vector while collapsing vector elements like c("yes","yes,"no") to atomic elements like "yes, yes, no"
#' @param lst list to collapse
#' @param delimeter what is the lst delimited by. default is ", "
#' @keywords list collapse
#' @export
#' @examples
#' collapse_list_function()

collapse_list<-function(lst,delimeter=", "){
  return(unlist(lapply(lst,paste,collapse=", ")))
}


#' rename variables function
#'
#' easier way to rename variables over base r. give a new name and old name (or a vector of new and old names) and this function will change them in the dataset
#' @param data dataframe containing names to convert
#' @param newname list of new names that you want for the data frame, if you do not want to change the name of every column then a vector of old names the same length as this should als be supplied
#' @param oldname a vector of old names the same length as newname which will do a 1:1 match for replacement. if left blank all column names will be relpace (i.e. newname should be a vector the name length as ncol(data))
#' @keywords rename variables replace change name
#' @export
#' @examples
#' rename.variables_function()

rename.variables<-function(data,newname,oldname=NULL){
  if(is.null(oldname)){oldname<-names(data)}
  if(length(newname)!=length(oldname)){
    warning("newname and oldname must be the same length")
  } else if(sum(names(data) %in% oldname)!=length(oldname)){
    warning("one or more of the oldnames supplied does not match a variable in data. please check the vector of oldnames given for errors")
  } else{
    for(i in 1:length(newname)){
      names(data)[names(data) == oldname[i]] <- newname[i]
    }
    return(data)
  }
}



#' is checked function
#'
#' this function is for converting common output of checkbox type variables to something else (e.g. default is converting the word 'Checked' to TRUE, and 'Unchecked' to FALSE)
#' @param x vector containing a dichotomous 'checked' and 'unchecked' state
#' @param pos the positive or affirmative state. default is 'Checked' (case sensitive)
#' @param neg the negative state. default is 'Unchecked'
#' @param ret_pos what the function returns for a positive instance. default is TRUE
#' @param ret_neg what the function returns for a negative instance. default is FALSE
#' @keywords ischecked checked dichotomous binary multi multiple choice
#' @export
#' @examples
#' ischecked_function()

ischecked<-function(x,pos="Checked",neg="Unchecked",ret_pos=TRUE,ret_neg=FALSE){
  ret<-ifelse(x==pos,ret_pos,ret_neg)
  return(ret)
}


#' clear labels function
#'
#' if a dataset has labels in it then it will not join using dpylr, use this funciton to clear the labels out before merging
#' @param x data frame to remove labels from
#' @keywords remove labels clear
#' @export
#' @examples
#' clear.labels_function()

clear.labels <- function(x) {
  if(is.list(x)) {
    for(i in seq_along(x)) {
      class(x[[i]]) <- setdiff(class(x[[i]]), 'labelled')
      attr(x[[i]],"label") <- NULL
    }
  } else {
    class(x) <- setdiff(class(x), "labelled")
    attr(x, "label") <- NULL
  }
  return(x)
}


#' rangelist conversion function
#'
#' concatenates elements of a vector with options for quotes and delimiter. mimics the rangelist function that I created in vba
#' @param x is the vector containing the elements to be concatenated
#' @param quotes boolean variable determining if each element should be surrounded by quotes (TRUE). the default for this argument is TRUE
#' @param delimiter string argument of what the delimiter should be betweeen elements. the defualt for this agrument is ", "
#' @param print_only whether the output should simply be written to the console unsing the base 'cat' function or should be stored as a value for later use. Default is FALSE indicating that it should be a value for later use
#' @keywords rangelist character number
#' @export
#' @examples
#' rangelist_function()

rangelist<-function(x,quotes=TRUE,delimiter=", ",print_only=FALSE){
  if (quotes==TRUE){
    for (i in 1:length(x)){
      if (i==1){
        rnglist<-paste("\"",x[i],"\"",sep = "")
      }
      else{
        tmp<-paste("\"",x[i],"\"",sep = "")
        rnglist<-paste(rnglist,tmp,sep = delimiter)
      }
    }
  }
  else{
    for (i in 1:length(x)){
      if (i==1){
        rnglist<-x[i]
      }
      else{
        rnglist<-paste(rnglist,x[i],sep = delimiter)
      }
    }
  }
  if(print_only){
   return(cat(rnglist))
  } else{
  return(rnglist)
  }
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



#' rown_to_var function
#'
#' creates a variable from the row names, optionally makes it the first variable, optionally removes the row names, and names it what you want
#' @param df a dataframe for which you want to turn row names into a variable
#' @param keeprn boolean indicating whether you want the rownames kept as rownames after they get added as a variable. default is FALSE
#' @param makefirst boolean indicating whether you want to make the rownames the first variable in the dataframe. default is TRUE
#' @param varname the name given to the new variable containing the rownames. default is 'terms'
#' @keywords rown_to_var row name to variable
#' @export
#' @examples
#' rown_to_var_function()

rown_to_var<-function(df,keeprn=FALSE,makefirst=TRUE,varname='terms'){
  df[,varname]<-row.names(df)
  if(makefirst==TRUE){
    df<-df[,c(length(df),1:length(df)-1)]
  }
  if(keeprn==FALSE){
    row.names(df)<-NULL
  }
  return(df)
}



#' row_to_var function
#'
#' deprecated but remains for functional use. given a data frame takes the first row of the data frame and makes it the variable names and then removes the first row. this is useful when using rbind or when an imported dataset doesn't account for headers
#' @param df a dataframe for which you want to turn the first row into variable names
#' @keywords row_to_var row to variable
#' @export
#' @examples
#' row_to_var_function()

row_to_var <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}


#' firstrow_to_vars function
#'
#' given a data frame takes the first row of the data frame and makes it the variable names and then removes the first row. this is useful when using rbind or when an imported dataset doesn't account for headers. duplicative with row_to_var but more optional and less error prone
#' @param df a dataframe for which you want to turn the first row into variable names
#' @param removefirstrow boolean indicating whether to remove the first row after making it the df variable names. default is TRUE
#' @param reset_rownames booelan indicating whether to reset the row names after removing the first row. default is TRUE
#' @keywords firstrow_to_vars row to variable
#' @export
#' @examples
#' firstrow_to_vars_function()

firstrow_to_vars <- function(df,removefirstrow=TRUE,reset_rownames=TRUE) {
  df <- rename.variables(df,df[1,])
  if (removefirstrow==TRUE){df<-df[-1,]}
  if (reset_rownames==TRUE){rownames(df)<-NULL}
  return(df)
}




#' fp function
#'
#' takes a frequency and percent variable and combines them in the form of 'x (xx%)'
#' @param freq a value representing a frequency to be combined
#' @param perc a value representing a percent to be combined
#' @keywords fp frequency percent
#' @export
#' @examples
#' fp_function()

fp<-function(freq,perc){
  if(length(freq)!=length(perc)){warning("agruments must be the same length")}
  l<-length(freq)
  bs<-data.frame("freq"=freq,"perc"=perc,"l"=rep(" (",l),"r"=rep(")",l))
  ret<-unite(bs,"fp",c("freq","l","perc","r"),sep = "")
  return(ret$fp)
}



#' pval_star function
#'
#' takes a vector of pvalues and values and adds star demarcation depending on the p values given
#' @param pvals a vector of p values used to determine the demarcation to apply
#' @param starmat a vector of values to which demarcation (e.g. '*') will be applied. if this is not supplied then the demarcation will be applied to the pvalues supplied in pvals
#' @param vec_return boolean indcating whether to return a vector instaed of a data frame. default is FALSE
#' @param correction switch indicating whether a p.value correction for multiple comparisons should be applied. this uses stats::p.adjust and takes any of those types. default is 'none'
#' @param digs the number of digits to round the values to. default is 'none' indicating no rounding
#' @keywords pval_star pvalue star
#' @export
#' @examples
#' pval_star_function()
#'
pval_star<- function(pvals,starmat,vec_return=FALSE,correction='none',digs='none'){
  if(correction!='none'){pvals<-stats::p.adjust(pvals,method=correction)}
  x<-as.data.frame(pvals)
  if (missing(starmat)){
    starmat<-pvals
  }
  y<-as.data.frame(starmat)
  if (is.numeric(digs)==TRUE){
    y[,1]<-apply(y,1,sprintf,fmt=paste("%.",digs,"f",sep = ""))
  }
  for (i in 1:ncol(y)){
    for (j in 1:nrow(y)){
      y[j,i]<-ifelse(as.numeric(x[j,i])<.001,paste(y[j,i],"***",sep = ""),
              ifelse(as.numeric(x[j,i])<.01,paste(y[j,i],"**",sep = ""),
              ifelse(as.numeric(x[j,i])<.05,paste(y[j,i],"*",sep = ""),
              ifelse(as.numeric(x[j,i])<.1,paste(y[j,i],"`",sep = ""),y[j,i]))))
    }
  }
  `colnames<-`(y,colnames(starmat))
  if (vec_return==TRUE & ncol(y)==1){
    return(as.vector(y[,1]))
  }
  else{
    return(as.data.frame(y))
  }
}






#' create_studyid function
#'
#' given a seed this function creates a pseudo random ID that should be unique to a patient. the seed also must be unique to the patient but preferably not a single identifier. for example it would be better to use ssn multiplied by dob and gender rather than ssn alone. additionally row numbers can be used if they are unlikely to yield patient information
#' @param x a seed unique to a patient used for generating the pseudorandom id.
#' @keywords create_studyid studyid study id deidentify de-identify
#' @export
#' @examples
#' create_studyid_function()
#'
create_studyid<-function(x,sha=FALSE){
  if(is.na(x)){
    ret<-NA
  } else if (sha==TRUE){
    ret<-as.vector(openssl::sha256(as.character(x)))
  } else {
    set.seed(x)
    ret<-ids::random_id(1,use_openssl=FALSE)
  }
  return(ret)
}



#' n2n function
#'
#' base function used to create list_to_vecs, recommend using that instead. This function takes a list comprised only of lists and converts the list elements to vectors. list_to_vecs takes any list/dataframe and converts any elements that are lists to vectors. This is useful when using functions like pivot_wider which can often return lists of lists.
#' @param x list of lists which you would like to convert to a list of vectors. this must be all lists, if a list element is instead a vector this function will cause errors.
#' @keywords n2n list vector convert list_to_vecs pivot_wider
#' @export
#' @examples
#' n2n_function()
#'
n2n<-function(x){
  if(is.null(x)){
    x<-NA
  } else {
    x<-toString(x)
  }
  return(x)
}


#' list_to_vecs function
#'
#' wrapper for n2n this function takes a list or dataframe that may contain lists and converts the list elements to vectors. This is useful when using functions like pivot_wider which can often return lists of lists.
#' @param x list of lists which you would like to convert to a list of vectors
#' @keywords list_to_vecs list vector convert n2n pivot_wider
#' @export
#' @examples
#' list_to_vecs_function()
#'
list_to_vecs<-function(x){
  x[,unlist(lapply(x,is.list))]<-lapply(x[,unlist(lapply(x,is.list))],sapply,n2n)
  return(x)
}



#' either_both function
#'
#' Takes two variables (or two vectors of variables) and returns a vector representing the 'or' or the 'and' of the combination of two variables and adds a custom suffix
#' @param x quoted variable names (e.g. "var1") or vector of quoted variable names in the dataset to be compared to y
#' @param y quoted variable names (e.g. "var1") or vector of quoted variable names in the dataset to be compared to x. If x and y are vectors of variable names they must be the same length
#' @param data the dataset that contains the variables in x and y
#' @param either boolean determining if the function should operate by 'or' or 'and'. default is TRUE indicating that the function returns the positive value if either input variable is 'positive'. FALSE returns positive if both input variables are positive
#' @param pos the value that indicates 'positive', 'affirmative', or 'yes'. default is '1' to be used wit binary 0/1 variables
#' @param neg the value that indicates 'negative', or 'no'. default is '0' to be used wit binary 0/1 variables
#' @param addtn suffix to add to the variable names. default is '_comb' indicating 'combined'. if 'usexnames'==FALSE then addtn will be the base variable name and the iteration number will be added to it (e.g. if addtn is 'combined' then we would get combined1, combined2, etc.)
#' @param usexnames boolean determining if the x variables should be used as the base variable names for the new combined variables. default is TRUE. if FALSE addtn is used as the base and then iterated if needed
#' @param rmv text to remove from variable names if usexnames is TRUE. for example if all variable names contained '_pre_' and we wanted that removed we could put it in this argument. default is 'none' indicating no text is to be removed
#' @keywords either_both either both combine and or
#' @export
#' @examples
#' either_function()
#'
either_both<-function(x,y,data=NULL,either=TRUE,pos=1,neg=0,addtn="_comb",usexnames=TRUE,rmv='none'){
  if(length(x)!=length(y)){"x and y must be the same length"}
  ret<-data.frame(rep(NA,nrow(data)))
  if(is.null(data)){data<-data.frame("x"=x,"y"=y)}
  for(i in 1:length(x)){
    if(either){
      if(usexnames==TRUE){
        vname<-paste(x[i],addtn,sep = "")
        if(rmv!='none'){
          vname<-gsub(rmv,"",vname)
        }
        ret[,vname]<-ifelse(data[,x[i]]==pos|data[,y[i]]==pos,pos,neg)
      } else{
        ret[,paste(addtn,i,sep = "")]<-ifelse(data[,x[i]]==pos|data[,y[i]]==pos,pos,neg)
      }
    } else{
      if(usexnames==TRUE){
        vname<-paste(x[i],addtn,sep = "")
        if(rmv!='none'){
          vname<-gsub(rmv,"",vname)
        }
        ret[,vname]<-ifelse(data[,x[i]]==pos&data[,y[i]]==pos,pos,neg)
      } else{
        ret[,paste(addtn,i,sep = "")]<-ifelse(data[,x[i]]==pos&data[,y[i]]==pos,pos,neg)
      }
    }
  }
  ret<-ret[,-1]
  return(ret)
}




