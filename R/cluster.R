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
