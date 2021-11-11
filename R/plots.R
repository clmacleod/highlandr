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
