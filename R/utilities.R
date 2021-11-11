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
