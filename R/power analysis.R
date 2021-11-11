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
