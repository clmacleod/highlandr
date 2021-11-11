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
