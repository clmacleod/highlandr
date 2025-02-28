


#' en_kfold_accuracy function
#'
#' This function calculates the cross validated accuracy of an enr model
#' @param ddata data frame containing the data to be modeled
#' @param response_var string identifying the name of the outcome variable
#' @param mod_alpha the alpha(s) value(s) to be checked. alpha is the ENR blending parameter that governs how much ridge regression (0) and lasso regression (1) will be used.
#' @param mod_lambda the lambda(s) to be checked. lambda is the ENR penalty parameter for the ridge portion of the ENR
#' @param iter the number of iterations to use
#' @param k the number of folds to use
#' @param seed the seed value for allowing results to be reproduced
#' @param loo boolean indicating whether 'leave one out' cross validation should be used
#' @param eq_wt boolean indicating whether the 0/1 classes should be balanced with weights. you may want to use this if there is a bad class imbalance
#' @param type_meas the 'type measure' which is passed to cv.glmnet that governs its training penalty when tuning lambda. this should match arguments expected in cv.glmnet
#' @param lr_cutoff vetor of cutoff values to test/tune for optimization. the default is 'c(.5)' which is to say 'equal distance from all classes' which is typical in standard analyses
#' @keywords en_kfold_accuracy enr
#' @export
#' @examples
#' en_kfold_accuracy()

en_kfold_accuracy<-function(ddata,response_var,mod_alpha,mod_lambda=NULL,iter=100,k=10,seed=123,loo=FALSE,eq_wt=FALSE,type_meas = "deviance",lr_cutoff=c(.5)){

  print("cutoff")
  print(lr_cutoff)

  kfoldcv_results_data<-data.frame()

  set.seed(seed)
  itern<-1

  ac_model_form <- as.formula(paste(response_var, "~", paste(names(ddata[,!names(ddata) %in% c(response_var)]), collapse=" + ")))

  loo_tracker<-vector()

  if(loo==TRUE){
    k<-nrow(ddata)
    iter<-1
  }

  pbar <- create_progress_bar('text')
  pbar$init(iter*k)

  for(i in 1:iter){
    if(loo==FALSE){
      ddata<-ddata[order(runif(nrow(ddata))),]
    }
    ddata$grp<-rep(1:k,length.out=nrow(ddata))

    for(j in 1:k)
    {
      itername<-paste("cv_",i,"_reffold_",j,sep = "")

      if(loo==TRUE){
        train<-ddata[-j,-ncol(ddata)]
        test<-ddata[j,-ncol(ddata)]
      } else {
        train<-ddata[ddata$grp!=k,-ncol(ddata)]
        test<-ddata[ddata$grp==k,-ncol(ddata)]
      }

      wt<-NULL
      if(eq_wt==TRUE){
        wt<-ifelse(train[,1]==0,
                   1-(sum(train[,1]==0)/nrow(train)),
                   1-(sum(train[,1]==1)/nrow(train)))
      }

      train_pred <- model.matrix(ac_model_form,train)[,-1]
      train_resp <- train[,response_var]

      test_pred <- model.matrix(ac_model_form,test)[,-1]
      if(loo==TRUE){
        test_pred<-t(test_pred)
      }
      test_resp <- test[,response_var]

      # Fitting
      if(is.null(mod_lambda)){
        model <- cv.glmnet(x = train_pred,
                           y = train_resp,
                           weights = wt,
                           type.measure=type_meas,
                           alpha=mod_alpha,
                           family="binomial")
      } else {
        model <- glmnet(x = train_pred,
                        y = train_resp,
                        weights = wt,
                        type.measure=type_meas,
                        alpha=mod_alpha,
                        lambda = mod_alpha,
                        family="binomial")
      }




      # print("alpha")
      # print(mod_alpha)
      # print("cutoff")
      # print(lr_cutoff)
      # print("test response")
      # print(length(test_resp))
      # print("test predict")
      # print(nrow(test_pred))

      # Predict results
      # results_pred <- predict(model,newx=test_pred,type="class",s=model$lambda.1se)

      results_pred_prob <- predict(model,newx=test_pred,type="response",s=model$lambda.1se)

      #print(results_pred_prob)
      #print(nrow(results_pred_prob))

      #print(results_pred_prob)
      #print(results_pred_prob>lr_cutoff)

      results_pred <- ifelse((cton(predict(model,newx=test_pred,type="response",s=model$lambda.1se))>lr_cutoff),1,0)

      #print(results_pred)
      #print(nrow(results_pred))

      results_pred<-factor(results_pred,levels = c("1","0"))
      test_resp<-factor(test_resp,levels = c("1","0"))


      #print(cbind(results_pred,test_resp,(results_pred_prob>lr_cutoff),lr_cutoff))

      # Confusion matrix
      cm <- caret::confusionMatrix(data = results_pred,
                                   reference = test_resp,
                                   positive = "1")

      #AUC
      mod_auc<-suppressMessages(roc(test_resp,cton(results_pred_prob))$auc)

      # Collecting results
      cm_results<-t(data.frame(c("iter_fold"=itername,cm$overall,cm$byClass,"AUC"=mod_auc)))

      #rbind the results together
      kfoldcv_results_data<-rbind(kfoldcv_results_data,cm_results)

      itern<-itern+1

      pbar$step()

    }
  }
  # Average accuracy of the model
  rownames(kfoldcv_results_data)<-1:nrow(kfoldcv_results_data)
  kfoldcv_results<-colMeans(apply(kfoldcv_results_data[,2:ncol(kfoldcv_results_data)],2,cton),na.rm = TRUE)

  kfold_results<-list("kfold_agg_results"=kfoldcv_results,"kfold_results"=kfoldcv_results_data)

  return(kfold_results)

}



#' en_kfold_accuracy_tied function
#'
#' This function breaks ties if model performance for enr models is tied (DOCUMENTATION COMING)
#' @param tied_alphas a vector of the alphas that are tied
#' @param tied_cutoffs a vector of the cutoffs that are tied
#' @param ddata data frame containing the data to be modeled
#' @param response_var string identifying the name of the outcome variable
#' @param iter the number of iterations to use
#' @param k the number of folds to use
#' @param seed the seed value for allowing results to be reproduced
#' @param favor_parsimony boolean indicating if a simpler model should be favored
#' @param eq_wt boolean indicating whether the 0/1 classes should be balanced with weights. you may want to use this if there is a bad class imbalance
#' @param type_meas the 'type measure' which is passed to cv.glmnet that governs its training penalty when tuning lambda. this should match arguments expected in cv.glmnet
#' @keywords en_kfold_accuracy_tied enr tied
#' @export
#' @examples en_kfold_accuracy_tied()

en_kfold_accuracy_tied<-function(tied_alphas,tied_cutoffs,ddata,response_var,iter=10,k=10,favor_parsimony=TRUE,eq_wt=FALSE,seed=123,type_meas="deviance"){
  set.seed(seed)
  bestmods_accuracy_stats<-data.frame()

  #print(ddata)
  #print(response_var)

  for (j in 1:length(tied_alphas)){

    # print("tc")
    # print(tied_cutoffs)
    # print("tcj")
    # print(tied_cutoffs[j])
    # print("tcjj")
    # print(tied_cutoffs[[j]])

    bestmods_accuracy_stats_results<-en_kfold_accuracy(ddata = ddata,
                                                       response_var = response_var,
                                                       mod_alpha =  tied_alphas[j],
                                                       lr_cutoff= tied_cutoffs[j],
                                                       iter = iter,
                                                       k = k,
                                                       eq_wt = eq_wt,
                                                       type_meas=type_meas,
                                                       accuracy_modeling=TRUE)
    #print(j)
    #print(bestmods_accuracy_stats_results)
    if(j==1){
      bestmods_accuracy_stats<-rown_to_var(data.frame("Value"=bestmods_accuracy_stats_results$kfold_agg_results),varname = "statistic")
    } else {
      bestmods_accuracy_stats<-cbind(bestmods_accuracy_stats,data.frame(bestmods_accuracy_stats_results$kfold_agg_results))
    }

  }

  names(bestmods_accuracy_stats)<-c("metric",as.character(tied_alphas))

  bestmods_accuracy_stats_lim<-subset.data.frame(bestmods_accuracy_stats,metric!="AccuracyLower"&
                                                   metric!="AccuracyUpper"&
                                                   metric!="AccuracyNull"&
                                                   metric!="AccuracyPValue"&
                                                   metric!="McnemarPValue")

  if(favor_parsimony==TRUE){
    bestmods_accuracy_stats_lim<-bestmods_accuracy_stats_lim[,c(1,rev(2:ncol(bestmods_accuracy_stats_lim)))]
  }

  choice<-function(x){
    names(which.max(x))
  }

  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }

  bestmods_accuracy_stats_lim$best_alpha<-apply(bestmods_accuracy_stats_lim[,2:ncol(bestmods_accuracy_stats_lim)],
                                                1,
                                                choice)

  best_alpha<-getmode(bestmods_accuracy_stats_lim$best_alpha)
  best_alpha_value<-cton(getmode(bestmods_accuracy_stats_lim$best_alpha))

  return(list("best_alpha_value"=best_alpha_value,
              "best_alpha"=best_alpha,
              "limited results"=bestmods_accuracy_stats_lim,
              "all_results"=bestmods_accuracy_stats))
}








#' en_kfold_accuracy_tied_grid function
#'
#' This function breaks ties if model performance for grid search enr models is tied (DOCUMENTATION COMING)
#' @param tied_alphas a vector of the alphas that are tied
#' @param tied_cutoffs a vector of the cutoffs that are tied
#' @param ddata data frame containing the data to be modeled
#' @param response_var string identifying the name of the outcome variable
#' @param iter the number of iterations to use
#' @param k the number of folds to use
#' @param seed the seed value for allowing results to be reproduced
#' @param favor_parsimony boolean indicating if a simpler model should be favored
#' @param eq_wt boolean indicating whether the 0/1 classes should be balanced with weights. you may want to use this if there is a bad class imbalance
#' @param type_meas the 'type measure' which is passed to cv.glmnet that governs its training penalty when tuning lambda. this should match arguments expected in cv.glmnet
#' @keywords en_kfold_accuracy_tied_grid tied grid enr
#' @export
#' @examples
#' en_kfold_accuracy_tied_grid()

en_kfold_accuracy_tied_grid<-function(tied_alphas,tied_lambdas,tied_cutoffs,ddata,response_var,iter=100,k=10,favor_parsimony=TRUE,eq_wt=FALSE,seed=522,type_meas="deviance",selection_type="modal"){
  set.seed(seed)
  bestmods_accuracy_stats<-data.frame()

  #print(ddata)
  #print(response_var)

  #print("here1")

  for (j in 1:length(tied_alphas)){

    #print("here")
    # print(tied_alphas[j])
    # print(tied_lambdas[j])
    # print(tied_cutoffs[j])
    # print("tcjj")
    # print(tied_cutoffs[[j]])

    bestmods_accuracy_stats_results<-en_kfold_accuracy_grid(ddata = ddata,
                                                            response_var = response_var,
                                                            mod_alpha =  tied_alphas[j],
                                                            mod_lambda = tied_lambdas[j],
                                                            lr_cutoff= tied_cutoffs[j],
                                                            iter = iter,
                                                            k = k,
                                                            eq_wt = eq_wt,
                                                            type_meas=type_meas,
                                                            #accuracy_modeling=TRUE
                                                            )
    #print(j)
    #print(bestmods_accuracy_stats_results)
    if(j==1){
      bestmods_accuracy_stats<-rown_to_var(data.frame("Value"=bestmods_accuracy_stats_results$kfold_agg_results),varname = "statistic")
    } else {
      bestmods_accuracy_stats<-cbind(bestmods_accuracy_stats,data.frame(bestmods_accuracy_stats_results$kfold_agg_results))
    }

  }

  names(bestmods_accuracy_stats)<-c("metric",as.character(tied_alphas))

  bestmods_accuracy_stats_lim<-subset.data.frame(bestmods_accuracy_stats,metric!="AccuracyLower"&
                                                   metric!="AccuracyUpper"&
                                                   metric!="AccuracyNull"&
                                                   metric!="AccuracyPValue"&
                                                   metric!="McnemarPValue"&
                                                   metric!="Prevalence")

  print(bestmods_accuracy_stats_lim)

  #print("here2")

  if(favor_parsimony==TRUE){
    bestmods_accuracy_stats_lim<-bestmods_accuracy_stats_lim[,c(1,rev(2:ncol(bestmods_accuracy_stats_lim)))]
  }

  choice<-function(x,tied_alphas,tied_lambdas,tied_cutoffs){
    index<-names(which.max(x))
    alpha<-tied_alphas[which.max(x)]
    lambda<-tied_lambdas[which.max(x)]
    cutoff<-tied_cutoffs[which.max(x)]
    return(c("index"=index,"alpha"=alpha,"lambda"=lambda,"cutoff"=cutoff))
  }

  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }



  last_col<-ncol(bestmods_accuracy_stats_lim)

  print(choice(bestmods_accuracy_stats_lim[1,2:last_col],
               tied_alphas=tied_alphas,
               tied_lambdas=tied_lambdas,
               tied_cutoffs=tied_cutoffs))

  type_meas_result<-choice(bestmods_accuracy_stats_lim[1,2:last_col],
                           tied_alphas=tied_alphas,
                           tied_lambdas=tied_lambdas,
                           tied_cutoffs=tied_cutoffs)

  bestmods_accuracy_stats_lim$best_alpha<-apply(bestmods_accuracy_stats_lim[,2:last_col],
                                                1,
                                                function(x) choice(x,
                                                                   tied_alphas=tied_alphas,
                                                                   tied_lambdas=tied_lambdas,
                                                                   tied_cutoffs=tied_cutoffs)["alpha"])

  bestmods_accuracy_stats_lim$best_lambda<-apply(bestmods_accuracy_stats_lim[,2:last_col],
                                                1,
                                                function(x) choice(x,
                                                                   tied_alphas=tied_alphas,
                                                                   tied_lambdas=tied_lambdas,
                                                                   tied_cutoffs=tied_cutoffs)["lambda"])

  bestmods_accuracy_stats_lim$best_cutoff<-apply(bestmods_accuracy_stats_lim[,2:last_col],
                                                 1,
                                                 function(x) choice(x,
                                                                    tied_alphas=tied_alphas,
                                                                    tied_lambdas=tied_lambdas,
                                                                    tied_cutoffs=tied_cutoffs)["cutoff"])

  print(bestmods_accuracy_stats_lim)

  print(getmode(bestmods_accuracy_stats_lim$best_alpha))
  print(getmode(bestmods_accuracy_stats_lim$best_lambda))
  print(getmode(bestmods_accuracy_stats_lim$best_cutoff))



  #print("here6")

  return(list("mode_results"=list("best_alpha"=getmode(bestmods_accuracy_stats_lim$best_alpha),
                                  "best_lambda"=getmode(bestmods_accuracy_stats_lim$best_lambda),
                                  "best_cutoff"=getmode(bestmods_accuracy_stats_lim$best_cutoff),
                                  "best_alpha_value"=cton(getmode(bestmods_accuracy_stats_lim$best_alpha)),
                                  "best_lambda_value"=cton(getmode(bestmods_accuracy_stats_lim$best_lambda)),
                                  "best_cutoff_value"=cton(getmode(bestmods_accuracy_stats_lim$best_cutoff))),
              "type_meas_results"=list("best_alpha"=type_meas_result["alpha"],
                                       "best_lambda"=type_meas_result["lambda"],
                                       "best_cutoff"=type_meas_result["cutoff"],
                                       "best_alpha_value"=cton(type_meas_result["alpha"]),
                                       "best_lambda_value"=cton(type_meas_result["lambda"]),
                                       "best_cutoff_value"=cton(type_meas_result["cutoff"])),
              "limited results"=bestmods_accuracy_stats_lim,
              "all_results"=bestmods_accuracy_stats))
}







#' en_kfold_model function
#'
#' run kfold cross validated enr models (DOCUMENTATION COMING- CURRENT DOCUMENTATION INCORRECT)
#' @param ddata data frame containing the data to be modeled
#' @param response_var string identifying the name of the outcome variable
#' @param iter the number of iterations to use
#' @param k the number of folds to use
#' @param num_alpha an integer of the number of alphas to consider. this will be split across 0 to 1. for example if '5' is given then alphas will go from 0 to 1 and will be num_alpha/iteration (i.e. 0, .2, .4, .6, .8, 1)
#' @param seed the seed value for allowing results to be reproduced
#' @param fit_met string indicating the fit metric to be used to evaluate model performance. options are c(accuracy, auroc, logloss, f1, ppv, npv, sens, spec, bal_acc)
#' @param loo boolean indicating whether 'leave one out' cross validation should be used
#' @param up_dn_samp string indicating whether unbalanced classes should be balanced by having the smaller class upsampled to be the same size as the larger class or vice versa. can take the form 'upsamp', 'downsamp', and 'none' (default)
#' @param eq_wt boolean indicating whether the 0/1 classes should be balanced with weights. you may want to use this if there is a bad class imbalance
#' @param type_meas the 'type measure' which is passed to cv.glmnet that governs its training penalty when tuning lambda. this should match arguments expected in cv.glmnet
#' @param na_rm boolean indicating whether missing values should be removed. default is TRUE
#' @param lr_cutoff vetor of cutoff values to test/tune for optimization. the default is 'c(.5)' which is to say 'equal distance from all classes' which is typical in standard analyses
#' @param accuracy_modeling switch determining if we need to break ties between optimal solutions
#' @keywords en_kfold_model enr
#' @export
#' @examples
#' en_kfold_model()

en_kfold_model<-function(ddata,response_var,iter=10,k=10,num_alpha=20,seed=123,fit_met='accuracy',loo=FALSE,up_dn_samp='none',eq_wt=FALSE,type_meas="deviance",na_rm=TRUE,lr_cutoff=c(.5),accuracy_modeling=FALSE){
  set.seed(seed)

  if(up_dn_samp=='upsamp'){
    ddata<-upSample(ddata,as.factor(ddata[,response_var]))
    ddata<-ddata[,-ncol(ddata)]
  }
  if(up_dn_samp=='dwnsamp'){
    ddata<-downSample(ddata,as.factor(ddata[,response_var]))
    ddata<-ddata[,-ncol(ddata)]
  }

  print(paste("there are ",nrow(ddata)," cases in the training dataset"))
  acc_data<-ddata
  itern<-1
  f <- as.formula(paste(response_var, "~", paste(names(ddata[,!names(ddata) %in% c(response_var)]), collapse=" + ")))

  if(loo==TRUE){
    k<-nrow(ddata)
    iter<-1
  }

  pbar <- create_progress_bar('text')
  pbar$init(iter*k*num_alpha*length(lr_cutoff))

  list.of.fits <- list()
  results <- data.frame()
  loo_tracker<-vector()

  print("Running k fold cross validation to find optimal alpha value")

  for(i in 1:iter){
    if(loo==FALSE){
      ddata<-ddata[order(runif(nrow(ddata))),]
    }
    ddata$grp<-rep(1:k,length.out=nrow(ddata))

    for(j in 1:k)
    {
      itername<-paste("iter_",i,"_fold_",j,sep = "")

      if(loo==TRUE){
        train<-ddata[-j,-ncol(ddata)]
        test<-ddata[j,-ncol(ddata)]
      } else {
        train<-ddata[ddata$grp!=j,-ncol(ddata)]
        test<-ddata[ddata$grp==j,-ncol(ddata)]
      }
      #print(mean(train[,1]))
      #print(mean(test[,1]))
      wt<-NULL
      if(eq_wt==TRUE){
        wt<-ifelse(train[,1]==0,
                   1-(sum(train[,1]==0)/nrow(train)),
                   1-(sum(train[,1]==1)/nrow(train)))
      }

      train_pred <- model.matrix(f,train)[,-1]
      train_resp <- train[,response_var]

      #print(head(train_pred))
      #print(train_resp)

      test_pred <- model.matrix(f,test)[,-1]
      test_resp <- test[,response_var]

      #print(head(test_pred))
      #print(test_resp)

      for(alphaa in 0:num_alpha) {
        #print(runif(1))
        fit.name<-paste("iter_",i,"_fold_",j,"_alpha_",(alphaa/num_alpha),sep = "")

        ## Now fit a model (i.e. optimize lambda) and store it in a list that
        ## uses the variable name we just created as the reference.
        #print(head(wt))
        list.of.fits[[fit.name]] <- cv.glmnet(train_pred,
                                              train_resp,
                                              weights = wt,
                                              type.measure = type_meas,
                                              alpha=alphaa/num_alpha,
                                              family="binomial")

        #print(list.of.fits[[fit.name]])

        # predicted <- cton(predict(list.of.fits[[fit.name]],
        #              s=list.of.fits[[fit.name]]$lambda.1se,type="class", newx=test_pred))

        predicted_prob <- cton(predict(list.of.fits[[fit.name]],
                                       s=list.of.fits[[fit.name]]$lambda.1se,type="response", newx=test_pred))

        test_resp<-cton(test_resp)
        #print(paste("predddd",mean(predicted)))

        for(l_cut in 1:length(lr_cutoff)){
          lr_c<-lr_cutoff[l_cut]
          #print(lr_c)

          predicted <- cton(ifelse(predicted_prob>lr_c,1,0))

          ## Calculate the fit...CHANGED THIS TO LOG LOSS BASED ON INFORMATION FOUND HERE https://towardsdatascience.com/why-not-mse-as-a-loss-function-for-logistic-regression-589816b5e03c
          if(fit_met=="accuracy"){
            fit <- MLmetrics::Accuracy(y_pred = predicted,
                                       y_true = test_resp)
          } else if(fit_met=="auroc"){
            fit <- MLmetrics::AUC(y_true = factor(test_resp,levels = c("1","0")),
                                  y_pred = factor(predicted,levels = c("1","0")))
          } else if(fit_met=="logloss"){
            fit <- MLmetrics::LogLoss(y_pred = predicted_prob,
                                      y_true = test_resp)
          } else if(fit_met=="f1"){
            fit <- MLmetrics::F1_Score(y_true = factor(test_resp,levels = c("1","0")),
                                       y_pred = factor(predicted,levels = c("1","0")),positive = "1")
          } else if(fit_met=="ppv"){
            fit <- MLmetrics::Precision(y_true = factor(test_resp,levels = c("1","0")),
                                        y_pred = factor(predicted,levels = c("1","0")),positive = "1")
          } else if(fit_met=="npv"){
            fit <- caret::negPredValue(factor(test_resp,levels = c("1","0")),
                                       factor(predicted,levels = c("1","0")),positive = "1")
          } else if(fit_met=="sens"){
            fit <- MLmetrics::Sensitivity(y_true = factor(test_resp,levels = c("1","0")),
                                          y_pred = factor(predicted,levels = c("1","0")),positive = "1")
          } else if(fit_met=="spec"){
            fit <- MLmetrics::Specificity(y_true = factor(test_resp,levels = c("1","0")),
                                          y_pred = factor(predicted,levels = c("1","0")),positive = "1")
          } else if(fit_met=="bal_acc"){
            fit <- yardstick::bal_accuracy_vec(truth = factor(test_resp,levels = c("1","0")),
                                               estimate = factor(predicted,levels = c("1","0")))
          } else {
            fit <- mean((test_resp - predicted)^2)
          }

          #print(lr_c)

          ## Store the results
          temp <- data.frame(alpha=alphaa/num_alpha, fit=fit, fit.name=fit.name, class_cutoff = lr_c)
          results <- rbind(results, temp)
          #print(temp)
          itern<-itern+1

          pbar$step()

        }


      }
    }
  }

  print(paste("n(%) of NA fit values ",sum(is.na(results$fit))," (",highlandr::percent((sum(is.na(results$fit))/length(results$fit)),digits=1),")",sep = ""))

  agg_results<-aggregate(list(fit=results$fit),by=list(alpha=results$alpha, class_cutoff=results$class_cutoff),mean,na.action=na_rm)
  agg_results_select<-agg_results[!is.na(agg_results$fit),]

  #print(names(agg_results_select))
  #print("look here")
  #print(agg_results_select[agg_results_select$fit==max(agg_results_select$fit),"class_cutoff"])
  #print(agg_results_select[agg_results_select$fit==min(agg_results_select$fit),"class_cutoff"])

  print(paste("n(%) of NA agg results ",sum(is.na(agg_results$fit))," (",highlandr::percent((sum(is.na(agg_results$fit))/length(agg_results$fit)),digits=1),")",sep = ""))

  #besten_mod_name<-as.character(agg_results_select[agg_results_select$fit==min(agg_results_select$fit),"fit.name"])
  if(fit_met=="accuracy" | fit_met=="auroc" | fit_met=="f1" | fit_met=="ppv" | fit_met=="npv" | fit_met=="sens" | fit_met=="spec" | fit_met=="bal_acc"){
    best_mod_alpha<-agg_results_select[agg_results_select$fit==max(agg_results_select$fit),"alpha"]
    best_mod_cutoff<-agg_results_select[agg_results_select$fit==max(agg_results_select$fit),]$class_cutoff
    #print(agg_results_select[agg_results_select$fit==max(agg_results_select$fit),"class_cutoff"])
    #print(agg_results_select[agg_results_select$fit==max(agg_results_select$fit),])
  } else {
    best_mod_alpha<-agg_results_select[agg_results_select$fit==min(agg_results_select$fit),"alpha"]
    best_mod_cutoff<-agg_results_select[agg_results_select$fit==min(agg_results_select$fit),]$class_cutoff
    #print(best_mod_cutoff<-agg_results_select[agg_results_select$fit==min(agg_results_select$fit),])
  }

  #print(agg_results)
  print("best alphas")
  print(best_mod_alpha)
  print("best cutoffs")
  #print(names(best_mod_cutoff))
  print(best_mod_cutoff)


  x <- model.matrix(f,ddata)[,-1]
  y <- ddata[,response_var]

  if((length(best_mod_alpha)>1)&!accuracy_modeling){
    print("This will take a litte while longer becase we need to examine performance metrics as there was a tie for best alpha")
    best_mod_alpha_res<-en_kfold_accuracy_tied(tied_alphas=best_mod_alpha,
                                               tied_cutoffs=best_mod_cutoff,
                                               ddata = acc_data,
                                               response_var = response_var,
                                               eq_wt = eq_wt,
                                               type_meas = type_meas)
    best_mod_alpha<-best_mod_alpha_res$best_alpha_value
    best_mod_cutoff<-best_mod_alpha_res$best_cutoff
  } else {
    best_mod_alpha_res<-"there was no need to use performance measures"
  }

  best_model<-cv.glmnet(x,
                        y,
                        type.measure=type_meas,
                        alpha=best_mod_alpha,
                        family="binomial")

  if(eq_wt==TRUE){
    print(y)
    final_weights<-ifelse(y==0,
                          1-(sum(y==0)/length(y)),
                          1-(sum(y==1)/length(y)))

    #print(final_weights)

    best_model<-cv.glmnet(x,
                          y,
                          type.measure=type_meas,
                          alpha=best_mod_alpha,
                          weights = final_weights,
                          family = "binomial")
  }

  print("Calculating model performance")

  if(loo==TRUE){
    en_accuracy<-en_kfold_accuracy(ddata = acc_data,
                                   response_var = response_var,
                                   mod_alpha =  best_mod_alpha,
                                   loo = TRUE,
                                   type_meas = type_meas,
                                   lr_cutoff = best_mod_cutoff)
  } else {
    en_accuracy<-en_kfold_accuracy(ddata = acc_data,
                                   response_var = response_var,
                                   mod_alpha =  best_mod_alpha,
                                   iter = iter,
                                   k = k,
                                   type_meas = type_meas,
                                   lr_cutoff = best_mod_cutoff)
  }

  # print(model.matrix(formula_maker(response_var,names(ddata)[!names(ddata) %in% c(response_var)]),ddata)[,-c(1,ncol(ddata))])
  # print(predict(best_model,
  #               newx=model.matrix(formula_maker(response_var,names(ddata)[!names(ddata) %in% c(response_var)]),ddata)[,-c(1,ncol(ddata))],
  #               type="class"))
  # print(ddata[,response_var])

  best_model_internal_pred_probs<-predict(best_model,
                                          newx=model.matrix(formula_maker(response_var,names(ddata)[!names(ddata) %in% c(response_var)]),ddata)[,-c(1,ncol(ddata))],
                                          type="response")

  best_model_internal_conf_mat<-caret::confusionMatrix(factor(ifelse(predict(best_model,
                                                                             newx=model.matrix(formula_maker(response_var,names(ddata)[!names(ddata) %in% c(response_var)]),ddata)[,-c(1,ncol(ddata))],
                                                                             type="response")>best_mod_cutoff,1,0),
                                                              levels = c("1","0")),
                                                       factor(ddata[,response_var],
                                                              levels = c("1","0")))$table

  en_accuracy_results<-en_accuracy$kfold_results
  en_accuracy_stats<-rown_to_var(data.frame("Value"=round(en_accuracy$kfold_agg_results,4)),varname = "statistic")
  best_mod_ors_all<-cbind(rown_to_var(as.data.frame(as.matrix(round(exp(coef(best_model,best_model$lambda.1se)),3)))),
                          "variable_dropped"=as.vector(coef(best_model,best_model$lambda.1se))==0)
  best_mod_betas<-rown_to_var(as.data.frame(as.matrix(round(coef(best_model,best_model$lambda.1se),3))))

  print(paste("best alpha =",best_mod_alpha))
  print(paste("best lambda =",best_model$lambda.1se))
  print(paste("best cutoff =",best_mod_cutoff))
  print(en_accuracy_stats)
  print(best_model_internal_conf_mat)

  return(list("best_en_model"=best_model,
              "best_mod_ors_all"=best_mod_ors_all,
              "best_mod_betas"=best_mod_betas,
              "alpha_agg_results"=agg_results,
              "best_mod_alpha"=best_mod_alpha,
              "best_mod_lambda"=best_model$lambda.1se,
              "best_mod_cutoff"=best_mod_cutoff,
              "best_model_internal_conf_mat"=best_model_internal_conf_mat,
              "best_model_internal_pred_probs"=best_model_internal_pred_probs,
              "best_mod_alpha_res"=best_mod_alpha_res,
              "en_accuracy_stats"=en_accuracy_stats,
              "en_accuracy_results"=en_accuracy_results,
              #"results"=results,
              "nas_removed_from_results"=paste("n(%) of NA fit values ",sum(is.na(results$fit))," (",highlandr::percent((sum(is.na(results$fit))/length(results$fit)),digits=1),")",sep = ""),
              "nas_removed_from_agg_results"=paste("n(%) of NA agg results ",sum(is.na(agg_results$fit))," (",highlandr::percent((sum(is.na(agg_results$fit))/length(agg_results$fit)),digits=1),")",sep = ""),
              "list.of.fits"=list.of.fits))

}








#' run_all_enr_fit_mets function
#'
#' Uses cross validation ENR functions to consecutively check maximization of multiple performance metrics (DOCUMENTATION COMING- CURRENT DOCUMENTATION INCORRECT)
#' @param dat data frame containing the data to be modeled
#' @param response_var string identifying the name of the outcome variable
#' @param tune_type string indicating the tuning method to use. current options are 'og' (default) which will use the 'en_kfold_model' function to tune alpha and cutoffs using k fold cross validation and 'grid_lim' which will use the 'en_kfold_model_grid_lim' function to simultaneously estimate the three parameters using a randomized expanded grid
#' @param modname string of the base name of the model. default is 'model'
#' @param specs a vector of the first function to use (i.e. outside the parentheses) if fp='FALSE'. default is 'mean'. if supplying different functions be sure to quote e.g. "IQR"
#' @param date_tf boolean indicating if the date should be written to the output files. default is TRUE
#' @param time_tf boolean indicating if the time should be written to the output files. default is FALSE
#' @param ties_measure string indicating the method for breaking ties. default is 'mode' indicating that the model with the best performance across all fit metrics listed will when when model results are tied.
#' @param fit_mets vector indicating all fit metrics to be used to evaluate model performance. options are c(accuracy, auroc, logloss, f1, ppv, npv, sens, spec, bal_acc)
#' @param dir_name string indicating the directory to which model results should be saved
#' @param iter the number of iterations to use
#' @param k the number of folds to use
#' @param num_alpha an integer of the number of alphas to consider. this will be split across 0 to 1. for example if '5' is given then alphas will go from 0 to 1 and will be num_alpha/iteration (i.e. 0, .2, .4, .6, .8, 1)
#' @param eq_wt boolean indicating whether the 0/1 classes should be balanced with weights. you may want to use this if there is a bad class imbalance
#' @param lr_cutoff vector of cutoff values to test/tune for optimization. the default is 'c(.5)' which is to say 'equal distance from all classes' which is typical in standard analyses
#' @keywords run_all_enr_fit_mets enr
#' @export
#' @examples
#' run_all_enr_fit_mets()



run_all_enr_fit_mets<-function(dat,response_var,tune_type="og",modname="model",specs=TRUE,date_tf=TRUE,time_tf=FALSE,ties_measure="mode",fit_mets=c("acc","balacc","ppv","f1","sens","auroc","npv","spec","logloss"),dir_name='I:/Lagisetty SDR Misuse/5. Identifiable Data/E. Database/treatment arm creation/treatment arm creation/enr mods/',iter=50,k=10,num_alpha=20,eq_wt = FALSE, lr_cutoff = seq(from = .05, to = .95, by = .05), ...){

  list_of_files<-vector()
  metric_comparisons<-data.frame()
  metric_fitstats<-data.frame()
  metric_ors<-data.frame()
  metric_confmats<-list()

  if(date_tf&time_tf){
    date_time_var<-format(now(),"%m_%d_%Y_%H.%M.%S")
  } else if (date_tf) {
    date_time_var<-format(now(),"%m_%d_%Y")
  } else if (time_tf){
    date_time_var<-format(now(),"%H.%M.%S")
  } else {
    date_time_var<-""
  }

  if(specs){
    specs_var<-paste("i",iter,"_k",k,"_a",num_alpha,"_c",length(lr_cutoff),"_ew",ifelse(eq_wt,"T_","F_"),sep = "")
  } else {
    specs_var<-""
  }

  pbar <- create_progress_bar('text')
  pbar$init(length(fit_mets))

  for(i in 1:length(fit_mets)){
    print(fit_mets[i])

    mod_name<-paste(modname,"_",fit_mets[i],"_",tune_type,"tune_",specs_var,date_time_var,sep = "")

    if(tune_type=="og"){

      mod<-en_kfold_model(ddata = dat,
                          response_var = response_var,
                          iter=iter,
                          k=k,
                          num_alpha=num_alpha,
                          eq_wt = eq_wt,
                          fit_met=fit_mets[i],
                          lr_cutoff = lr_cutoff,
                          ...)

    } else if(tune_type=="grid"){

      mod<-en_kfold_model_grid(ddata = dat,
                               response_var = response_var,
                               iter=iter,
                               k=k,
                               num_alpha=num_alpha,
                               eq_wt = eq_wt,
                               fit_met=fit_mets[i],
                               lr_cutoff = lr_cutoff,
                               ties_measure="mode",
                               ...)

    } else if(tune_type=="grid_lim"){

      mod<-en_kfold_model_grid_lim(ddata = dat,
                               response_var = response_var,
                               iter=iter,
                               k=k,
                               num_alpha=num_alpha,
                               eq_wt = eq_wt,
                               fit_met=fit_mets[i],
                               lr_cutoff = lr_cutoff,
                               ties_measure="mode",
                               ...)

    }  else {

      print("enter valid tune_type")

    }

    #filename<-paste(dir_name,mod_name,".RData",sep = "")
    filename<-paste(dir_name,mod_name,".rds",sep = "")

    list_of_files[i]<-filename

    #save(mod,file=filename)

    #do.call(save,list(mod,file=filename))
    do.call(saveRDS,list(mod,file=filename))

    if(i==1){
      metric_comparisons<-data.frame(mod$en_accuracy_stats)
      metric_ors<-data.frame(mod$best_mod_ors_all)
      metric_fitstats[i,c("model","alpha","lambda")]<-c(mod_name,mod$best_mod_alpha,mod$best_mod_lambda)
      metric_confmats[[fit_mets[i]]]<-mod$best_model_internal_conf_mat

      metric_comparisons<-highlandr::rename.variables(metric_comparisons,fit_mets[i],"Value")
      metric_ors<-highlandr::rename.variables(metric_ors,apply(expand.grid(fit_mets[i], c("or","drop")), 1, paste, collapse="_"),c("s1","variable_dropped"))
    } else {
      # metric_comparisons<-as.data.frame(cbind(metric_comparisons,mod$en_accuracy_stats$Value))
      metric_comparisons[,fit_mets[i]]<-mod$en_accuracy_stats$Value
      metric_fitstats[i,c("model","alpha","lambda")]<-c(mod_name,mod$best_mod_alpha,mod$best_mod_lambda)
      metric_ors[,apply(expand.grid(fit_mets[i], c("or","drop")), 1, paste, collapse="_")]<-mod$best_mod_ors_all[,c("s1","variable_dropped")]
      metric_confmats[[fit_mets[i]]]<-mod$best_model_internal_conf_mat
    }

    rm(mod)

    print(metric_comparisons)
    print("total complete")
    pbar$step()

  }

  #colnames(metric_comparisons[,2:length(fit_mets)])<-fit_mets

  ret<-list("list of files"=list_of_files,"metric_comparisons"=metric_comparisons,"metric_fitstats"=metric_fitstats,"metric_ors"=metric_ors,"metric_confmats"=metric_confmats)

  #save(ret,file=paste(dir_name,"file_list_",modname,"_",date_time_var,".RData",sep = ""))

  #do.call(save, list(ret,file=paste(dir_name,"file_list_",modname,"_",date_time_var,".RData",sep = "")))
  do.call(saveRDS, list(ret,file=paste(dir_name,"file_list_",modname,"_",date_time_var,".rds",sep = "")))

  return(ret)

}





#' retrieve_enr_mods function
#'
#' retrieves ENR models from a directiory using the output from run_all_enr_fit_mets function (DOCUMENTATION COMING- CURRENT DOCUMENTATION INCORRECT)
#' @param directory directory to look for the models
#' @param list_of_files output from 'run_all_enr_models' containing the file names
#' @param all_files boolean indicating if all fiels should be read into memory
#' @param patterns a regex style pattern to identify which files to load into memory, all files should be false if supplying an argument here
#' @keywords retrieve_enr_mods enr
#' @export
#' @examples
#' retrieve_enr_mods()

retrieve_enr_mods<-function(directory,list_of_files=NULL,all_files=FALSE,patterns=""){

  if(!is.null(list_of_files)){
    enr_mods<-list()
    list_of_names<-sapply(lapply(sapply(list_of_files,function(x) strsplit(x,"/")),tail,n=1),function(x) strsplit(x,"\\.")[[1]][1])
    #print(sapply(list_of_files,function(x) strsplit(x,"\\.")[[1]][1]))
    print(list_of_names)
    for(i in 1:length(list_of_files)){
      enr_mods[list_of_names[i]]<-load(list_of_files[i])
    }
  } else if(all_files){
    files <- list.files(directory)

    if(patterns!=""){
      for(j in 1:length(patterns)){
        files <- files %>%
          tibble() %>%
          filter(stringr::str_detect(files, patterns[j])) %>%
          pull()
      }
    }

    enr_mods <- list() #create empty list

    #loop through the files
    for (k in files) {
      #print(i)
      enr_mods[[k]] <- get(load(paste0(directory, k))) #add files to list position
    }
  } else {
    print("supply file names or switch all_files to TRUE")
  }
  return(enr_mods)
}









#' en_kfold_accuracy_grid function
#'
#' calculates accuracy of grid based ENR results (DOCUMENTATION COMING- CURRENT DOCUMENTATION INCORRECT)
#' @param ddata data frame containing the data to be modeled
#' @param response_var string identifying the name of the outcome variable
#' @param mod_alpha the alpha(s) value(s) to be checked. alpha is the ENR blending parameter that governs how much ridge regression (0) and lasso regression (1) will be used.
#' @param mod_lambda the lambda(s) to be checked. lambda is the ENR penalty parameter for the ridge portion of the ENR
#' @param iter the number of iterations to use
#' @param k the number of folds to use
#' @param seed the seed value for allowing results to be reproduced
#' @param loo boolean indicating whether 'leave one out' cross validation should be used
#' @param eq_wt boolean indicating whether the 0/1 classes should be balanced with weights. you may want to use this if there is a bad class imbalance
#' @param type_meas the 'type measure' which is passed to cv.glmnet that governs its training penalty when tuning lambda. this should match arguments expected in cv.glmnet
#' @param lr_cutoff vector of cutoff values to test/tune for optimization. the default is 'c(.5)' which is to say 'equal distance from all classes' which is typical in standard analyses
#' @keywords en_kfold_accuracy_grid enr
#' @export
#' @examples
#' en_kfold_accuracy_grid()

en_kfold_accuracy_grid<-function(ddata,response_var,mod_alpha,mod_lambda,iter=100,k=10,seed=123,loo=FALSE,eq_wt=FALSE,type_meas = "deviance",lr_cutoff=c(.5)){

  print("cutoff")
  print(lr_cutoff)

  kfoldcv_results_data<-data.frame()

  set.seed(seed)
  itern<-1

  ac_model_form <- as.formula(paste(response_var, "~", paste(names(ddata[,!names(ddata) %in% c(response_var)]), collapse=" + ")))

  loo_tracker<-vector()

  if(loo==TRUE){
    k<-nrow(ddata)
    iter<-1
  }

  pbar <- create_progress_bar('text')
  pbar$init(iter*k)

  for(i in 1:iter){
    if(loo==FALSE){
      ddata<-ddata[order(runif(nrow(ddata))),]
    }
    ddata$grp<-rep(1:k,length.out=nrow(ddata))

    for(j in 1:k)
    {
      itername<-paste("cv_",i,"_reffold_",j,sep = "")

      if(loo==TRUE){
        train<-ddata[-j,-ncol(ddata)]
        test<-ddata[j,-ncol(ddata)]
      } else {
        train<-ddata[ddata$grp!=k,-ncol(ddata)]
        test<-ddata[ddata$grp==k,-ncol(ddata)]
      }

      wt<-NULL
      if(eq_wt==TRUE){
        wt<-ifelse(train[,1]==0,
                   1-(sum(train[,1]==0)/nrow(train)),
                   1-(sum(train[,1]==1)/nrow(train)))
      }

      train_pred <- model.matrix(ac_model_form,train)[,-1]
      train_resp <- train[,response_var]

      test_pred <- model.matrix(ac_model_form,test)[,-1]
      if(loo==TRUE){
        test_pred<-t(test_pred)
      }
      test_resp <- test[,response_var]

      # Fitting
      model <- glmnet(x = train_pred,
                      y = train_resp,
                      weights = wt,
                      type.measure=type_meas,
                      alpha=mod_alpha,
                      lambda=mod_lambda,
                      family="binomial")

      # Predict results
      # results_pred <- predict(model,newx=test_pred,type="class",s=model$lambda.1se)

      results_pred_prob <- predict(model,newx=test_pred,type="response",s=mod_lambda)

      #print(results_pred_prob)
      #print(nrow(results_pred_prob))

      #print(results_pred_prob)
      #print(results_pred_prob>lr_cutoff)

      results_pred <- ifelse((cton(predict(model,newx=test_pred,type="response",s=mod_lambda))>lr_cutoff),1,0)

      #print(results_pred)
      #print(nrow(results_pred))

      results_pred<-factor(results_pred,levels = c("1","0"))
      test_resp<-factor(test_resp,levels = c("1","0"))


      #print(cbind(results_pred,test_resp,(results_pred_prob>lr_cutoff),lr_cutoff))

      # Confusion matrix
      cm <- caret::confusionMatrix(data = results_pred,
                                   reference = test_resp,
                                   positive = "1")

      #AUC
      mod_auc<-suppressMessages(roc(test_resp,cton(results_pred_prob))$auc)

      # Collecting results
      cm_results<-t(data.frame(c("iter_fold"=itername,cm$overall,cm$byClass,"AUC"=mod_auc)))

      #rbind the results together
      kfoldcv_results_data<-rbind(kfoldcv_results_data,cm_results)

      itern<-itern+1

      pbar$step()

    }
  }
  # Average accuracy of the model
  rownames(kfoldcv_results_data)<-1:nrow(kfoldcv_results_data)
  kfoldcv_results<-colMeans(apply(kfoldcv_results_data[,2:ncol(kfoldcv_results_data)],2,cton),na.rm = TRUE)

  kfold_results<-list("kfold_agg_results"=kfoldcv_results,"kfold_results"=kfoldcv_results_data)

  return(kfold_results)

}









#' cut_performance function
#'
#' fucntion to calculate optimal cut performance (DOCUMENTATION COMING- CURRENT DOCUMENTATION INCORRECT)
#' @param predicted_probs vector of predicted probabilities coming from a logistic model
#' @param cutpoints vector of cut points to loop through to and produce classification based on predicted probabilities
#' @param fit_met the fit metric used to evaluate the classification given the cutoff
#' @param y_true vector of 'true' values to be compared against classification based on predicted probs and cutoffs
#' @keywords cut_performance enr
#' @export
#' @examples
#' cut_performance()

cut_performance<-function(predicted_probs,cutpoints,fit_met,y_true){

  preds<-rename.variables(data.frame(sapply(cutpoints,function(x) factor(cton(ifelse(predicted_probs>x,1,0)),levels = c("1","0")))),paste("c_",cutpoints,sep=""))
  #res<-lapply(preds,MLmetrics::Accuracy,y_true=factor(y_true,levels = c("1","0")))
  #print(preds)

    if(fit_met=="accuracy"){
    fit <- lapply(preds,MLmetrics::Accuracy,y_true=factor(y_true,levels = c("1","0")))
  } else if(fit_met=="auroc"){
    fit <- lapply(preds,MLmetrics::AUC,y_true=factor(y_true,levels = c("1","0")))
  } else if(fit_met=="logloss"){
    fit <- lapply(preds,MLmetrics::LogLoss,y_true=factor(y_true,levels = c("1","0")))
  } else if(fit_met=="f1"){
    fit <- lapply(preds,MLmetrics::F1_Score,y_true=factor(y_true,levels = c("1","0")),positive = "1")
  } else if(fit_met=="ppv"){
    fit <- lapply(preds,MLmetrics::Precision,y_true=factor(y_true,levels = c("1","0")),positive = "1")
  } else if(fit_met=="npv"){
    fit <- lapply(preds,caret::negPredValue,reference=factor(y_true,levels = c("1","0")),positive = "1")
  } else if(fit_met=="sens"){
    fit <- lapply(preds,MLmetrics::Sensitivity,y_true=factor(y_true,levels = c("1","0")),positive = "1")
  } else if(fit_met=="spec"){
    fit <- lapply(preds,MLmetrics::Specificity,y_true=factor(y_true,levels = c("1","0")),positive = "1")
  } else if(fit_met=="bal_acc"){
    fit <- lapply(lapply(preds,factor,levels = c("1","0")),yardstick::bal_accuracy_vec,truth=factor(y_true,levels = c("1","0")))
  } else {
    fit <- lapply(preds,function(x) mean((y_true - x)^2))
  }

  return(unlist(fit))
}







#' en_kfold_model_grid_lim function
#'
#' Runs k fold cross validation ENR models with simultaneous estimation of alpha, lambda, and cutoff via randomized grid (DOCUMENTATION COMING- CURRENT DOCUMENTATION INCORRECT)
#' @param ddata data frame containing the data to be modeled
#' @param response_var string identifying the name of the outcome variable
#' @param iter the number of iterations to use
#' @param k the number of folds to use
#' @param seed the seed value for allowing results to be reproduced
#' @param fit_met string indicating the fit metric to be used to evaluate model performance. options are c(accuracy, auroc, logloss, f1, ppv, npv, sens, spec, bal_acc)
#' @param loo boolean indicating whether 'leave one out' cross validation should be used
#' @param up_dn_samp string indicating whether unbalanced classes should be balanced by having the smaller class upsampled to be the same size as the larger class or vice versa. can take the form 'upsamp', 'downsamp', and 'none' (default)
#' @param eq_wt boolean indicating whether the 0/1 classes should be balanced with weights. you may want to use this if there is a bad class imbalance
#' @param type_meas the 'type measure' which is passed to cv.glmnet that governs its training penalty when tuning lambda. this should match arguments expected in cv.glmnet
#' @param na_rm boolean indicating whether missing values should be removed. default is TRUE
#' @param lr_cutoff vetor of cutoff values to test/tune for optimization. the default is 'c(.5)' which is to say 'equal distance from all classes' which is typical in standard analyses
#' @param accuracy_modeling switch determining if we need to break ties between optimal solutions
#' @param ties_measure string indicating the method for breaking ties. default is 'mode' indicating that the model with the best performance across all fit metrics listed will when when model results are tied.
#' @param save_results boolean indicating if the unaggregated results should be saved and returned. default is FALSE as this is typically too much material to save and could crash the r session
#' @param writeout boolean indicating if the progress of the function should be written out in real time in order to restart if need be. the file wll contain the aggregated results up to some iteration so that the process may be restarted without having to return to i = 1
#' @param writeout_num the number of iterations before the results are written out to save. a higher number will move faster (beacuse it's not writing out all the time) and a lower number will move slower but will probably mean a lower number of reruns in the even of an interruption. default is alpha*5 so in the event of checking 20 alphas the function will write results every 100 iterations
#' @param writeout_path string of the path directory for writing out results
#' @param restarting boolean indicating whether the process is restarting. if it is then the function looks for a file in the 'writeout_path' and reads that in and continues from where it left off
#' @keywords en_kfold_model_grid_lim enr
#' @export
#' @examples
#' en_kfold_model_grid_lim()

en_kfold_model_grid_lim<-function(ddata,response_var,iter=10,k=10,num_alpha=20,num_lambda=100,seed=123,fit_met='accuracy',loo=FALSE,up_dn_samp='none',eq_wt=FALSE,type_meas="deviance",na_rm=TRUE,lr_cutoff=c(.5),accuracy_modeling=FALSE,ties_measure="mode",save_results = FALSE,writeout = T,writeout_num=num_alpha*5,writeout_path = "I:/Lagisetty SDR Misuse/5. Identifiable Data/E. Database/treatment arm creation/treatment arm creation/enr grid writeout directory/",restarting=FALSE){
  start.time <- Sys.time()
  set.seed(seed)
  alpha_start <- 1
  timercnt <- 1
  # if(!is.factor(d1_enr[,"include_criteria"])){
  #   d1_enr[,"include_criteria"]<-factor(d1_enr[,"include_criteria"],levels=c(0,1),labels = c("0","1"))
  #   }

  enr_grid<-expand.grid(alpha = seq(0,1,by=(1/(num_alpha))),
                        lambda = (10^seq(-3, 3, length = num_lambda))) %>% #,
                        #cutoff = lr_cutoff) %>%
            arrange(alpha,lambda)#,cutoff)

  #print(nrow(enr_grid))

  if(restarting){
    # List all the .rds files in the directory
    rds_files <- list.files(path = writeout_path, pattern = paste0(fit_met,".*\\.rds$"), full.names = TRUE)

    # Check if there are any .rds files in the directory
    if (length(rds_files) > 0) {

      if (length(rds_files) > 1) {
        print("There is more than 1 file in directory, using the first one but double check")
      }

      first_rds_file <- rds_files[1]

      restart_df <- readRDS(first_rds_file)

      # print(enr_grid)
      # print(restart_df)
      #
      # print(nrow(enr_grid))
      # print(nrow(restart_df))

      enr_grid<-anti_join(enr_grid,restart_df,by=c("alpha" = "alpha","lambda" = "lambda"))#,"cutoff" = "class_cutoff"))

      timercnt <- nrow(enr_grid)
      # print(enr_grid)
      # print(nrow(enr_grid))

      #alpha_start <- nrow(restart_df) + 1

    } else {
      cat("No .rds files found in the specified directory. Not limiting the data frame at all\n")
    }
  }

  if(up_dn_samp=='upsamp'){
    ddata<-upSample(ddata,as.factor(ddata[,response_var]))
    ddata<-ddata[,-ncol(ddata)]
  }
  if(up_dn_samp=='dwnsamp'){
    ddata<-downSample(ddata,as.factor(ddata[,response_var]))
    ddata<-ddata[,-ncol(ddata)]
  }

  #print(restarting)
  #print()

  if(restarting&nrow(enr_grid)==0){
    agg_results<-restart_df
    f <- as.formula(paste(response_var, "~", paste(names(ddata[,!names(ddata) %in% c(response_var)]), collapse=" + ")))
    acc_data<-ddata
  } else {


  print(paste("there are",nrow(ddata),"cases in the training dataset"))
  print(paste("with",num_alpha,"alphas,",num_lambda,"lambdas, and",length(lr_cutoff),"cutoffs there are",format(nrow(enr_grid)*length(lr_cutoff),scientific=FALSE),"hyperparameter combinations which is",format(iter*k*nrow(enr_grid),scientific=FALSE),"models to run"))
  acc_data<-ddata
  itern<-1
  f <- as.formula(paste(response_var, "~", paste(names(ddata[,!names(ddata) %in% c(response_var)]), collapse=" + ")))

  if(loo==TRUE){
    k<-nrow(ddata)
    iter<-1
  }

  # pbar <- create_progress_bar('text')
  # pbar$init(iter*k*nrow(enr_grid))

  pbar<-txtProgressBar(min=0,max = (iter*k*nrow(enr_grid)),style = 3)

  list.of.fits <- list()
  results <- data.frame()
  loo_tracker<-vector()
  if(restarting){
    agg_results<-restart_df
  } else {
    agg_results<-data.frame()
  }

  na_results<-0
  previous_file_name<-NULL

  print("Running k fold cross validation to find optimal alpha value")


  for(alphaa in alpha_start:nrow(enr_grid)) {

    #print(alpha_start)
    #print(alphaa)

    results<-data.frame()

      for(i in 1:iter){

        results<-data.frame()

        if(loo==FALSE){
          ddata<-ddata[order(runif(nrow(ddata))),]
        }
        ddata$grp<-rep(1:k,length.out=nrow(ddata))



        for(j in 1:k){



          itername<-paste("iter_",i,"_fold_",j,sep = "")

          if(loo==TRUE){
            train<-ddata[-j,-ncol(ddata)]
            test<-ddata[j,-ncol(ddata)]
          } else {
            train<-ddata[ddata$grp!=j,-ncol(ddata)]
            test<-ddata[ddata$grp==j,-ncol(ddata)]
          }


          wt<-NULL
          if(eq_wt==TRUE){
            wt<-ifelse(train[,1]==0,
                       1-(sum(train[,1]==0)/nrow(train)),
                       1-(sum(train[,1]==1)/nrow(train)))
          }

          train_pred <- model.matrix(f,train)[,-1]
          train_resp <- train[,response_var]

          test_pred <- model.matrix(f,test)[,-1]
          test_resp <- test[,response_var]

          fit.name<-paste("iter_",i,"_fold_",j,"_alpha_",(alphaa/num_alpha),sep = "")

          mod_alpha<-enr_grid[alphaa,"alpha"]
          mod_lambda<-enr_grid[alphaa,"lambda"]
          #mod_cutoff<-enr_grid[alphaa,"cutoff"]

          #print(enr_grid)

          ## Now fit a model (i.e. optimize lambda) and store it in a list that
          ## uses the variable name we just created as the reference.

          model_fit <- glmnet(train_pred,
                              train_resp,
                              weights = wt,
                              alpha=mod_alpha,
                              lambda=mod_lambda,
                              family="binomial")

          #print("made it 4")

          predicted_prob <- cton(predict(model_fit,
                                         s=mod_lambda,type="response", newx=test_pred))

          #print("made it 5")

          cut_perform<-cut_performance(predicted_prob,lr_cutoff,fit_met,test_resp)

          #print("made it 6")

          ## Store the results
          temp <- data.frame(alpha=rep(mod_alpha,length(lr_cutoff)),
                             lambda=rep(mod_lambda,length(lr_cutoff)),
                             class_cutoff = lr_cutoff,
                             fit=cut_perform,
                             fit.name=rep(itername,length(lr_cutoff)))

          #print("made it 7")

          results <- rbind(results, temp)

          itern<-itern+1

          setTxtProgressBar(pbar,(alphaa*i*j))

          update_timer(start.time,Sys.time(),timercnt,(iter*k*nrow(enr_grid)))

          timercnt <- timercnt+1

          #pbar$step()

        }

      }


    na_results<-na_results+sum(is.na(results$fit))

    agg_results_proto<-aggregate(list(fit=results$fit),by=list(alpha=results$alpha, lambda=results$lambda, class_cutoff=results$class_cutoff),mean,na.action=na_rm)

    agg_results<-rbind(agg_results,agg_results_proto)

    if((alphaa%%writeout_num)==0&writeout){


      if(restarting){
        model_combos<-alphaa+(nrow(restart_df)/length(lr_cutoff))
      } else {
        model_combos<-alphaa
      }

      writ_path<-paste(writeout_path,"model_results_first_",format(now(),"%m_%d_%Y"),"_",fit_met,"_",model_combos,".rds",sep = "")

      do.call(saveRDS,list(agg_results,file=writ_path))

      end.time <- Sys.time()

      dif_time<-(end.time - start.time)


      if (!is.null(previous_file_name)) {
        if (file.exists(previous_file_name)) {
          file.remove(previous_file_name)
        }
      }

      previous_file_name<-writ_path


    }

  }



print(paste("n(%) of NA fit values ",na_results," (",highlandr::percent((na_results/(iter*k*nrow(enr_grid))),digits=1),")",sep = ""))

  }

  agg_results_select<-agg_results[!is.na(agg_results$fit),]

  #print(names(agg_results_select))
  #print("look here")
  #print(agg_results_select[agg_results_select$fit==max(agg_results_select$fit),"class_cutoff"])
  #print(agg_results_select[agg_results_select$fit==min(agg_results_select$fit),"class_cutoff"])

  print(paste("n(%) of NA agg results ",sum(is.na(agg_results$fit))," (",highlandr::percent((sum(is.na(agg_results$fit))/length(agg_results$fit)),digits=1),")",sep = ""))

  #besten_mod_name<-as.character(agg_results_select[agg_results_select$fit==min(agg_results_select$fit),"fit.name"])
  if(fit_met=="accuracy" | fit_met=="auroc" | fit_met=="f1" | fit_met=="ppv" | fit_met=="npv" | fit_met=="sens" | fit_met=="spec" | fit_met=="bal_acc"){
    best_mod_alpha<-agg_results_select[agg_results_select$fit==max(agg_results_select$fit),"alpha"]
    best_mod_lambda<-agg_results_select[agg_results_select$fit==max(agg_results_select$fit),"lambda"]
    best_mod_cutoff<-agg_results_select[agg_results_select$fit==max(agg_results_select$fit),]$class_cutoff
    #print(agg_results_select[agg_results_select$fit==max(agg_results_select$fit),"class_cutoff"])
    #print(agg_results_select[agg_results_select$fit==max(agg_results_select$fit),])
  } else {
    best_mod_alpha<-agg_results_select[agg_results_select$fit==min(agg_results_select$fit),"alpha"]
    best_mod_lambda<-agg_results_select[agg_results_select$fit==min(agg_results_select$fit),"lambda"]
    best_mod_cutoff<-agg_results_select[agg_results_select$fit==min(agg_results_select$fit),]$class_cutoff
    #print(best_mod_cutoff<-agg_results_select[agg_results_select$fit==min(agg_results_select$fit),])
  }

  #print(agg_results)
  print("best alphas")
  print(best_mod_alpha)
  print("best lambdas")
  print(best_mod_lambda)
  print("best cutoffs")
  print(best_mod_cutoff)


  x <- model.matrix(f,ddata)[,-1]
  y <- ddata[,response_var]

  if((length(best_mod_alpha)>1)&!accuracy_modeling){
    print("This will take a little while longer becase we need to examine performance metrics as there was a tie for best alpha")
    best_mod_tied_res<-en_kfold_accuracy_tied_grid(tied_alphas=best_mod_alpha,
                                                    tied_lambdas=best_mod_lambda,
                                                    tied_cutoffs=best_mod_cutoff,
                                                    ddata = acc_data,
                                                    response_var = response_var,
                                                    eq_wt = eq_wt,
                                                    type_meas = type_meas)
    if(ties_measure=="type_measure"){
      best_mod_alpha<-best_mod_tied_res$type_meas_results$best_alpha_value[1]
      best_mod_lambda<-best_mod_tied_res$type_meas_results$best_lambda_value[1]
      best_mod_cutoff<-best_mod_tied_res$type_meas_results$best_cutoff_value[1]
    } else{
      #print(best_mod_tied_res)
      best_mod_alpha<-best_mod_tied_res$mode_results$best_alpha_value
      best_mod_lambda<-best_mod_tied_res$mode_results$best_lambda_value
      best_mod_cutoff<-best_mod_tied_res$mode_results$best_cutoff_value
    }
  } else {
    best_mod_tied_res<-"there was no need to use performance measures"
  }

  best_model<-glmnet(x,
                     y,
                     alpha=best_mod_alpha,
                     lambda=best_mod_lambda,
                     family="binomial")

  if(eq_wt==TRUE){
    print(y)
    final_weights<-ifelse(y==0,
                          1-(sum(y==0)/length(y)),
                          1-(sum(y==1)/length(y)))

    #print(final_weights)

    best_model<-glmnet(x,
                       y,
                       alpha=best_mod_alpha,
                       lambda=best_mod_lambda,
                       weights = final_weights,
                       family = "binomial")
  }

  print("Calculating model performance")

  if(loo==TRUE){
    en_accuracy<-en_kfold_accuracy_grid(ddata = acc_data,
                                        response_var = response_var,
                                        mod_alpha =  best_mod_alpha,
                                        mod_lambda = best_mod_lambda,
                                        loo = TRUE,
                                        type_meas = type_meas,
                                        lr_cutoff = best_mod_cutoff)
  } else {
    en_accuracy<-en_kfold_accuracy_grid(ddata = acc_data,
                                        response_var = response_var,
                                        mod_alpha =  best_mod_alpha,
                                        mod_lambda = best_mod_lambda,
                                        iter = iter,
                                        k = k,
                                        type_meas = type_meas,
                                        lr_cutoff = best_mod_cutoff)
  }

  # print(model.matrix(formula_maker(response_var,names(ddata)[!names(ddata) %in% c(response_var)]),ddata)[,-c(1,ncol(ddata))])
  # print(predict(best_model,
  #               newx=model.matrix(formula_maker(response_var,names(ddata)[!names(ddata) %in% c(response_var)]),ddata)[,-c(1,ncol(ddata))],
  #               type="class"))
  # print(ddata[,response_var])

  if("grp" %in% colnames(ddata)) {
  ddata <- ddata[, !(colnames(ddata) %in% c("grp"))]
  }

  print(model.matrix(formula_maker(response_var,names(ddata)[!names(ddata) %in% c(response_var)]),ddata)[,-1])



  best_model_internal_pred_probs<-predict(best_model,
                                          newx=model.matrix(formula_maker(response_var,names(ddata)[!names(ddata) %in% c(response_var)]),ddata)[,-1],
                                          type="response")

  #print("here2")

  best_model_internal_conf_mat<-caret::confusionMatrix(factor(ifelse(predict(best_model,
                                                                             newx=model.matrix(formula_maker(response_var,names(ddata)[!names(ddata) %in% c(response_var)]),ddata)[,-1],
                                                                             type="response")>best_mod_cutoff,1,0),
                                                              levels = c("1","0")),
                                                       factor(ddata[,response_var],
                                                              levels = c("1","0")))$table

  en_accuracy_results<-en_accuracy$kfold_results
  en_accuracy_stats<-rown_to_var(data.frame("Value"=round(en_accuracy$kfold_agg_results,4)),varname = "statistic")
  best_mod_ors_all<-cbind(rown_to_var(as.data.frame(as.matrix(round(exp(coef(best_model)),3)))),
                          "variable_dropped"=as.vector(coef(best_model))==0)
  best_mod_betas<-rown_to_var(as.data.frame(as.matrix(round(coef(best_model),3))))

  print(paste("best alpha =",best_mod_alpha))
  print(paste("best lambda =",best_mod_lambda))
  print(paste("best cutoff =",best_mod_cutoff))
  print(en_accuracy_stats)
  print(best_model_internal_conf_mat)

  end.time <- Sys.time()
  time.taken<-format_time_difference(end.time - start.time)
  print(time.taken)

  return(list("best_en_model"=best_model,
              "best_mod_ors_all"=best_mod_ors_all,
              "best_mod_betas"=best_mod_betas,
              "alpha_agg_results"=agg_results,
              "best_mod_alpha"=best_mod_alpha,
              "best_mod_lambda"=best_mod_lambda,
              "best_mod_cutoff"=best_mod_cutoff,
              "best_model_internal_conf_mat"=best_model_internal_conf_mat,
              "best_model_internal_pred_probs"=best_model_internal_pred_probs,
              "best_mod_tied_res"=best_mod_tied_res,
              "en_accuracy_stats"=en_accuracy_stats,
              "en_accuracy_results"=en_accuracy_results,
              "runtime"=time.taken,
              #"results"=results,
              #"nas_removed_from_results"=paste("n(%) of NA fit values ",sum(is.na(results$fit))," (",highlandr::percent((sum(is.na(results$fit))/length(results$fit)),digits=1),")",sep = ""),
              "nas_removed_from_agg_results"=paste("n(%) of NA agg results ",sum(is.na(agg_results$fit))," (",highlandr::percent((sum(is.na(agg_results$fit))/length(agg_results$fit)),digits=1),")",sep = "")#,
              #"list.of.fits"=list.of.fits
  ))

}









