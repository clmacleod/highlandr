% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/enr functions.R
\name{en_kfold_model_grid_lim}
\alias{en_kfold_model_grid_lim}
\title{en_kfold_model_grid_lim function}
\usage{
en_kfold_model_grid_lim(
  ddata,
  response_var,
  iter = 10,
  k = 10,
  num_alpha = 20,
  num_lambda = 100,
  seed = 123,
  fit_met = "accuracy",
  loo = FALSE,
  up_dn_samp = "none",
  eq_wt = FALSE,
  type_meas = "deviance",
  na_rm = TRUE,
  lr_cutoff = c(0.5),
  accuracy_modeling = FALSE,
  ties_measure = "mode",
  save_results = FALSE,
  writeout = T,
  writeout_num = num_alpha * 5,
  writeout_path =
    "I:/Lagisetty SDR Misuse/5. Identifiable Data/E. Database/treatment arm creation/treatment arm creation/enr grid writeout directory/",
  restarting = FALSE
)
}
\arguments{
\item{ddata}{data frame containing the data to be modeled}

\item{response_var}{string identifying the name of the outcome variable}

\item{iter}{the number of iterations to use}

\item{k}{the number of folds to use}

\item{seed}{the seed value for allowing results to be reproduced}

\item{fit_met}{string indicating the fit metric to be used to evaluate model performance. options are c(accuracy, auroc, logloss, f1, ppv, npv, sens, spec, bal_acc)}

\item{loo}{boolean indicating whether 'leave one out' cross validation should be used}

\item{up_dn_samp}{string indicating whether unbalanced classes should be balanced by having the smaller class upsampled to be the same size as the larger class or vice versa. can take the form 'upsamp', 'downsamp', and 'none' (default)}

\item{eq_wt}{boolean indicating whether the 0/1 classes should be balanced with weights. you may want to use this if there is a bad class imbalance}

\item{type_meas}{the 'type measure' which is passed to cv.glmnet that governs its training penalty when tuning lambda. this should match arguments expected in cv.glmnet}

\item{na_rm}{boolean indicating whether missing values should be removed. default is TRUE}

\item{lr_cutoff}{vetor of cutoff values to test/tune for optimization. the default is 'c(.5)' which is to say 'equal distance from all classes' which is typical in standard analyses}

\item{accuracy_modeling}{switch determining if we need to break ties between optimal solutions}

\item{ties_measure}{string indicating the method for breaking ties. default is 'mode' indicating that the model with the best performance across all fit metrics listed will when when model results are tied.}

\item{save_results}{boolean indicating if the unaggregated results should be saved and returned. default is FALSE as this is typically too much material to save and could crash the r session}

\item{writeout}{boolean indicating if the progress of the function should be written out in real time in order to restart if need be. the file wll contain the aggregated results up to some iteration so that the process may be restarted without having to return to i = 1}

\item{writeout_num}{the number of iterations before the results are written out to save. a higher number will move faster (beacuse it's not writing out all the time) and a lower number will move slower but will probably mean a lower number of reruns in the even of an interruption. default is alpha*5 so in the event of checking 20 alphas the function will write results every 100 iterations}

\item{writeout_path}{string of the path directory for writing out results}

\item{restarting}{boolean indicating whether the process is restarting. if it is then the function looks for a file in the 'writeout_path' and reads that in and continues from where it left off}
}
\description{
Runs k fold cross validation ENR models with simultaneous estimation of alpha, lambda, and cutoff via randomized grid (DOCUMENTATION COMING- CURRENT DOCUMENTATION INCORRECT)
}
\examples{
en_kfold_model_grid_lim()
}
\keyword{en_kfold_model_grid_lim}
\keyword{enr}
