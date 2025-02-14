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


#' match any function - PARTIAL
#'
#' An extension of '%in%' that cycles through every element of a list/vector and evaluates if there is a partial or full match to a different list/vector
#' @param pattern the list/vector of patterns to match
#' @param list the list/vector that will be compared against for matches
#' @keywords '%rina%' in any match list
#' @export
#' @examples c(1,2,3,4,5) %rin% c(5,6,7,8,9,10,11,12) == TRUE TRUE FALSE FALSE TRUE | c(5,6,7,8,9,10,11,12) %rin% c(1,2,3,4,5) TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#' %rinp%
'%rina%'<-function(pattern,list){
  vapply(pattern, function (p) any(grepl(p,list)),logical(1L), USE.NAMES = FALSE)
}



#' match any function - EXACT
#'
#' An extension of '%in%' that cycles through every element of a list/vector and evaluates if there is an exact match to a different list/vector
#' @param pattern the list/vector of patterns to match
#' @param list the list/vector that will be compared against for matches
#' @keywords '%rine%' in any match list
#' @export
#' @examples c(1,2,3,4,5) %rin% c(5,6,7,8,9,10,11,12) == TRUE TRUE FALSE FALSE TRUE | c(5,6,7,8,9,10,11,12) %rin% c(1,2,3,4,5) TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#' %rin%
'%rine%'<-function(pattern,list){
  vapply(pattern, function (p) any(p == list),logical(1L), USE.NAMES = FALSE)
}





#' update_timer_simple function
#'
#' A simple update timer for elapsed time in a for loop
#' @param start_time A 'Sys.time()' object documenting the time the loop started
#' @keywords time timer progress
#' @export
#' @examples
#' update_timer_simple()
#'
update_timer_simple <- function(start_time) {
  elapsed_time <- difftime(Sys.time(), start_time, units = "hours")
  cat(sprintf("\rElapsed Time: %.2f hours", as.numeric(elapsed_time)))
  flush.console() # Ensure the output gets flushed to the console
}


#' format_time_difference function
#'
#' A function for formatting a time difference object into 'hours - minutes - seconds
#' @param difftime_obj A time difference object
#' @keywords time timer progress format
#' @export
#' @examples
#' format_time_difference()
#'
format_time_difference <- function(difftime_obj) {
  total_seconds <- as.numeric(difftime_obj, units = "secs")
  hours <- total_seconds %/% 3600
  minutes <- (total_seconds %% 3600) %/% 60
  seconds <- total_seconds %% 60
  sprintf("Hours: %02d, Minutes: %02d, Seconds: %05.2f", hours, minutes, seconds)
}



#' update_timer function
#'
#' A more advanced update timer for elapsed time in a for loop
#' @param start_time A 'Sys.time()' object documenting the time the loop started
#' @param cur_time A 'Sys.time()' object documenting the current time. default is current system time
#' @param cur_iter The current iteration of the for loop
#' @param total_iter The total number of iterations of the loop
#' @keywords time timer progress
#' @export
#' @examples
#' update_timer()
#'
# Progress indicator update
update_timer <- function(start_time,cur_time=Sys.time(),cur_iter,total_iter) {

  time_elapsed <- as.numeric((cur_time-start_time), units = "secs")
  hours <- time_elapsed %/% 3600
  minutes <- (time_elapsed %% 3600) %/% 60
  seconds <- time_elapsed %% 60

  perc_compl<-cur_iter/total_iter
  amnt_left<-(total_iter-cur_iter)
  perc_left<-(total_iter-cur_iter)/total_iter

  avg_seconds_per_unit<-time_elapsed/cur_iter
  #seconds_avg <- avg_seconds_per_unit %% 60

  time_remaining<-avg_seconds_per_unit*amnt_left

  hours_rem <- time_remaining %/% 3600
  minutes_rem <- (time_remaining %% 3600) %/% 60
  seconds_rem <- time_remaining %% 60

  elapsed_time <- difftime(Sys.time(), start_time, units = "hours")

  cat(sprintf("\rIteration - %i | Elapsed Time - Hours: %02d Minutes: %02d Seconds: %05.2f | Average Secs: %05.2f | Estimated Time Remaining - Hours: %02d Minutes: %02d Seconds: %05.2f", cur_iter, hours, minutes, seconds, avg_seconds_per_unit, hours_rem, minutes_rem, seconds_rem))
  flush.console() # Ensure the output gets flushed to the console

  # cat(sprintf("\nElapsed Time - Hours: %02d Minutes: %02d Seconds: %05.2f", hours, minutes, seconds))
  # flush.console() # Ensure the output gets flushed to the console
  # cat(sprintf("\nAverage Secs: %05.2f", avg_seconds_per_unit))
  # flush.console() # Ensure the output gets flushed to the console
  # cat(sprintf("\nEstimated Time Remaining - Hours: %02d Minutes: %02d Seconds: %05.2f", hours_rem, minutes_rem, seconds_rem))
  # flush.console() # Ensure the output gets flushed to the console
}
