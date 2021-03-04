#' Sample with replacement from a data set
#' @param interview_data data frame generated with `prepare_interviews()`
#'   See details.
#' @export

randomize_data = function(interview_data) {

  # sample row indices
  random_rows = sample(x = 1:nrow(interview_data), size = nrow(interview_data), replace = TRUE)

  # return the randomized output
  return(interview_data[random_rows,])
}
