#' Sample with replacement from a data set
#'
#' Samples with replacement from a data frame containing interview data
#'
#' @inheritParams estimate_harvest
#'

randomize_data = function(interview_data) {

  # sample row indices
  random_rows = sample(x = 1:nrow(interview_data), size = nrow(interview_data), replace = TRUE)

  # return the randomized output
  return(interview_data[random_rows,])
}
