#' Sample with replacement from a data set
#' @param interview_data data frame generated with `prepare_interviews()`
#' @param source_weights named vector representing sampling weights for different interview data sources.
#'   See details.
#' @details Some details here about the sample weights.
#' @export

randomize_data = function(interview_data, source_weights = NULL) {

  # handle resampling probabilities
  interview_data$weight = 1

  # if the weights are not null, set them
  if (!is.null(source_weights)) {
    for (s in 1:length(source_weights)) {
      interview_data[interview_data$source == names(source_weights)[s],"weight"] = source_weights[s]
    }
  }

  # remove any records with weight of zero
  interview_data = interview_data[interview_data$weight != 0,]

  # sample row indices
  random_rows = sample(x = 1:nrow(interview_data), size = nrow(interview_data), replace = TRUE, prob = interview_data$weight)

  # return the randomized output
  return(interview_data[random_rows,])
}
