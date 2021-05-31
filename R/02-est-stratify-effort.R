#' Apportion total effort estimate to geographic
#'
#' Applies a simple rule to stratify the total effort estimate for a gear type
#'
#' @inheritParams estimate_effort
#' @param effort_est Numeric; the total effort estimate
#' @details Calculates the average proportion of effort counted via air in each stratum then multiply by the total effort estimate to apportion
#'

stratify_effort = function(flight_data, gear, effort_est) {

  # STEP 1: count up total effort counted during each flight
  flight_counts = flight_data[,stringr::str_detect(colnames(flight_data), gear)]

  # STEP 2: calculate the proportion of total effort that was counted in each stratum on each flight
  p_stratum = t(apply(flight_counts, 1, function(x) x/sum(x)))

  # STEP 3: calculate the average proportion in each stratum across flights
  ave_p_stratum = unname(colMeans(p_stratum))

  # STEP 4: multiply this by the total effort estimate
  stratified_effort_est = round(effort_est * ave_p_stratum)

  # STEP 5: give the elements nice names for the stratum
  stratum_names = colnames(flight_counts)
  stratum_names = stringr::str_remove(stratum_names, paste0("_", gear))
  names(stratified_effort_est) = stratum_names

  # return the output
  return(stratified_effort_est)
}
