#' Create a table to report the number of interviews by data source
#'
#' @export

interview_data_table = function(interview_data) {
  # count the number of interviews from each data source
  tab = sort(table(interview_data$source), decreasing = TRUE)

  # use the longer names
  names(tab) = source_names[names(tab),"source_long"]
  tab = t(t(tab))
  tab = rbind(tab, Total = sum(tab))

  # append the percent of all interviews by source
  tab = cbind(tab, Percent = percentize(tab[,1]/tab["Total",1]))

  # formatting
  tab = cbind("Data Source" = rownames(tab), tab); rownames(tab) = NULL
  colnames(tab)[2] = "Interviews"

  # build the kable
  knitr::kable(tab, "latex", linesep = "", booktabs = TRUE, longtable = FALSE, escape = TRUE, align = "lrr",
               caption = "The number and percent of fisher interviews conducted by location and organization.") %>%
    kableExtra::kable_styling(position = "center", latex_options = "HOLD_position") %>%
    kableExtra::row_spec(nrow(tab) - 1, hline_after = TRUE) %>%
    kableExtra::row_spec(nrow(tab), bold = TRUE) %>%
    kableExtra::row_spec(0, bold = TRUE)
}

