summ = function(x, p = c(0.5, 0.025, 0.975), rnd = NULL, na.rm = F, prettify = FALSE) {

  # calculate the main summary
  out = c(mean = mean(x, na.rm = na.rm),
          sd = sd(x, na.rm = na.rm),
          quantile(x, p)
  )

  # if rounding, do it
  if (!is.null(rnd)) {
    out = round(out, rnd)
  }

  # if prettifying, do it
  if(prettify) {
    out = prettify(out)
  }

  # return the output
  out
}

# x = rnorm(100, 50000, 2500)
#
# summ(x, rnd = -3, prettify = T)

# rand.summ = function(x, pretty = F, rnd = -1, method = "quantile") {
#
#   if (method == "quantile") {
#     summ = round(c(mean = mean(x, na.rm = T), quantile(x, c(0.025, 0.975), na.rm = T)),rnd)
#   }
#
#   if (method == "delta") {
#     x.bar = mean(x, na.rm = T)
#     delta = x.bar - x
#
#     ci = unname(sort(x.bar - quantile(delta, c(0.025, 0.975), na.rm = T)))
#
#     summ = c(x.bar, ci)
#     names(summ) = c("mean", "2.5%", "97.5%")
#   }
#
#   if(pretty) {
#     summ = prettyNum(summ, big.mark = ",", scientific = F)
#   }
#
#   summ
# }

prettify = function(x) {
  prettyNum(x, big.mark = ",", scientific = F)
}
