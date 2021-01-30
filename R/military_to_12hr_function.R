

military.to.12hr = function(times) {
  hours = floor(times)
  decimals = times - hours
  mins = round((decimals) * 60)
  mins = as.character(mins)
  mins[mins == "0"] = "00"
  mins = ifelse(nchar(mins) == 1, paste("0", mins, sep = ""), mins)
  am.pm = ifelse(hours >= 12 & hours != 24, "PM", "AM")
  hours = ifelse(am.pm == "PM" & hours != 12, hours - 12, hours)
  times = paste(paste(hours, mins, sep = ":"),am.pm, sep = "")
  
  times = ifelse(hours == 24, "12:00AM", times)
  times
}

