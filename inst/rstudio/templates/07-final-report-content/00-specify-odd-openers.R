# ::::::::::::::::::::::::::::::::::::::::::::::::::: #
# SCRIPT TO DEFINE OPENERS THAT NEED SPECIAL TREAMENT #
# ::::::::::::::::::::::::::::::::::::::::::::::::::: #

# NOTE: if no openers fell into one of these "odd" categories, leave NA
# NOTE: no flight openers still need empty data files with the correct date

# which openers had only set net data?
# EXAMPLE OF 2021: set_only_openers = c("2021-06-02", "2021-06-05", "2021-06-09")
set_only_openers = c(NA)

# which drift+set openers didn't make set net estimates?
# EXAMPLE OF 2021: no_set_openers = c("2021-07-02")
no_set_openers = c(NA)

# which had no flight data and had no harvest/effort estimates generated?
# EXAMPLE OF 2021: no_flight_openers = c("2021-07-09")
no_flight_openers = c(NA)
