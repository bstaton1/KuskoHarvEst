# :::::::::::::::::::::::::::::::::::::::::::::::::::::::: #
# SCRIPT TO CREATE OUTPUT FOR BUILDING FINAL REPORT TABLES #
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::: #

# clear the workspace
rm(list = ls(all = TRUE))

# load the info about odd openers
source("00-specify-odd-openers.R")

# input and output directories
in_dir = "compiled-output"
out_dir = "tables-for-report"
if (!dir.exists(out_dir)) dir.create(out_dir)

