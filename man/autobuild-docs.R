
# SCRIPT TO RENDER ALL RMD DOCUMENTATION FILES

# get the current working directory
the_dir = getwd()

# set the working directory to the location of the documentation source files
doc_dir = "inst/rstudio/templates/project/resources/04-documentation"
setwd(doc_dir)

# get the names of the Rmd source files located there
rmd_files = list.files(pattern = "\\.Rmd$")

# loop through files, rendering each one
junk = sapply(rmd_files, rmarkdown::render)

# reset the working directory to the original location
setwd(the_dir)
