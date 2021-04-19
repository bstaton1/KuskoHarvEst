
KuskoHarvEst_skeleton = function(path) {

  # create the package directory
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  # create subdirectories
  dir.create(file.path(path, "data-raw"))

  TRUE
}
