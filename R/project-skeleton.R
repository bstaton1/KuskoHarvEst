
KuskoHarvest_skeleton = function(path, set_only) {

  # create the package directory
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  # create a settings file
  settings_filename = file.path(path, "settings.txt")
  writeLines(paste("set_only:", set_only), settings_filename)

  # create subdirectories
  dir.create(file.path(path, "data"))

  TRUE
}
