# *NEWS*

# KuskoHarvEst 1.1.0 (2021-09-19)

Although the core functionality was completed by the end of May 2021, much feedback was gathered during its use in the 2021 season and many changes were introduced following the conclusion of the season. These are grouped into "user-facing" (i.e., something a user may notice relative to earlier versions) and "non user-facing" (i.e., something the user would not likely notice), and further into "major" and "minor" changes. The hyperlink next to each change references the GitHub pull request or issue that contains more detailed information on what was changed and why.

This version is considered final and may be used in future seasons assuming key features of the monitoring program do not change. Small changes to the software may be made in the future, but the core functionality will not change. All future changes will be documented here and tracked as part of the [GitHub repository](https://github.com/bstaton1/KuskoHarvEst).

## User-Facing Changes

### Major Changes

* Added documentation describing the code framework ([#129](https://github.com/bstaton1/KuskoHarvEst/pull/129))
* Added documentation describing the automated quality assurance checks performed ([#155](https://github.com/bstaton1/KuskoHarvEst/pull/155))
* Added ability to automate generation of final report content (tables and figures), accompanied by documentation for this process ([#157](https://github.com/bstaton1/KuskoHarvEst/pull/157))
* Added ability to produce non-salmon harvest estimates to be presented in an additional appendix in the in-season reports ([#160](https://github.com/bstaton1/KuskoHarvEst/pull/160))

### Minor Changes

* **BUG FIX**: issues displaying announcement numbers and links ([#133](https://github.com/bstaton1/KuskoHarvEst/pull/133))
* Discarded appendix tables displaying trip start/stop time summaries ([#135](https://github.com/bstaton1/KuskoHarvEst/pull/135))
* Added ability to summarize chum and sockeye information separately rather than in aggregate ([#136](https://github.com/bstaton1/KuskoHarvEst/pull/136))
* Improved the `note` column created after preparing raw data ([#138](https://github.com/bstaton1/KuskoHarvEst/pull/138))
* Replaced "Special Action" terminology with "announcement" terminology at the top of output documents ([#140](https://github.com/bstaton1/KuskoHarvEst/pull/140))
* Included functionality to ensure all reported percentages sum to 100% exactly where appropriate ([#159](https://github.com/bstaton1/KuskoHarvEst/pull/159), [#168](https://github.com/bstaton1/KuskoHarvEst/pull/168))
* The meta-data tool now closes automatically upon clicking the "save" button ([#164](https://github.com/bstaton1/KuskoHarvEst/pull/164))
* All documentation was updated and proof-read for errors and ease of interpretation ([#167](https://github.com/bstaton1/KuskoHarvEst/pull/167))

## Non User-Facing Changes

### Major Changes

* Improvements to the consistency in checking raw data for errors that caused problems in the 2021 season ([#131](https://github.com/bstaton1/KuskoHarvEst/pull/131))
  * Spaces in character fields that should not be present are now removed ([#117](https://github.com/bstaton1/KuskoHarvEst/issues/117))
  * Empty rows in flight data are now removed prior to further processing ([#126](https://github.com/bstaton1/KuskoHarvEst/issues/126))
  * No warnings are now returned when reading in raw CSV files ([#127](https://github.com/bstaton1/KuskoHarvEst/issues/127))
  * Interviews that have strata that are not part of the study area (e.g., area O) are now automatically discarded ([#128](https://github.com/bstaton1/KuskoHarvEst/issues/128))
* The set net effort estimator was updated to ensure the effort estimate is at least as large as the number of set net interviews gathered ([#134](https://github.com/bstaton1/KuskoHarvEst/pull/134))

### Minor Changes

* Added dependency on the '[yaml]([https://CRAN.R-project.org/package=yaml](https://cran.r-project.org/package=yaml))' R package to simplify the code in `build_yaml()` ([#141](https://github.com/bstaton1/KuskoHarvEst/pull/141))
* **BUG FIX**: updated detection of soak time outliers to handle the case where only one interview of a given gear type is available ([#143](https://github.com/bstaton1/KuskoHarvEst/pull/143))
* A `date` argument to `report()` was added to help with post-season summarization ([#145](https://github.com/bstaton1/KuskoHarvEst/pull/145))
* Added flexibility to histograms ([#152](https://github.com/bstaton1/KuskoHarvEst/pull/152))
* An unnecessary function (`unlist_dfs()`) was removed ([#153](https://github.com/bstaton1/KuskoHarvEst/pull/153))
* **BUG FIX**: typo in `note` column corrected for impossible trip times ([#154](https://github.com/bstaton1/KuskoHarvEst/pull/154))
* An informative error message will be returned if the supplied data source name is not recognized ([#165](https://github.com/bstaton1/KuskoHarvEst/pull/165))

# KuskoHarvEst 1.0.2 (2021-07-19)

* Fixed small bug in `make_effort_plot()` that caused a failure to plot when no flight data available ([#125](https://github.com/bstaton1/KuskoHarvEst/pull/125))

# KuskoHarvEst 1.0.1 (2021-06-03)

* Fixed small bug in appendix tables dealing with soak time and trip duration ([#115](https://github.com/bstaton1/KuskoHarvEst/pull/115))

# KuskoHarvEst 1.0.0 (2021-06-01)

* First complete version of package, to be used in 2021 season.
* Package contains complete functionality to perform in-season harvest and effort estimation for one day of fishing conditional on the sampling program operating as it has done since 2016.
* The user interacts with a project template and three interactive RStudio add-ins to generate the estimates and build their reports in-season.
* No editing of code is required on the part of the user.
* The package is complete with detailed documentation to guide the user through how to format the data files and use each of the tools.
