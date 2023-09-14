# *NEWS*

# KuskoHarvEst 1.2.2 (2023-07-08)

## Internal (Not User-Facing)

* Moved `kable_replace()` from inside the body of `KuskoHarvEst:::make_harvest_sensitivity_table()` to the 'KuskoHarvUtils' package -- it likely has more uses and should be more widely accessible.
* Removed the 'magrittr' package as a dependency, and replaced all uses of the `%>%` pipe with the native `|>` pipe operator. 

# KuskoHarvEst 1.2.1 (2023-06-06)

* Removed BFSA from the list of data-collecting organizations.

# KuskoHarvEst 1.2.0 (2023-05-29)

## New Features: User-Facing

* Estimation for coho salmon now allowed ([#178](https://www.github.com/bstaton1/KuskoHarvEst/issues/178))
* Estimation for geographic stratum D2 is now allowed ([#213](https://www.github.com/bstaton1/KuskoHarvEst/pull/213))
* Missing strata are now allowed ([#213](https://www.github.com/bstaton1/KuskoHarvEst/pull/213))
* Interfaces are overall simpler ([#217](https://www.github.com/bstaton1/KuskoHarvEst/pull/217)), for example:
  * User no longer needs to enter the spatial areas in the meta data tool, these are now automatically pulled from flight data
  * Slider to select bootstrap samples is now a checkbox: long or not
  * Gear selection: turn on or off either net type if not a set net only opener; otherwise force set nets only
  * Species selector
* Introduced `KuskoHarvEst:::install_TeX()` to simplify installing LaTeX dependencies ([#220](https://www.github.com/bstaton1/KuskoHarvEst/pull/220))
* All documentation updated, including the package README, R help files, and the long form 'vignettes'

## New Features: Internal

* Creation of flexible `KuskoHarvEst:::effort_bullets()` ([#218](https://www.github.com/bstaton1/KuskoHarvEst/pull/218)) and `KuskoHarvEst:::harvest_bullets()` ([#201](https://www.github.com/bstaton1/KuskoHarvEst/issues/201))
* Major reduction in the number of `.Rmd` source files ([#219](https://www.github.com/bstaton1/KuskoHarvEst/pull/2))
* Large amount of generality adjustments to accept varying combinations of species ([#197](https://www.github.com/bstaton1/KuskoHarvEst/pull/197), [#198](https://www.github.com/bstaton1/KuskoHarvEst/pull/198), [#205](https://www.github.com/bstaton1/KuskoHarvEst/pull/205), [#206](https://www.github.com/bstaton1/KuskoHarvEst/pull/206), [#208](https://www.github.com/bstaton1/KuskoHarvEst/208))
* Simplify combining bootstrap samples ([#194](https://www.github.com/bstaton1/KuskoHarvEst/pull/194))
* Updated effort estimator ([#182](https://www.github.com/bstaton1/KuskoHarvEst/pull/182))
* Moved utility functions ([#181](https://www.github.com/bstaton1/KuskoHarvEst/pull/181))
* Files reorganized to prevent warnings upon installation regarding portability and paths exceeding 100 bytes ([#224](https://www.github.com/bstaton1/KuskoHarvEst/pull/224))

# KuskoHarvEst 1.1.1 (2022-05-04)

## Bug Fixes

* Fixed issue causing sensitivity document to fail ([#174](https://www.github.com/bstaton1/KuskoHarvEst/pull/174))

## New Features

* Added the Staton (2021) 'KuskoHarvEst' project summary report to the package, and included a link in the help tool for easy access ([#173](https://www.github.com/bstaton1/KuskoHarvEst/pull/173))

# KuskoHarvEst 1.1.0 (2021-09-19)

Although the core functionality was completed by the end of May 2021, much feedback was gathered during its use in the 2021 season and many changes were introduced following the conclusion of the season.
These are grouped into "user-facing" (i.e., something a user may notice relative to earlier versions) and "non user-facing" (i.e., something the user would not likely notice), and further into "major" and "minor" changes.
The hyperlink next to each change references the GitHub pull request or issue that contains more detailed information on what was changed and why.

This version is considered final and may be used in future seasons assuming key features of the monitoring program do not change.
Small changes to the software may be made in the future, but the core functionality will not change.
All future changes will be documented here and tracked as part of the [GitHub repository](https://github.com/bstaton1/KuskoHarvEst).

## User-Facing Changes

### Major Changes

* Added documentation describing the code framework ([#129](https://www.github.com/bstaton1/KuskoHarvEst/pull/129))
* Added documentation describing the automated quality assurance checks performed ([#155](https://www.github.com/bstaton1/KuskoHarvEst/pull/155))
* Added ability to automate generation of final report content (tables and figures), accompanied by documentation for this process ([#157](https://www.github.com/bstaton1/KuskoHarvEst/pull/157))
* Added ability to produce non-salmon harvest estimates to be presented in an additional appendix in the in-season reports ([#160](https://www.github.com/bstaton1/KuskoHarvEst/160))

### Minor Changes

* **BUG FIX**: issues displaying announcement numbers and links ([#133](https://www.github.com/bstaton1/KuskoHarvEst/pull/133))
* Discarded appendix tables displaying trip start/stop time summaries ([#135](https://www.github.com/bstaton1/KuskoHarvEst/pull/135))
* Added ability to summarize chum and sockeye information separately rather than in aggregate ([#136](https://www.github.com/bstaton1/KuskoHarvEst/pull/136))
* Improved the `note` column created after preparing raw data ([#138](https://www.github.com/bstaton1/KuskoHarvEst/pull/138))
* Replaced "Special Action" terminology with "announcement" terminology at the top of output documents ([#140](https://www.github.com/bstaton1/KuskoHarvEst/pull/140))
* Included functionality to ensure all reported percentages sum to 100% exactly where appropriate ([#159](https://www.github.com/bstaton1/KuskoHarvEst/pull/159), [#168](https://www.github.com/bstaton1/KuskoHarvEst/pull/168))
* The meta-data tool now closes automatically upon clicking the "save" button ([#164](https://www.github.com/bstaton1/KuskoHarvEst/pull/164))
* All documentation was updated and proof-read for errors and ease of interpretation ([#167](https://www.github.com/bstaton1/KuskoHarvEst/pull/167))

## Non User-Facing Changes

### Major Changes

* Improvements to the consistency in checking raw data for errors that caused problems in the 2021 season ([#131](https://www.github.com/bstaton1/KuskoHarvEst/pull/131))
  * Spaces in character fields that should not be present are now removed ([#117](https://www.github.com/bstaton1/KuskoHarvEst/issues/117))
  * Empty rows in flight data are now removed prior to further processing ([#126](https://www.github.com/bstaton1/KuskoHarvEst/issues/126))
  * No warnings are now returned when reading in raw CSV files ([#127](https://www.github.com/bstaton1/KuskoHarvEst/issues/127))
  * Interviews that have strata that are not part of the study area (e.g., area O) are now automatically discarded ([#128](https://www.github.com/bstaton1/KuskoHarvEst/isues/128))
* The set net effort estimator was updated to ensure the effort estimate is at least as large as the number of set net interviews gathered ([#134](https://www.github.com/bstaton1/KuskoHarvEst/pull/134))

### Minor Changes

* Added dependency on the '[yaml]([https://CRAN.R-project.org/package=yaml](https://cran.r-project.org/package=yaml))' R package to simplify the code in `build_yaml()` ([#141](https://www.github.com/bstaton1/KuskoHarvEst/pull/141))
* **BUG FIX**: updated detection of soak time outliers to handle the case where only one interview of a given gear type is available ([#143](https://www.github.com/bstaton1/KuskoHarvEst/pull/143))
* A `date` argument to `report()` was added to help with post-season summarization ([#145](https://www.github.com/bstaton1/KuskoHarvEst/pull/145))
* Added flexibility to histograms ([#152](https://www.github.com/bstaton1/KuskoHarvEst/pull/152))
* An unnecessary function (`unlist_dfs()`) was removed ([#153](https://www.github.com/bstaton1/KuskoHarvEst/pull/153))
* **BUG FIX**: typo in `note` column corrected for impossible trip times ([#154](https://www.github.com/bstaton1/KuskoHarvEst/pull/154))
* An informative error message will be returned if the supplied data source name is not recognized ([#165](https://www.github.com/bstaton1/KuskoHarvEst/pull/165))

# KuskoHarvEst 1.0.2 (2021-07-19)

* Fixed small bug in `make_effort_plot()` that caused a failure to plot when no flight data available ([#125](https://www.github.com/bstaton1/KuskoHarvEst/pull/125))

# KuskoHarvEst 1.0.1 (2021-06-03)

* Fixed small bug in appendix tables dealing with soak time and trip duration ([#115](https://www.github.com/bstaton1/KuskoHarvEst/pull/115))

# KuskoHarvEst 1.0.0 (2021-06-01)

* First complete version of package, to be used in 2021 season.
* Package contains complete functionality to perform in-season harvest and effort estimation for one day of fishing conditional on the sampling program operating as it has done since 2016.
* The user interacts with a project template and three interactive RStudio add-ins to generate the estimates and build their reports in-season.
* No editing of code is required on the part of the user.
* The package is complete with detailed documentation to guide the user through how to format the data files and use each of the tools.
