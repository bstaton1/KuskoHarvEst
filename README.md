# KuskoHarvEst <img src="man/figures/KuskoHarvEst-logo.png" align="right" height=200px/>

> R package that provides efficient and interactive interfaces for creating and reporting in-season harvest and effort estimates for the Lower Kuskokwim River subsistence salmon fisheries.
>
> Its intent is to convert a process that was previously driven by "copy, paste, edit, repeat" on code files (thereby sometimes subjective and error-prone) into one that is menu-driven, intuitive, and automated. **Importantly, it requires no knowledge of R programing to conduct estimation and create the standardized reports.**

## Set Up

To use 'KuskoHarvEst', you'll need to first install two programs (in this order). Be sure to use the correct version for your operating system (for example, Mac or Windows). 

> **NOTE**: If you already have these programs, but they were installed later than in the last ~6 months, it is advised that you uninstall the older versions and install the newest versions.

* [Program R](https://cran.rstudio.com/): this is the program that runs the code, but the interface is entirely code-based and not easy to work with.
* [RStudio Desktop](https://www.rstudio.com/products/rstudio/download/): this is a program that makes working with R much easier, and allows construction of custom  interactive interfaces (drop down menus, text entry boxes, etc.).

Once you have installed RStudio, open it, paste the code below in the console and press <kbd>ENTER</kbd>. This will install   nearly everything you need to create harvest estimates, except for the data in the current year.

```R
install.packages("remotes")
remotes::install_github("bstaton1/KuskoHarvEst")
```

One more step: you'll need a LaTeX distribution to build the report documents. Rather than require installation of a complete distribution (e.g., MikTeX, which is very large and requires that you have administrative access to your computer), 'KuskoHarvEst' depends on 'tinytex'. This is an R package that installs a minimal LaTeX distribution which includes only the core functionality by default, then installs extensions as necessary (more details [here](https://yihui.org/tinytex/)). Importantly, it does not require that you have administrative access in order to install it. After the above code is done running, type this code into the console and press <kbd>ENTER</kbd>:

```R
tinytex::install_tinytex()
```

> **NOTE**: you will need a stable internet connection to complete all of these steps.

## The Workflow

You will interact with intuitive menu-driven interfaces contained in 'KuskoHarvEst' to create documents that summarize the data and results of harvest estimation for a given harvest opportunity. Your workflow for a given opportunity will be:

1. Gather the raw data files and ensure all data appear reasonable and files are formatted properly\*
2. Create a new R project and include the raw data files within it
3. Enter the "meta-data" for the opportunity (for example, date/times, gear types allowed, etc.)
4. Process the raw data files into a standardized format, including screening interviews for potentially unreliable information
5. Produce the reports (one for the main summary and one for sensitivity analyses), which are automatically populated with the inputs and outputs of each analysis

This means the only things you will edit directly are either (a) in the data files or (b) contained in a straight-forward interface. In step (4), you may find some data entry problems. If that is the case, simply close the interface and edit the data to correct the problem, and start step (4) over.

> \*Improperly formatted/entered data will be the number one cause of errors you see (hopefully you see none!), so pay extra care to this topic.

## Documentation

It is recommended that you review the documentation for 'KuskoHarvEst' before using it. Lots of information is available through the built-in "Help" tool, available through the "Addins" menu at the top of RStudio:

<p align="center">
  <img src="man/figures/readme-screenshots/addin-menu.png" width="600"/>
</p>

There are dedicated documents with step-by-step instructions and general guidance on how to use each interactive tool and more, accessible through this menu in the 'KuskoHarvEst' help tool:

<p align="center">
  <img src="man/figures/readme-screenshots/help-instructions.png" width="300"/>
</p>

Additionally, there are example data files to illustrate precisely how the individual raw data files should be formatted.  These are accessible through this menu in the 'KuskoHarvEst' help tool:

<p align="center"> 
  <img src="man/figures/readme-screenshots/help-data.png" width="300"/> 
</p>

Users new to in-season Kuskokwim River subsistence harvest monitoring and estimation are advised to read the Staton (2018) report -- it details the data collection and analytical methods. This is accessible by clicking the link in this section of the 'KuskoHarvest' help tool:

<p align="center">
  <img src="man/figures/readme-screenshots/help-report.png" width="600"/>
</p>

## Acknowledgements

Several people provided valuable feedback on the functionality included in this package, in alphabetical order they are: B. Bechtol, G. Decossas, J. Esquible, D. Lowrey, J. Spaeder, and K. Whitworth. L. Coggins co-developed the statistical foundations of the harvest/effort estimators with B. Staton starting in 2016, with earlier work done in 2015. The graphic of the fisher in the 'KuskoHarvEst' package logo was created by N. Tamburello. 

This package is totally reliant on [RStudio Desktop](https://www.rstudio.com/products/rstudio/), [Rmarkdown](https://rmarkdown.rstudio.com/), and [Shiny](https://shiny.rstudio.com/) to do its job. The software developers are owed gratitude for making the construction of intuitive workflows like those contained in 'KuskoHarvEst' possible. 

Funding for the development of this package was provided by the Kuskokwim River Inter-Tribal Fish Commission, administered through the Bering Sea Fisherman's Association through grant #AC-2101 between January and October, 2021. Funding for the initial development of the statistical and reporting framework, off of which 'KuskoHarvEst' is based, was funded by the U.S. Fish and Wildlife Service through a Pathways Position during the summers of 2016 - 2018.
