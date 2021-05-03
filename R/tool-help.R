#' An interactive tool to find package help
#'

help_tool = function() {

  # find all documentation files
  doc_files = list.files(resource_path("04-documentation"), full.names = TRUE)

  # find which are .html output and which are .Rmd source
  html_files = doc_files[stringr::str_detect(doc_files, "\\.html$")]
  rmd_files = doc_files[stringr::str_detect(doc_files, "\\.Rmd$")]

  # extract the title line from each document
  doc_titles = unname(sapply(rmd_files, function(x) {
    contents = readLines(x)
    title_line = contents[stringr::str_detect(contents, "^title\\:")]
    title = stringr::str_remove(title_line, "^title\\: ")
    title = stringr::str_remove_all(title, '\\"')
    title = stringr::str_remove_all(title, "\\*")
  }))

  # build the html file names as a list, and give it names
  # this will be passed to a selectInput
  html_files = as.list(html_files)
  names(html_files) = doc_titles

  # find all example data files
  ex_data_files = list.files(resource_path("05-example-data"), full.names = TRUE)
  ex_data_files = as.list(ex_data_files)
  names(ex_data_files) = c(
    "Bethel Boat Harbor (BBH)",
    "Community-Based Monitoring Program (CBM)",
    "Bethel Area Fish Camps (FC)",
    "Flight Data"
  )

  # add placeholder elements to both lists
  html_files = append(list("Choose a topic" = ""), html_files)
  ex_data_files = append(list("Choose a file type:" = ""), ex_data_files)

  # USER-INTERFACE
  ui = miniUI::miniPage(

    # to allow toggleStates
    shinyjs::useShinyjs(),

    # create the title
    miniUI::gadgetTitleBar("KuskoHarvEst Help Tool"),

    # populate page with input widgets
    miniUI::miniContentPanel(
      # descriptive text
      shiny::p(shiny::em("This tool allows you to access the help documentation for how to use the features in this package to produce in-season harvest estimates from beginning to end.")),

      # header for documentation section
      shiny::h4(shiny::strong("Instructions For a Specific Topic")),

      # selector for a documentation file
      shiny::selectizeInput("html_file", label = NULL, choices = html_files, width = "60%"),

      # opener for documentation file
      shiny::actionButton("open_html_file", label = "Open the File on This Topic", icon = shiny::icon("folder-open"), class = "btn-primary"),

      # horizontal rule to separate sections
      shiny::hr(),

      # header for example data section
      shiny::h4(shiny::strong("Example Raw Data Files")),

      # selector for example data file
      shiny::selectizeInput("ex_data_file", label = NULL, choices = ex_data_files, width = "60%"),

      # opener for example data file
      shiny::actionButton("open_ex_data_file", label = "Open the Example Data File", icon = shiny::icon("folder-open"), class = "btn-primary"),

      # horizontal rule to separate sections
      shiny::hr(),

      # header for report
      shiny::h4(shiny::strong("2018 Technical Report")),
      shiny::p("The Staton (2018) report documents some of the motivators for in-season harvest estimates, data collection methods, analytical methods, and results from the 2018 season. It is recommended reading for users wishing to become familiar with these topics."),

      # clickable link to open the Staton (2018) report
      shiny::actionLink("open_2018_report", label = "View Staton (2018)", icon = shiny::icon("file-pdf"))

    )
  )

  # SERVER-SIDE OPERATIONS
  server = function(input, output, session) {

    # make buttons clickable when appropriate
    shiny::observe({
      shinyjs::toggleState("open_html_file", input$html_file != "")
      shinyjs::toggleState("open_ex_data_file", input$ex_data_file != "")
    })

    # open documentation file when requested
    shiny::observeEvent(input$open_html_file, {
      file.show(input$html_file)
    })

    # open example data file when requested
    shiny::observeEvent(input$open_ex_data_file, {
      file.show(input$ex_data_file)
    })

    # open the 2018 report when requested
    shiny::observeEvent(input$open_2018_report, {
      file.show(resource_path("04-documentation/Staton - 2018 - In-season harvest and effort estimates.pdf"))
    })

    # Handle the Done button being pressed
    shiny::observeEvent(input$done, {
      stopApp()
    })
  }

  # launch gadget
  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("", width = 700, height = 500))

}
