#' An interactive tool to specify opportunity meta-data
#'

rmd_tool = function() {

  # file paths
  proj_dir = rstudioapi::getActiveProject()
  input_data_dir = file.path(proj_dir, "data-raw")
  output_data_dir = file.path(proj_dir, "data-use")
  output_dir = file.path(proj_dir, "output")

  # load the meta data file and return an error if it is not present
  meta_file = list.files(output_data_dir, pattern = "meta\\.rds$")
  if (length(meta_file) == 0) {
    stop ("No meta-data file detected - you must run the meta-data tool before this tool.")
  } else {
    meta = readRDS(file.path(output_data_dir, meta_file))
  }

  # load the interview data file and return an error if it is not present
  interview_file = list.files(output_data_dir, pattern = "interview_data")
  if (length(interview_file) == 0) {
    stop ("No interview data file detected - you must run the interview/flight data tool before this tool.")
  } else {
    interview_data = readRDS(file.path(output_data_dir, interview_file))
  }

  # load the flight data file and return an error if it is not present
  flight_file = list.files(output_data_dir, pattern = "flight_data")
  if (length(flight_file) == 0) {
    stop ("No flight data file detected - you must run the interview/flight data tool before this tool.")
  } else {
    flight_data = readRDS(file.path(output_data_dir, flight_file))
  }

  # create the output directory if it doesn't exist already
  if (!dir.exists(output_dir)) dir.create(output_dir)

  # USER-INTERFACE
  ui = miniUI::miniPage(

    # for disabling buttons until actionable
    shinyjs::useShinyjs(),
    suppressWarnings(tippy::use_tippy()),

    # create the title
    miniUI::gadgetTitleBar("KuskoHarvEst Report Builder Tool"),

    # interface layout
    miniUI::miniTabstripPanel(

      # tab for in-season reports
      miniUI::miniTabPanel(
        "In-Season Report Builder", icon = shiny::icon("tools"),
        miniUI::miniContentPanel(
          shiny::h3(shiny::strong("In-season Estimate Report Builder"), style = "margin:0;"),
          shiny::p(shiny::em("Here you will select some options for what to include in the in-season harvest report, create the source code file, and render it to a PDF.
                             After you click 'Build PDF Report', you will see the calculation progress in the backgound -- this takes some time. The PDF will automatically display when complete.",
                             shiny::strong("If you wish to use different settings after you have already built the PDF, please close the PDF file first."))),
          shiny::actionLink("est_get_help", label = "Get Help with Using This Tool", icon = shiny::icon("question-circle")),

          # widgets for estimate report control
          shiny::sliderInput(inputId = "est_n_boot", label = "Number of Bootstrap Samples", value = 1000, min = 100, max = 1000, step = 100),
          shiny::checkboxInput(inputId = "est_draft", label = "Mark as Draft", value = TRUE),
          shiny::checkboxInput(inputId = "est_do_setnets", label = "Make Set Net Harvest Estimate", value = TRUE),
          shiny::checkboxInput(inputId = "est_include_johnson_table", label = "Include Johnson River Proximity Table", value = ifelse(meta$set_only, FALSE, TRUE)),
          shiny::checkboxInput(inputId = "est_include_goal_table", label = "Include Harvest Goal Attainment Table", value = TRUE),
          shiny::checkboxInput(inputId = "est_include_appendix", label = "Include Detailed Appendix", value = TRUE)
        ),

        # buttons for estimate report
        miniUI::miniButtonBlock(
          shiny::actionButton(inputId = "save_est_rmd", label = "Save Rmarkdown Source File", icon = shiny::icon("save"), class = "btn-primary"),
          shiny::actionButton(inputId = "knit_est_rmd", label = "Build PDF Report", icon = shiny::icon("file-pdf"), class = "btn-primary")
        )
      ),

      # tab for sensitivity report
      miniUI::miniTabPanel(
        "Sensitivity Analysis Report Builder", icon = shiny::icon("tools"),
        miniUI::miniContentPanel(
          shiny::h3(shiny::strong("Sensitivity Analysis Report Builder"), style = "margin:0;"),
          shiny::p(shiny::em("Here you will select some options for what to include in the sensitivity analysis report, create the source code file, and render it to a PDF.
                             After you click 'Build PDF Report', you will see the calculation progress in the backgound -- this takes some time (possibly several minutes). The PDF will automatically display when complete.",
                             shiny::strong("If you wish to use different settings after you have already built the PDF, please close the PDF file first."))),
          shiny::actionLink("sen_get_help", label = "Get Help with Using This Tool", icon = shiny::icon("question-circle")),

          # returns a message saying report can't be built if it is a set net only opener
          shiny::uiOutput("sen_setonly_message"),

          # widgets for sensitivity report control
          shiny::sliderInput(inputId = "sen_n_boot", label = "Number of Bootstrap Samples", value = 1000, min = 100, max = 1000, step = 100),
          shiny::checkboxInput(inputId = "sen_draft", label = "Mark as Draft", value = ifelse(meta$set_only, FALSE, TRUE)),
          shiny::checkboxInput(inputId = "sen_do_setnets", label = "Make Set Net Harvest Estimate", value = ifelse(meta$set_only, FALSE, TRUE)),
          shiny::checkboxInput(inputId = "sen_include_plots", label = "Include Trip Time Plots", value = FALSE)
        ),

        # buttons for sensitivity report
        miniUI::miniButtonBlock(
          shiny::actionButton(inputId = "save_sen_rmd", label = "Save Rmarkdown Source File", icon = shiny::icon("save"), class = "btn-primary"),
          shiny::actionButton(inputId = "knit_sen_rmd", label = "Build PDF Report", icon = shiny::icon("file-pdf"), class = "btn-primary")
        )
      )
    )
  )

  # SERVER-SIDE OPERATIONS
  server = function(input, output, session) {

    # when the "get_help" link is clicked:
    shiny::observeEvent(input$est_get_help, {
      file.show(resource_path("04-documentation/04-report-builder-tool.html"))
    })
    shiny::observeEvent(input$sen_get_help, {
      file.show(resource_path("04-documentation/04-report-builder-tool.html"))
    })

    # reactive container/values
    vals = shiny::reactiveValues()
    vals$est_knitable = FALSE
    vals$sen_knitable = FALSE

    # toggles for estimate report tab
    shiny::observe({
      shinyjs::toggleState("est_do_setnets", condition = !meta$set_only)
      shinyjs::toggleState("est_include_johnson_table", condition = !meta$set_only)
      shinyjs::toggleState("knit_est_rmd", condition = vals$est_knitable)
    })

    # build the estimate report rmd when instructed
    shiny::observeEvent(input$save_est_rmd, {
      vals$est_knitable = TRUE
      vals$est_rmd_file = KuskoHarvEst:::build_estimate_report_Rmd(
        draft = input$est_draft,
        do_setnets = input$est_do_setnets,
        n_boot = input$est_n_boot,
        include_johnson_table = input$est_include_johnson_table,
        include_goal_table = input$est_include_goal_table,
        include_appendix = input$est_include_appendix,
        save_bootstrap = TRUE
      )
    })

    # knit the estimate report rmd when instructed
    shiny::observeEvent(input$knit_est_rmd, {
      est_out_file = file.path(output_dir, stringr::str_replace(vals$est_rmd_file, "Rmd", "pdf"))
      rmarkdown::render(input = vals$est_rmd_file,
                        output_file = est_out_file,
                        envir = new.env())

      # remove variables assigned to the global environment with <<-
      # this is a hack needed to ensure these objects are always available to report(), no matter how deeply it is buried in the function hierarchy
      if (!meta$set_only) {
        rm(list = c("boot_out", "set_effort_info", "drift_effort_info"), envir = globalenv())
      } else {
        rm(list = c("boot_out", "set_effort_info"), envir = globalenv())
      }
      file.show(est_out_file)
    })

    # build a message for set net only openers saying that the sensitivity report can't be produced
    output$sen_setonly_message = renderUI({
      if (meta$set_only) {
        shiny::p(shiny::strong("This report cannot be produced if the opportunity was for set nets only."))
      } else {
        NULL
      }
    })

    # toggles for sensitivity report tab
    shiny::observe({
      shinyjs::toggleState("sen_draft", condition = !meta$set_only)
      shinyjs::toggleState("sen_n_boot", condition = !meta$set_only)
      shinyjs::toggleState("sen_do_setnets", condition = !meta$set_only)
      shinyjs::toggleState("sen_include_plots", condition = !meta$set_only)
      shinyjs::toggleState("save_sen_rmd", condition = !meta$set_only)
      shinyjs::toggleState("knit_sen_rmd", condition = !meta$set_only & vals$sen_knitable)
    })

    # build the sensitivity report rmd when instructed
    shiny::observeEvent(input$save_sen_rmd, {
      vals$sen_knitable = TRUE
      vals$sen_rmd_file = KuskoHarvEst:::build_sensitivity_report_Rmd(
        draft = input$sen_draft,
        do_setnets = input$sen_do_setnets,
        n_boot = input$sen_n_boot,
        include_plots = input$sen_include_plots
      )
    })

    # knit the sensitivity report rmd when instructed
    shiny::observeEvent(input$knit_sen_rmd, {
      sen_out_file = file.path(output_dir, stringr::str_replace(vals$sen_rmd_file, "Rmd", "pdf"))
      rmarkdown::render(input = vals$sen_rmd_file,
                        output_file = sen_out_file,
                        envir = new.env())
      file.show(sen_out_file)
    })

    # Handle the Done button being pressed
    shiny::observeEvent(input$done, {
      shiny::stopApp()
    })
  }

  # launch gadget
  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("", width = 700, height = 500))
}
