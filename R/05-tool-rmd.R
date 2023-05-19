#' Interactively construct PDF reports
#'
#' An RStudio add-in that guides the user through constructing and rendering
#'   the Rmarkdown source scripts for in-season estimate and sensitivity analysis reports
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

  # prepare the salmon species choices for the dropdown menu
  salmon_species = species_names$species[species_names$is_salmon]
  salmon_species_show = KuskoHarvUtils::capitalize(salmon_species)
  salmon_choices = as.list(salmon_species)
  names(salmon_choices) = salmon_species_show

  # prepare the nonsalmon species choices for the dropdown menu
  nonsalmon_species = species_names$species[!species_names$is_salmon]
  nonsalmon_species_show = KuskoHarvUtils::capitalize(nonsalmon_species)
  nonsalmon_choices = as.list(nonsalmon_species)
  names(nonsalmon_choices) = nonsalmon_species_show

  # combine
  species_choices = list(
    "Salmon" = salmon_choices,
    "Non-Salmon" = nonsalmon_choices
  )

  # create the output directory if it doesn't exist already
  if (!dir.exists(output_dir)) dir.create(output_dir)

  # USER-INTERFACE
  ui = miniUI::miniPage(

    # for disabling buttons until actionable
    shinyjs::useShinyjs(),

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
          # species
          shiny::selectizeInput(inputId = "est_species", choices = species_choices, label = shiny::h4(shiny::strong("Species")), multiple = TRUE, selected = c("chinook", "chum", "sockeye"), options = list("plugins" = list("remove_button"), placeholder = "At Least One Must be Salmon")),
          shiny::div(shiny::checkboxInput(inputId = "est_dont_split_chum_sockeye", label = "Combine Chum & Sockeye In Some Summaries?", value = FALSE),
                     style = "margin-bottom: -5px; margin-top: -10px;"),

          # gear types
          shiny::h4(shiny::strong("Gear Types")),
          shiny::div(shiny::checkboxInput(inputId = "est_do_drift", label = "Drift Nets", value = TRUE),
                     style = "margin-bottom: -10px; margin-top: -5px;"),
          shiny::div(shiny::checkboxInput(inputId = "est_do_set", label = "Set Nets", value = TRUE),
                     style = "margin-bottom: -5px; margin-top: -10px;"),

          # include content
          shiny::h4(shiny::strong("Include")),
          shiny::div(shiny::checkboxInput(inputId = "est_long_boot", label = "Full Bootstrap", value = TRUE),
                     style = "margin-bottom: -10px; margin-top: -5px;"),
          shiny::div(shiny::checkboxInput(inputId = "est_include_johnson_table", label = "Johnson R. Table", value = ifelse(meta$set_only, FALSE, TRUE)),
                     style = "margin-bottom: -10px; margin-top: -10px;"),
          # shiny::div(shiny::checkboxInput(inputId = "est_include_goal_table", label = "Harvest Goal Attainment Table", value = FALSE),
                     # style = "margin-bottom: -10px; margin-top: -10px;"),
          shiny::div(shiny::checkboxInput(inputId = "est_include_appendix", label = "Appendix Summarizing Interview Data", value = TRUE),
                     style = "margin-bottom: -10px; margin-top: -10px;"),
          shiny::div(shiny::checkboxInput(inputId = "est_draft", label = "Draft Watermark", value = TRUE),
                     style = "margin-bottom: -5px; margin-top: -10px;")
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
          # species
          shiny::selectizeInput(inputId = "sen_species", choices = salmon_choices, label = shiny::h4(shiny::strong("Species")), multiple = TRUE, selected = c("chinook", "chum", "sockeye"), options = list("plugins" = list("remove_button"), placeholder = "At Least One Must be Salmon")),
          shiny::div(shiny::checkboxInput(inputId = "sen_dont_split_chum_sockeye", label = "Combine Chum & Sockeye In Some Summaries?", value = FALSE),
                     style = "margin-bottom: -5px; margin-top: -10px;"),

          # gear types
          shiny::h4(shiny::strong("Gear Types")),
          shiny::div(shiny::checkboxInput(inputId = "sen_do_drift", label = "Drift Nets", value = TRUE),
                     style = "margin-bottom: -10px; margin-top: -5px;"),
          shiny::div(shiny::checkboxInput(inputId = "sen_do_set", label = "Set Nets", value = TRUE),
                     style = "margin-bottom: -5px; margin-top: -10px;"),

          # include
          shiny::h4(shiny::strong("Include")),
          shiny::div(shiny::checkboxInput(inputId = "sen_long_boot", label = "Full Bootstrap", value = TRUE),
                     style = "margin-bottom: -10px; margin-top: -5px;"),
          shiny::div(shiny::checkboxInput(inputId = "sen_include_plots", label = "Trip Time Plots", value = FALSE),
                     style = "margin-bottom: -5px; margin-top: -10px;"),
          shiny::div(shiny::checkboxInput(inputId = "sen_draft", label = "Draft Watermark", value = TRUE),
                     style = "margin-bottom: -5px; margin-top: -10px;")
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
      shinyjs::toggleState("est_include_johnson_table", condition = !meta$set_only & input$est_do_drift)
      shinyjs::toggleState("knit_est_rmd", condition = vals$est_knitable)

      # don't include johnson table if not doing drift nets
      if (!input$est_do_drift) {
        shiny::updateCheckboxInput(inputId = "est_include_johnson_table", value = FALSE)
      }

      # handle whether split chum/sockeye can be checked
      splitable = all(c("chum", "sockeye") %in% input$est_species)
      if (!splitable) {
        shiny::updateCheckboxInput(inputId = "est_dont_split_chum_sockeye", value = FALSE)
      }
      shinyjs::toggleState(id = "est_dont_split_chum_sockeye", condition = splitable)

      # handle whether gear types can be selected & which options are available
      if (meta$set_only) {
        shiny::updateCheckboxInput(inputId = "est_do_set", value = TRUE)
      }
      shinyjs::toggleState(id = "est_do_drift", condition = !meta$set_only)
      shinyjs::toggleState(id = "est_do_set", condition = !meta$set_only)

      # handle whether the draft watermark must be shown
      if (!input$est_long_boot) {
        shiny::updateCheckboxInput(inputId = "est_draft", value = TRUE)
      }
      shinyjs::toggleState(id = "est_draft", condition = input$est_long_boot)

      # handle whether the Rmd can be built
      # must have at least one salmon species and at least one gear selected
      shinyjs::toggleState(id = "save_est_rmd", condition = !is.null(input$est_species) & any(salmon_species %in% input$est_species))
    })

    # build the estimate report rmd when instructed
    shiny::observeEvent(input$save_est_rmd, {
      vals$est_knitable = TRUE
      vals$est_rmd_file = build_estimate_report_Rmd(
        draft = input$est_draft,
        do_drift = input$est_do_drift,
        do_set = input$est_do_set,
        n_boot = ifelse(input$est_long_boot, 1000, 100),
        species = input$est_species,
        split_chum_sockeye = !input$est_dont_split_chum_sockeye,
        include_johnson_table = input$est_include_johnson_table,
        # include_goal_table = input$est_include_goal_table,
        include_goal_table = FALSE,
        include_appendix = input$est_include_appendix,
        save_bootstrap = TRUE
      )
    })

    # knit the estimate report rmd when instructed
    shiny::observeEvent(input$knit_est_rmd, {
      est_out_file = file.path("output", stringr::str_replace(vals$est_rmd_file, "Rmd", "pdf"))
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

      # handle whether split chum/sockeye can be checked
      splitable = all(c("chum", "sockeye") %in% input$sen_species)
      if (!splitable) {
        shiny::updateCheckboxInput(inputId = "sen_dont_split_chum_sockeye", value = FALSE)
      }

      # handle whether the draft watermark must be shown
      if (!input$sen_long_boot) {
        shiny::updateCheckboxInput(inputId = "sen_draft", value = TRUE)
      }

      shinyjs::toggleState("sen_species", condition = !meta$set_only)
      shinyjs::toggleState("sen_dont_split_chum_sockeye", condition = !meta$set_only & splitable)
      shinyjs::toggleState("sen_do_drift", condition = !meta$set_only)
      shinyjs::toggleState("sen_do_set", condition = !meta$set_only)
      shinyjs::toggleState("sen_draft", condition = !meta$set_only & input$sen_long_boot)
      shinyjs::toggleState("sen_include_plots", condition = !meta$set_only)
      shinyjs::toggleState("sen_long_boot", condition = !meta$set_only)
      shinyjs::toggleState("save_sen_rmd", condition = !meta$set_only & !is.null(input$sen_species))
      shinyjs::toggleState("knit_sen_rmd", condition = !meta$set_only & vals$sen_knitable)

    })

    # build the sensitivity report rmd when instructed
    shiny::observeEvent(input$save_sen_rmd, {
      vals$sen_knitable = TRUE
      vals$sen_rmd_file = build_sensitivity_report_Rmd(
        do_drift = input$sen_do_drift,
        do_set = input$sen_do_set,
        species = input$sen_species,
        draft = input$sen_draft,
        n_boot = ifelse(input$sen_long_boot, 1000, 100),
        split_chum_sockeye = !input$sen_dont_split_chum_sockeye,
        include_plots = input$sen_include_plots
      )
    })

    # knit the sensitivity report rmd when instructed
    shiny::observeEvent(input$knit_sen_rmd, {
      sen_out_file = file.path("output", stringr::str_replace(vals$sen_rmd_file, "Rmd", "pdf"))
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
