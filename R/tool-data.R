#' An interactive tool to specify opportunity meta-data
#'

data_tool = function() {

  # file paths
  proj_dir = rstudioapi::getActiveProject()
  input_data_dir = file.path(proj_dir, "data-raw")
  output_data_dir = file.path(proj_dir, "data-use")

  # load the meta data file and return an error if it is not present
  meta_file = list.files(output_data_dir, pattern = "meta\\.rds$")
  if (length(meta_file) == 0) {
    stop ("No meta-data file detected - you must run the meta-data tool before this tool.")
  } else {
    meta = readRDS(file.path(output_data_dir, meta_file))
  }

  # return an error if the data files haven't been placed in the raw data folder
  data_files = list.files(input_data_dir)
  if (length(data_files) == 0) {
    stop ("No data files found in the 'data-raw' folder.")
  }

  # find which file contains the flight data and return an error if not found
  which_flight = stringr::str_detect(data_files, "Flight_counts")
  if (!any(which_flight)) {
    stop ("No flight count data file found in the 'data-raw' folder.")
  }
  interview_files = data_files[-which(which_flight)]
  flight_files = data_files[which_flight]

  # return error if no interview data files were found
  if (length(interview_files) == 0) {
    stop ("No interview data file(s) found in the 'data-raw' folder.")
  }

  # USER-INTERFACE
  ui = miniUI::miniPage(

    # for disabling buttons until actionable
    shinyjs::useShinyjs(),

    # create the title
    miniUI::gadgetTitleBar("KuskoHarvEst Interview/Flight Data Tool"),

    miniUI::miniTabstripPanel(
      miniUI::miniTabPanel(
        "Interview Data", icon = shiny::icon("clipboard"),

        miniUI::miniTabstripPanel(
          miniUI::miniTabPanel(
            "Options", icon = shiny::icon("cog"),
            miniUI::miniContentPanel(
              shiny::fillCol(
                flex = c(1,0.5,1,2),
                # shiny::fillRow(
                  shiny::p(shiny::em("Here you will select the interview data files to include and set global options for controlling how data are used.
                       Click 'Update Global Options' first, then click 'Load Raw Data Files'.
                       Summaries will be displayed below and the data can be explored on the 'Output' tab.
                       When you are finished, click 'Save Formatted Data File'. Then, check the 'data-use' subdirectory of your project to make sure the 'options.rds' and 'interview_data.rds' files are present.")),
                # ),
                shiny::actionLink("get_help", label = "Get Help with Using This Tool", icon = shiny::icon("question-circle")),
                shiny::fillRow(
                  shiny::selectInput(inputId = "interview_files", choices = interview_files, selected = interview_files, multiple = TRUE,
                                     label = shiny::h4(shiny::strong("Choose Files to Include"), style = "margin:0;"), width = "100%")

                ),
                shiny::fillCol(
                  flex = c(0.35,1,1),
                  shiny::h4(shiny::strong("Choose Global Options"), style = "margin:0;"),
                  shiny::fillRow(
                    shiny::numericInput(inputId = "soak_sd_cut", label = "Soak Time SD Threshold", value = 3, min = 0, max = 10, step = 1),
                    shiny::numericInput(inputId = "net_length_cut", label = "Net Length Threshold", value = 350, min = 0, max = 500, step = 50),
                    shiny::numericInput(inputId = "catch_per_trip_cut", label = "Exclusion Change Threshold", value = 0.05, min = 0, max = 1, step = 0.05),
                    shiny::numericInput(inputId = "pooling_threshold", label = "Pooling Threshold", value = 10, min = 5, max = 20, step = 1)
                  ),
                  miniUI::miniButtonBlock(
                    border = NA,
                    shiny::actionButton(inputId = "update_options", label = "Update Options", icon = shiny::icon("refresh"), class = "btn-primary"),
                    shiny::actionButton(inputId = "load_interview_data", label = "Load Raw Data Files", icon = shiny::icon("upload"), class = "btn-primary"),
                    shiny::actionButton(inputId = "save_interview_data", label = "Save Formatted Data File", icon = shiny::icon("save"), class = "btn-primary")
                  )
                )
              ),
              shiny::uiOutput("stratum_gear_summary_title"),
              shiny::tableOutput("stratum_gear_summary_table"),
              shiny::uiOutput("suitable_summary_title"),
              shiny::tableOutput("suitable_summary_table")
            )
          ),

          miniUI::miniTabPanel(
            "Output", icon = shiny::icon("table"),
            miniUI::miniContentPanel(
              shiny::fillCol(
                flex = c(0.75, 3),
                shiny::fillRow(
                  shiny::selectInput(inputId = "filter_DT_source", "Data Sources to View", choices = NULL, multiple = TRUE, selectize = TRUE),
                  shiny::selectInput(inputId = "filter_DT_gear", "Gear Types to View", choices = NULL, multiple = TRUE, selectize = TRUE),
                  shiny::selectInput(inputId = "filter_DT_stratum", "Strata to View", choices = NULL, multiple = TRUE, selectize = TRUE),
                  shiny::selectInput(inputId = "filter_DT_suit", "Suitability to View", choices = c("All", "Missing Trip Times", "Missing Catch Rate Info", "Unreliable Catch Rate Info", "Unreliable Soak Time", "Unreliable Net Length", "Has a Note"), multiple = FALSE, selectize = TRUE)
                ),
                shiny::fillRow(
                  DT::dataTableOutput("interview_data_DT")
                )
              )
            )
          )
        )
      ),

      miniUI::miniTabPanel(
        "Flight Data", icon = shiny::icon("plane"),
        miniUI::miniContentPanel(
          shiny::p(shiny::em("Please be sure to complete the tasks on the interview data tab before using this tab.
                   Here you will select the correct flight data file and check it for accuracy. Click 'Load Flight Data' to
                   view the data and click 'Save Flight Data' when you are finished.")),
          shiny::selectInput(inputId = "flight_file", label = "Select the Flight Data File", choices = flight_files, multiple = FALSE),
          miniUI::miniButtonBlock(
            border = NA,
            shiny::actionButton(inputId = "load_flight_data", label = "Load Flight Data", icon = shiny::icon("upload"), class = "btn-primary"),
            shiny::actionButton(inputId = "save_flight_data", label = "Save Flight Data", icon = shiny::icon("save"), class = "btn-primary")
          ),
          shiny::uiOutput("flight_summary_text"),
          shiny::tableOutput("flight_summary_table"),
          shiny::uiOutput("trips_plot_text"),
          shiny::plotOutput("trips_plot")
        )
      )
    )
  )

  # SERVER-SIDE OPERATIONS
  server = function(input, output, session) {

    # when the "get_help" link is clicked:
    shiny::observeEvent(input$get_help, {
      file.show(resource_path("04-documentation/03-interview-flight-data-tool.html"))
    })

    # reactive container object
    vals = shiny::reactiveValues()
    vals$loadable = FALSE

    # set the global options when told and save them to a file
    shiny::observeEvent(input$update_options, {
      options(
        soak_sd_cut = input$soak_sd_cut,
        net_length_cut = input$net_length_cut,
        catch_per_trip_cut = input$catch_per_trip_cut,
        central_fn = mean,
        pooling_threshold = input$pooling_threshold
      )

      vals$options = list(soak_sd_cut = input$soak_sd_cut,
                          net_length_cut = input$net_length_cut,
                          catch_per_trip_cut = input$catch_per_trip_cut,
                          central_fn = mean,
                          pooling_threshold = input$pooling_threshold
      )

      saveRDS(vals$options, file.path(output_data_dir, paste0(file_date(meta$start_date), "_options.rds")))
      vals$loadable = TRUE
    })

    # toggle state of buttons
    shiny::observe({
      shinyjs::toggleState(id = "load_interview_data", condition = vals$loadable)
      shinyjs::toggleState(id = "save_interview_data", condition = !is.null(vals$interview_data))
    })

    # read in and prepare the interview data and populate the filters in the DT tab
    shiny::observeEvent(input$load_interview_data, {
      vals$interview_data = prepare_interviews(input_files = file.path(input_data_dir, input$interview_files), include_village = TRUE, include_goals = TRUE)
      shiny::updateSelectInput(session, "filter_DT_source", choices = unique(vals$interview_data$source), selected = unique(vals$interview_data$source))
      shiny::updateSelectInput(session, "filter_DT_gear", choices = unique(vals$interview_data$gear), selected = unique(vals$interview_data$gear))
      shiny::updateSelectInput(session, "filter_DT_stratum", choices = sort(unique(vals$interview_data$stratum)), selected = sort(unique(vals$interview_data$stratum)))
    })

    # save the formatted interview data when told
    shiny::observeEvent(input$save_interview_data, {
      saveRDS(vals$interview_data, file.path(output_data_dir, paste0(file_date(meta$start_date), "_interview_data.rds")))
    })

    # create the label for the gear/stratum summary table
    output$stratum_gear_summary_title = shiny::renderUI({
      if (!is.null(vals$interview_data)) {
        shiny::p(shiny::strong("Interviews by Stratum and Gear"), style = "margin:0;")
      } else {
        NULL
      }
    })

    # create the stratum/gear summary table
    output$stratum_gear_summary_table = shiny::renderTable({
      if (!is.null(vals$interview_data)) {
        counts = table(vals$interview_data$stratum, vals$interview_data$gear)
        colnames(counts) = capitalize(colnames(counts))
        x = strata_names[strata_names$stratum %in% rownames(counts),]
        rownames(counts) = paste0(x$stratum_start, " - ", x$stratum_end, " (", rownames(counts), ")")
        counts = rbind(counts, Total = colSums(counts))
      } else {
        NULL
      }
    }, rownames = TRUE, digits = 0)

    # create the label for the suitability summary table
    output$suitable_summary_title = shiny::renderUI({
      if (!is.null(vals$interview_data)) {
        p(strong("Unsuitable Interviews by Source"), style = "margin:0;")
      } else {
        NULL
      }
    })

    # create the suitability summary table
    output$suitable_summary_table = shiny::renderTable({
      if (!is.null(vals$interview_data)) {
        # extract only the suitability information from the data
        suitables = vals$interview_data[,c("source", "suit_effort", "suit_cr_info", "suit_cr_reliable", "suit_avg_soak", "suit_avg_net")]

        # count/format the number of interviews that were unsuitable for a purpose
        suitables = reshape2::melt(suitables, id.vars = "source")
        suitables$source = as.factor(suitables$source)
        counts = with(subset(suitables, !value), table(source, variable))
        counts = reshape2::dcast(as.data.frame(counts), variable ~ source, value.var = "Freq")
        rownames(counts) = counts$variable; counts = counts[,-1]

        # count the total number of interviews by source and combine with other counts
        totals = table(vals$interview_data$source)
        counts = rbind(counts, "Total Interviews (Suitable + Unsuitable)" = totals)

        # reformat the row names
        rownames(counts)[rownames(counts) == "suit_effort"] = "Missing Trip Times"
        rownames(counts)[rownames(counts) == "suit_cr_info"] = "Missing Catch Rate Info"
        rownames(counts)[rownames(counts) == "suit_cr_reliable"] = "Unreliable Catch Rate Info"
        rownames(counts)[rownames(counts) == "suit_avg_soak"] = "Soak Time Unreliable For Average"
        rownames(counts)[rownames(counts) == "suit_avg_net"] = "Net Length Unreliable For Average"

        counts
      } else {
        NULL
      }
    }, rownames = TRUE)

    # create the DT to allow user to explore formatted data
    output$interview_data_DT = DT::renderDataTable({
      if (!is.null(vals$interview_data)) {

        df_print = vals$interview_data

        df_print = subset(df_print, source %in% input$filter_DT_source & gear %in% input$filter_DT_gear & stratum %in% input$filter_DT_stratum)

        if (input$filter_DT_suit == "All") df_print = df_print
        if (input$filter_DT_suit == "Missing Trip Times") df_print = subset(df_print, !suit_effort)
        if (input$filter_DT_suit == "Missing Catch Rate Info") df_print = subset(df_print, !suit_cr_info)
        if (input$filter_DT_suit == "Unreliable Catch Rate Info") df_print = subset(df_print, !suit_cr_reliable)
        if (input$filter_DT_suit == "Unreliable Soak Time") df_print = subset(df_print, !suit_avg_soak)
        if (input$filter_DT_suit == "Unreliable Net Length") df_print = subset(df_print, !suit_avg_net)
        if (input$filter_DT_suit == "Has a Note") df_print = subset(df_print, !is.na(note))

        df_print = df_print[,-which(stringr::str_detect(colnames(df_print), "suit"))]
        df_print = df_print[,-which(stringr::str_detect(colnames(df_print), "goal"))]
        df_print$trip_start = short_datetime(df_print$trip_start, include_date = FALSE)
        df_print$trip_end = short_datetime(df_print$trip_end, include_date = FALSE)
        df_print$trip_duration = as.character(df_print$trip_duration)
        df_print$soak_duration = as.character(df_print$soak_duration)

        DT::datatable(df_print, rownames = FALSE, options = list(searching = FALSE, paging = FALSE), selection = "none")
      } else {
        NULL
      }
    })

    # load/prepare the flight data when instructed
    shiny::observeEvent(input$load_flight_data, {
      vals$flight_data = prepare_flights(file.path(input_data_dir, input$flight_file))
    })

    # toggle flight data buttons
    shiny::observe({
      shinyjs::toggleState("load_flight_data", !is.null(vals$interview_data))
      shinyjs::toggleState("save_flight_data", !is.null(vals$flight_data))
    })

    # save the flight data when instructed
    shiny::observeEvent(input$save_flight_data, {
      saveRDS(vals$flight_data, file.path(output_data_dir, paste0(file_date(meta$start_date), "_flight_data.rds")))
    })

    # create the text that describes the flight data summary
    output$flight_summary_text = shiny::renderUI({
      if (!is.null(vals$interview_data) & !is.null(vals$flight_data)) {
        shiny::p(shiny::em("This table displays the data from each flight: the times the flight(s) were conducted, and the number of trips by gear and geographic stratum that were counted.
             Be sure to click 'Save Flight Data' to make these data available for use later."))
      } else {
        NULL
      }
    })

    # create the flight summary data table
    output$flight_summary_table = shiny::renderTable({
      if (!is.null(vals$flight_data)) {
        print_flights = vals$flight_data
        print_flights$start_time = short_datetime(print_flights$start_time)
        print_flights$end_time = short_datetime(print_flights$end_time)
        print_flights[,-1]
      } else {
        NULL
      }
    })

    # create the trips plot descriptive text
    output$trips_plot_text = shiny::renderUI({
      if (!is.null(vals$flight_data) & !is.null(vals$interview_data) & !meta$set_only) {
        shiny::p(shiny::em("This plot shows the reported start and end times of each trip along with the times each flight was active."))
      } else {
        NULL
      }
    })

    # create the trips plot
    output$trips_plot = shiny::renderPlot({
      if (!is.null(vals$flight_data) & !is.null(vals$interview_data) & !meta$set_only) {
        make_effort_plot(vals$flight_data, estimate_effort(vals$interview_data, vals$flight_data, "drift", "dbl_exp"), trips_only = TRUE)
      } else {
        NULL
      }
    })

    # Handle the Done button being pressed
    shiny::observeEvent(input$done, {
      shiny::stopApp()
    })
  }

  # launch gadget
  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("", width = 1000, height = 550))
}
