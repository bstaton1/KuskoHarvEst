#' An interactive tool to specify opportunity meta-data
#'
#' @import shiny miniUI shinyTime
#'

meta_tool = function() {

  proj_dir = rstudioapi::getActiveProject()
  output_data_dir = file.path(proj_dir, "data-use")
  if(!dir.exists(output_data_dir)) dir.create(output_data_dir)

  # USER-INTERFACE
  ui = miniPage(

    # create the title
    gadgetTitleBar("KuskoHarvEst Meta-Data Entry Tool"),

    # populate page with input widgets
    miniContentPanel(
      fillCol(
        flex = c(2,3,3,3,3,1),

        # descriptive text
        fillRow(
          p(em("Here you will enter some information that helps identify the estimates you will produce later.
                    When you are done, be sure to click save to prevent needing to do this again."))
        ),

        # opportunity date/times
        fillRow(
          flex = c(2,1,1),
          dateInput(inputId = "date", label = "Day of Opportunity",
                    value = lubridate::today(), format = "mm-dd-yyyy"),
          timeInput(inputId = "start_time", label = "Start Time (24-hr)", seconds = FALSE, value = strptime("12:00", "%R")),
          timeInput(inputId = "end_time", label = "End Time (24-hr)", seconds = FALSE, value = strptime("23:59", "%R"))
        ),

        # estimate spatial coverage
        fillRow(
          textInput(inputId = "downstream_end", label = "Downstream Boundary", value = "Tuntutuliak"),
          textInput(inputId = "upstream_end", label = "Upstream Boundary", value = "Akiak")
        ),

        # special action identifiers
        fillRow(
          flex = c(1,2,2),
          textInput(inputId = "spact_name", label = "Special Action #", placeholder = "Optional"),
          textInput(inputId = "spact_url", label = "Special Action URL", placeholder = "Optional"),
          textInput(inputId = "spact_news_url", label = "News Release URL", placeholder = "Optional")
        ),

        # contact information
        fillRow(
          textInput(inputId = "contact_persons", label = "Contact Person(s) (Optional)", width = "100%",
                    placeholder = "E.g., Person 1 (p1@email.com), Person 2 (p2@email.com)")
        ),

        # is the opener set-net only?
        fillRow(
          checkboxInput(inputId = "set_only", label = "Set Nets Only?", value = FALSE)
        )
      ),
    ),

    # button to export the information to a .rds file
    miniButtonBlock(
      actionButton(inputId = "save_meta", label = "Save", icon = icon("save"), class = "btn-primary")
    )
  )

  # SERVER-SIDE OPERATIONS
  server = function(input, output, session) {

    # when the "save" button is clicked:
    observeEvent(input$save_meta, {

      # combine the date and time into the start_date
      start_date = lubridate::as_datetime(strftime(input$start_time), tz = "US/Alaska")
      lubridate::date(start_date) = input$date

      # combine the date and time into the end_date
      end_date = lubridate::as_datetime(strftime(input$end_time), tz = "US/Alaska")
      lubridate::date(end_date) = input$date

      # combine the input information into a list
      meta = list(
        start_date = start_date,
        end_date = end_date,
        ds_bound = input$downstream_end,
        us_bound = input$upstream_end,
        spact_name = ifelse(input$spact_name == "", NA, input$spact_name),
        spact_url = ifelse(input$spact_url == "", NA, input$spact_url),
        spact_news_url = ifelse(input$spact_news_url == "", NA, input$spact_news_url),
        contact_persons = ifelse(input$contact_persons == "", NA, input$contact_persons),
        set_only = input$set_only
      )

      # export this list to an rds file to be used later
      saveRDS(meta, file.path(output_data_dir, paste0(file_date(meta$start_date), "_meta.rds")))
    })

    # Handle the Done button being pressed
    observeEvent(input$done, {
      stopApp()
    })
  }

  # launch gadget
  runGadget(ui, server, viewer = dialogViewer("", width = 700, height = 500))

}

