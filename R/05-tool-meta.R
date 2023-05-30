#' Interactively enter the opportunity meta-data
#'
#' An RStudio add-in that guides the user through entering certain meta-data
#'   that is used by other components of 'KuskoHarvEst'.
#'   Example meta-data include the date/times of the opener, spatial coverage of the data, and contact information for the analyst
#'

meta_tool = function() {

  proj_dir = rstudioapi::getActiveProject()
  output_data_dir = file.path(proj_dir, "data-use")
  if(!dir.exists(output_data_dir)) dir.create(output_data_dir)

  ### SET UP THE STRATA DROPDOWN MENUS ###

  # build the names that will be shown in the dropdown menuS
  strata_show_down = with(strata_names, paste0(stratum, ": ", stratum_start))
  strata_show_up = with(strata_names, paste0(stratum, ": ", stratum_end))

  # build the choice objects that will be selected from
  strata_down_choices = strata_names$stratum_start
  names(strata_down_choices) = strata_show_down
  strata_up_choices = strata_names$stratum_end
  names(strata_up_choices) = strata_show_up

  # make the first choice a placeholder
  strata_down_choices = c("Select One" = "", strata_down_choices)
  strata_up_choices = c("Select One" = "", strata_up_choices)

  # USER-INTERFACE
  ui = miniUI::miniPage(

    # for disabling buttons until actionable
    shinyjs::useShinyjs(),

    # create the title
    miniUI::gadgetTitleBar("KuskoHarvEst Meta-Data Entry Tool"),

    # populate page with input widgets
    miniUI::miniContentPanel(
      shiny::fillCol(
        flex = c(2,1,3,3,3,1),

        # descriptive text
        shiny::fillRow(
          shiny::p(shiny::em("Here you will enter some information that helps identify the estimates you will produce later.
                    When you are done, be sure to click save to prevent needing to do this again."))
        ),
        shiny::actionLink("get_help", label = "Get Help with Using This Tool", icon = shiny::icon("question-circle")),

        # opportunity date/times
        shiny::fillRow(
          flex = c(2,1,1),
          shiny::dateInput(inputId = "date", label = "Day of Opportunity",
                    value = lubridate::today(), format = "mm-dd-yyyy"),
          shinyTime::timeInput(inputId = "start_time", label = "Start Time (24-hr)", seconds = FALSE, value = strptime("12:00", "%R")),
          shinyTime::timeInput(inputId = "end_time", label = "End Time (24-hr)", seconds = FALSE, value = strptime("23:59", "%R"))
        ),

        # special action identifiers
        shiny::fillRow(
          flex = c(1,2,2),
          shiny::textInput(inputId = "announce_name", label = "Announcement #", placeholder = "Optional"),
          shiny::textInput(inputId = "announce_url", label = "Announcement URL", placeholder = "Optional"),
          shiny::textInput(inputId = "announce_news_url", label = "News Release URL", placeholder = "Optional")
        ),

        # contact information
        shiny::fillRow(
          shiny::textInput(inputId = "contact_persons", label = "Contact Person(s) (Optional)", width = "100%",
                    placeholder = "E.g., Person 1 (p1@email.com), Person 2 (p2@email.com)")
        ),

        # is the opener set-net only?
        shiny::fillRow(
          shiny::checkboxInput(inputId = "set_only", label = "Set Nets Only?", value = FALSE)
        )
      ),
    ),

    # button to export the information to a .rds file
    miniUI::miniButtonBlock(
      shiny::actionButton(inputId = "save_meta", label = "Save and Close", icon = shiny::icon("save"), class = "btn-primary")
    )
  )

  # SERVER-SIDE OPERATIONS
  server = function(input, output, session) {

    # when the "get_help" link is clicked:
    shiny::observeEvent(input$get_help, {
      file.show(resource_path("04-docs/02-meta-data-tool.html"))
    })

    # when the "save" button is clicked:
    shiny::observeEvent(input$save_meta, {

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
        announce_name = ifelse(input$announce_name == "", NA, input$announce_name),
        announce_url = ifelse(input$announce_url == "", NA, input$announce_url),
        announce_news_url = ifelse(input$announce_news_url == "", NA, input$announce_news_url),
        contact_persons = ifelse(input$contact_persons == "", NA, input$contact_persons),
        set_only = input$set_only
      )

      # export this list to an rds file to be used later
      saveRDS(meta, file.path(output_data_dir, paste0(KuskoHarvUtils::file_date(meta$start_date), "_meta.rds")))

      # close the app
      shiny::stopApp()
    })

    # Handle the Done button being pressed
    shiny::observeEvent(input$done, {
      shiny::stopApp()
    })
  }

  # launch gadget
  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("", width = 700, height = 500))

}
