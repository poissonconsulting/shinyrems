# Copyright 2020 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

# Module UI

#' @title   mod_about_ui and mod_about_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_about
#'
#' @keywords internal
mod_upload_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      title("Upload Data") %>% helper("tab1_upload"),
      br(),
      dl_button(ns("dl_template"), label = "Download Template"),
      fileInput(ns("upload_data"),
                buttonLabel = span(tagList(icon("upload"), "csv")),
                label = "",
                placeholder = "Upload your own dataset",
                accept = c(".csv")
      ),
      uiOutput(ns("ui_parameter")),
      uiOutput(ns("ui_site")),
      uiOutput(ns("ui_date_range"))
    ),
    mainPanel(
      tabsetPanel(
        selected = "Uploaded Data",
        id = ns("tabset_data"),
        tabPanel(
          title = "Uploaded Data",
          br(),
          uiOutput(ns("ui_table_upload"))
        ),
        tabPanel(
          title = "Processed Data",
          br(),
          uiOutput(ns("ui_table_processed"))
        )
      )
    )
  )
}

# Module Server

#' @rdname mod_about
#' @keywords internal

mod_upload_server <- function(input, output, session) {
  ns <- session$ns

  lookup <- getShinyOption("lookup", NULL)

  shinyhelper::observe_helpers(help_dir = system.file("helpfiles", package = "shinyrems"))

  rv <- reactiveValues(
    check_data = NULL,
    date_data = NULL,
    processed_data = NULL
  )

  dates <- reactive({
    req(rv$date_data)
    as.Date(range(rv$date_data$DateTime), na.rm = TRUE)
  })

  params <- reactive({
    rv$check_data$Variable
  })

  sites <- reactive({
    rv$check_data$Station
  })

  output$ui_parameter <- renderUI({
    params <- params()
    selectInput(ns("parameter"), "Select parameter",
                choices = params, selected = params[1])
  })

  output$ui_site <- renderUI({
    sites <- sites()
    select_input_x(ns("site"), label = "Select site(s)",
                choices = sites, selected = sites)
  })

  output$ui_date_range <- renderUI({
    dates <- dates()
    dateRangeInput(ns("date_range"),
                   label = "Filter data between dates:",
                   start = dates[1], end = dates[2],
                   min = dates[1], max = dates[2]
    )
  })

  observeEvent(input$upload_data, {
    data <- readr::read_csv(input$upload_data$datapath)
    check <- try(check_data_upload(data), silent = TRUE)
    if (is_try_error(check)) {
      rv$check_data <- NULL
      rv$date_data <- NULL
      updateTabsetPanel(session, "tabset_data", selected = "Uploaded Data")
      return(showModal(error_modal(check)))
    } else {
      rv$check_data <- check
      rv$date_data <- process_dates(check)
      updateSelectInput(session, "parameter", selected = check$Variable[1])
      updateTabsetPanel(session, "tabset_data", selected = "Processed Data")
    }
  })

  observe({
    req(input$parameter)
    req(input$site)
    req(input$date_range)
    req(rv$date_data)
    processed <- process_data_upload(rv$date_data,
                                     input$parameter,
                                     input$site,
                                     input$date_range)
    rv$processed_data <- processed
  })

  output$ui_table_upload <- renderUI({
    ems_table_output(ns("table_upload"))
  })

  output$table_upload <- DT::renderDT({
    ems_data_table(rv$check_data)
  })

  output$ui_table_processed <- renderUI({
    ems_table_output(ns("table_processed"))
  })

  output$table_processed <- DT::renderDT({
    ems_data_table(rv$processed_data)
  })

  output$dl_template <- downloadHandler(
    filename = function() paste0("ems_template.csv"),
    content = function(file) {
      file.copy(system.file("extdata/ems_template.csv", package = "shinyrems"), file)
    })

  return(
    list(
      dataset = reactive({
        "upload"
      }),
      lookup = reactive({
        lookup
      }),
      data = reactive({
        rv$processed_data
      }),
      all_data = reactive({
        rv$date_data
      }),
      date = reactive({
        input$date_range
      })
    )
  )

}
