#' overview UI Function
#' @description A shiny Module. This module gives a general overview of
#' data used in this application
#' @param id,input,output,session Internal parameters for {shiny}.
#' @importFrom DT DTOutput
#' @importFrom DT renderDT
#' @noRd
#' @importFrom shiny NS tagList
mod_database_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h5("Aktuelle Einträge in der Studiendatenbank (bei mehreren Datensätzen zur selben ID
       wird nur der aktuellste Datensatz angezeigt)."),
    h3(""),
    uiOutput(ns("dev_info_note")),
    actionButton(inputId = ns("refresh"), label = "Tabelle aktualisieren", icon = icon("refresh"),
                 class = "custom-btn-blue",
                 style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    downloadButton(outputId = ns("export"), label = "Excel Export",
                   class = "custom-btn-white",
                 style = "color: #337ab7; background-color: #fff; border: 2px solid #337ab7;"),
    h4(""),
    wellPanel(DT::DTOutput(outputId = ns("dwh_psa_data")))
  )
}

#' Function observe_refresh
#' @description
#' refresh table
#' @param input shiny input values
#' @param output shiny output elements
#' @return NULL
observe_refresh <- function(input, output) {
  observeEvent(input$refresh, {
    update_table(get_table_data(), output)
    showNotification(paste(currentTime(), ": Einträge aktualisiert."))
  })
}

#' Function observe_export
#' @description
#' export button handler
#' @param output shiny output elements
#' @param session rhsiny session
#' @return NULL
observe_export <- function(output, session) {
  output$export <- downloadHandler(
    filename = function() {
      paste0("aortenscreening_", Sys.Date(), "_", get_user_name(session)[[1]], ".xlsx", sep="")
    },
    content = function(file) {
      openxlsx::write.xlsx(get_table_data(), file)
    }
  )
  showNotification(paste(currentTime(), ": Einträge aktualisiert."))
}

#' Function update_table
#' @description
#' Update table
#' @param data tibble
#' @param output shiny elements
#' @return NULL
update_table <- function(data, output) {
  output$dwh_psa_data <- DT::renderDataTable(data,
                                             options = list(
                                               pageLength = 20,
                                               scrollX = TRUE,
                                               dom = 'Bfrtip',
                                               buttons = c(),
                                               fixedHeader = TRUE,
                                               keys = FALSE))
}

#' Function get_table_data
#' @description
#' Get patient data for table
#'
#' @return tibble
get_table_data <- function() {
  ret <- get_patients()
  if (nrow(ret) > 0) {
    ret <- ret %>%
      dplyr::group_by(id) %>%
      dplyr::filter(`_psa_last_edited` == max(`_psa_last_edited`)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(desc(`_psa_last_edited`))
  }
  return(ret)
}

#' Function
#' @description
#' Adding note that on dev/test we only see some patients but not all
#'
#' @param output rshiny output elements
#'
#' @return NULL
init_dev_info <- function(output) {
  if (tolower(Sys.getenv("R_SERVER_MODE", unset = "")) != "prod") {
    output$dev_info_note <- renderUI({
      h5(
        style = "background-color: #337ab7; color:white; padding: 15px; border-radius: 5px;",
        icon("circle-exclamation"),
        HTML(paste0("&nbsp; <strong> ", "In dieser DEMO Version werden maximal 100 Einträge angezeigt." , "</strong>"))
      )
    })
  }
}

#' overview Server Functions
#' @param id id of mod
#' @param r, reactive values and data
#' @import httr
#' @noRd
mod_database_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    init_dev_info(output)
    update_table(get_table_data(), output)
    observe_refresh(input, output)
    observe_export(output, session)
  })
}
