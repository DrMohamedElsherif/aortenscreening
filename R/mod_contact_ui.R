#' overview UI Function
#' @description A shiny Module. This module gives a general overview of
#' data used in this application
#' @param id,input,output,session Internal parameters for {shiny}.
#' @importFrom DT DTOutput
#' @importFrom DT renderDT
#' @noRd
#' @importFrom shiny NS tagList
mod_contact_ui <- function(id) {
  ns <- NS(id)
  includeMarkdown(here::here("R", "contact.Rmd"))
}

#' overview Server Functions
#' @param id id of mod
#' @param r, reactive values and data
#' @noRd
mod_contact_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}
