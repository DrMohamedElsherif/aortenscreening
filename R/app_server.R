#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @param input list of input elements
#' @param output list of output elements
#' @param sessions rshiny sessions
#' @include utils.R global_reactives.R
#' @noRd
app_server <- function(input, output, session) {
  # Global data preparation
  r <- init_reactives(input, output)
  init_count_values_bg(output, session)
  init_user_text(session, output)
  # modules
  mod_upload_server("upload_1", r = r)
  mod_database_server("database_1", r = r)
  mod_contact_server("contact_1", r = r)
}
