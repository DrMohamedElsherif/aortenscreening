#' Function init_reactives
#' @description defining global reactive values
#' @param input input elements
#' @param output output elements
#' @return reactive values
init_reactives <- function(input, output) {
  r <- reactiveValues()
  r$dwh_updates <- 0
  r$selectedPatId <- ""
  return(r)
}
