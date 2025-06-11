#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinyWidgets
#' @import shiny
#' @importFrom shinyalert shinyalert
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(
      dashboardHeader(title = "AortenscreenR"),
      dashboardSidebar(sidebarMenu(
        div(uiOutput("count_today_ui"),
            uiOutput("count_all_ui"),
            uiOutput("count_rec_ui"),
            uiOutput("last_t_ui")),
        div(style = "position: relative; display: flex; flex-direction: column; align-items: center;",
            div(img(src = "www/aorta_sketch.png",
                    style = "position: fixed; bottom: 5px; left: 0; width: 230px; height: 100%; z-index: 1000; opacity:30%"),
                style = "width: 100%;"))
      ),
      div(style = "width:100%",
          h4(style = "position: fixed; bottom: 0px;background-color: #1e282c; padding: 15px;",
             icon("envelope-open-text"), HTML("Kontakt: medic@rbk.de"))
      )),
      dashboardBody(
        fluidRow(
          column(7, h5(
            style = "background-color: #dd4b39; color:white; padding: 15px; border-radius: 5px;",
            icon("circle-exclamation"),
            HTML(paste0("&nbsp; <strong> ", "Diese Software dient der klinischen Studie \"Aortenscreening\". Eine Verwendung über diesen Rahmen hinaus ist nicht zulässig." , "</strong>"))
          )),
          column(2, uiOutput("user")),
          column(1, h5("")),
          column(2,  div(img(src="www/bhc.jpg", style="width:100%; opacity: 100%; z-index:0")))),
        h5(""),
        tags$style(
          HTML(
            "
            .main-header {background-color: #337ab7}
            .box.box-solid.box-primary{background:transparent}
            .pat-info-text1231 {color: #bac8d9;}
            .box {margin-left: -15px;}
            .move_left {margin-left: -15px;}
            .content-wrapper {background: #ffffff}
            .well {
              background-color: #ffffff; /* Change this to your desired color */
              border-color: #d2d6de; /* Optional: Change the border color */
              border-top-width: 3px; }
            .info-box {padding: -30px !important;}
            .bg-blue {background-color: #337ab7 !important; }
            .selectize-dropdown-content .option {
              white-space: nowrap;
              overflow: hidden;
              text-overflow: ellipsis;
            }
            .selectize-input {
              white-space: nowrap;
              overflow: hidden;
              text-overflow: ellipsis;
              display: block;
            }
            .custom-btn-blue:hover,
            .custom-btn-blue:focus {
              background-color: #23527c !important; /* Darker blue */
            }

            .custom-btn-white:hover,
            .custom-btn-white:focus {
              background-color: #eeeeee !important; /* Soft blue background */
            }
            .alert .confirm {color: #337ab7 !important; background-color: #fff !important; border: 2px solid #337ab7 !important;}
            .alert .cancel {color: #fff !important; background-color: #337ab7!important; border-color: #2e6da4!important;}

            .alert .confirm:hover {background-color: #eeeeee !important; /* Soft blue background */}

            .alert .cancel:hover,
            .alert .cancel:focus {background-color: #23527c !important; /* Soft blue background */}
            .label-default {background-color: #337ab7;}
            "
          )
        ),
        h5(""),
        tabsetPanel(
          type = "tabs",
          tabPanel("Daten erfassen", mod_upload_ui("upload_1")),
          tabPanel("Datenbank", mod_database_ui("database_1"), id = "db"),
          tabPanel("Hilfe & Kontakt", mod_contact_ui("contact_1"))
        )
      )
    )
  )
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "AortenscreenR"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
