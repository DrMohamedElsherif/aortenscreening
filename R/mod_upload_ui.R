#' @description A shiny Module. This module gives a general overview of
#' data used in this application
#' @param id,input,output,session Internal parameters for {shiny}.
#' @importFrom DT DTOutput
#' @importFrom DT renderDT
#' @importFrom shinyjs useShinyjs disable enable
#' @noRd
#' @importFrom shiny NS tagList wellPanel textInput actionButton radioButtons checkboxInput selectInput fluidRow column
mod_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(), # to allow disable
    h5(uiOutput(outputId = ns("text_details"))),
    div(
      style = "display: flex; justify-content: flex-end; align-items: center; width: 100%; padding-right: 5px;",
      fluidRow(
        column(4, offset = 4, textInput(inputId = ns("id"), label = "Ausgewählte Patientenid")),
        column(4, textInput(inputId = ns("create_time"), label = "Erstellzeit"))
      )
    ),
    h4(""),
    fluidRow(column(
      4,
      wellPanel(
        titlePanel(strong("Untersuchung", style="color:#337ab7")),
        fluidRow(
          column(12, selectInput(inputId = ns("selectpatient"), label = "Auswahl Proband*in", choices = c("Neue Anlage"),
                                 selected = "Neue Anlage")),
          column(12, dateInput(inputId = ns("screeningdate"), label = "Screeningdatum", format = "dd.mm.yyyy"))
        )
      ),
    ),
    column(8,
           wellPanel(
             titlePanel(strong("Stammdaten", style="color:#337ab7")),
             fluidRow(
               column(3, textInput(inputId = ns("name_family"), label = "Nachname* (max. 100 Zeichen)")),
               column(3, textInput(inputId = ns("name_given"), label = "Vorname* (max. 100 Zeichen)")),
               column(3, textInput(inputId = ns("birthdate"), label = "Geburtsdatum [TT.MM.JJJJ]*")),
               column(3, selectizeInput(inputId = ns("gender"), label = "Geschlecht*", choices = c("", "M", "F", "D", "U"), selected = "")),
             ),
             fluidRow(
               column(3, selectizeInput(inputId = ns("postalcode"), label = "PLZ", choices = NULL, selected = NA)),
               column(3, textInput(inputId = ns("weight"), label = "Gewicht [kg]*")),
               column(3, textInput(inputId = ns("height"), label = "Größe [m / punktgetrennt]*")),
               column(3, textInput(inputId = ns("bmi_display"), label = "BMI")),
             )))
    ),
    fluidRow(column(
      6,
      wellPanel(
        titlePanel(strong("Vorgeschichte", style="color:#337ab7")),
        fluidRow(
          column(3, checkboxInput(inputId = ns("hypertension"), label = "Arterielle Hypertonie")),
          column(3, checkboxInput(inputId = ns("bikuspid_valve"), label = "Bikuspide Aortenklappe")),
          column(3, checkboxInput(inputId = ns("family"), label = "Pos. Familienanamnese")),
          column(3,
                 selectInput(
                   inputId = ns("ctd"),
                   label = "Bindegewebsschwäche",
                   choices = c(
                     "Keine",
                     "Marfan-Syndrom",
                     "Ehlers-Danlos-Syndrom",
                     "Loeys-Dietz-Syndrom",
                     "Turner Syndrom",
                     "FTAAD",
                     "Andere"
                   ),
                   selected = "Keine"
                 ))
        ),
        fluidRow(
          column(4, textInput(inputId = ns("pre_surgery"), label = "Vor-OP (max. 100 Zeichen)")),
          column(8, textInput(inputId = ns("comment"), label = "Kommentar (max. 300 Zeichen)"))
        )
      )
    ),
    column(3,
           wellPanel(
             titlePanel(strong("Messung", style="color:#337ab7")),
             fluidRow(
               column(3, shinyWidgets::materialSwitch(inputId = ns("aorta_asc_mes"), label = "Messbar", value = TRUE)),
               column(9, textInput(inputId = ns("aorta_asc"), label = "Aorta ascendens [mm]*")),
               column(3, shinyWidgets::materialSwitch(inputId = ns("aorta_abd_mes"), label = "Messbar", value = TRUE)),
               column(9, textInput(inputId = ns("aorta_abd"), label = "Aorta abdominalis [mm]*"))
             )
           )),
    column(3,
           wellPanel(
             titlePanel(strong("Fazit", style="color:#337ab7")),
             fluidRow(
               column(6, checkboxInput(inputId = ns("followup_rec"), label = "Empfehlung Aortensprechstunde")),
               column(6, dateInput(inputId = ns("followup_date"), label = "Datum Sprechstunde", format = "dd.mm.yyyy"),
                      value = ""),
               column(12, textInput(inputId = ns("followup_therapy"), label = "Therapie Sprechstunde (max. 100 Zeichen)"))
             )
           )))
    ,
    h5(""),
    actionButton(inputId = ns("save"), label = "Werte in Datenbank übernehmen",
                 class = "custom-btn-blue", icon = icon("save"),
                 style = "color: #fff; background-color: #337ab7; border-color: #2e6da4;
                      transition: background-color 0.3s ease;"),
    actionButton(inputId = ns("newpatient"), label = "Reset",
                 class = "custom-btn-white", icon = icon("refresh"),
                 style = "color: #337ab7; background-color: #fff; border: 2px solid #337ab7;
                      transition: background-color 0.3s ease;"),
  )
}

#' overview Server Functions
#' @param id id of mod
#' @param r reactive values and data
#' @noRd
mod_upload_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    mod_upload_server_init(session, output)
    iv <- get_input_validator(input)
    ## Observer elements ################
    observe_bmi(session, input)
    observe_fu(input)
    observe_height(session, input)
    observe_weight(session, input)
    observe_aorta_asc(session, input)
    observe_aorta_abd(session, input)
    observe_dwh_updates(session, r)
    observe_selection(session, input)
    observe_new(session, input, iv)
    observe_save(session, r, input, output, iv)
    observe_name(session, input)
    observe_first_name(session, input)
    observe_dob(session, input)
    observe_comment(session, input)
    observe_pre_surgery(session, input)
    observe_followup_therapy(session, input)
    observe_asc_measurable(session, input)
    observe_abd_measurable(session, input)
  })
}

#' Function mod_upload_server_init
#' @description
#' Init some fields with disable or default text
#' @param session shiny sessions
#' @param output shiny output elements
#' @return NULL
mod_upload_server_init <- function(session, output) {
  suppressWarnings(updateSelectizeInput(session, 'postalcode', choices = get_postalcodes_germany(), server = FALSE,
                                        selected = ""))
  updateSelectInput(session, inputId = "selectpatient", selected = "Neue Anlage")
  shinyjs::disable("screeningdate")
  shinyjs::disable("id")
  shinyjs::disable("bmi_display")
  shinyjs::disable("followup_date")
  shinyjs::disable("followup_therapy")
  shinyjs::disable("create_time")
  suppressWarnings(updateDateInput(session, inputId = "followup_date", value = NA))
  output$text_details <- renderUI({
    h5(HTML("Über dieses Dashboard haben Sie die Möglichkeit, einen <strong>neuen</strong> Datensatz anzulegen oder einen bestehenden zu <strong>aktualisieren</strong>.
         Um einen neuen Datensatz anzulegen, befüllen Sie mind. alle mit einem <strong>*</strong> gekennzeichneten Felder und klicken Sie anschließend auf <strong>Werte in Datenbank übernehmen</strong>.
         Fehlerhafte Eingaben werden Ihnen ggfs. angezeigt.
            Um einen bestehenden Datensatz zu <strong>ändern</strong>, wählen Sie im Bereich <em>Untersuchung</em> eine ID und passen Sie die Felder entsprechend an. Die Eingabe bestätigen Sie ebenfalls mit
            dem Button <em>Werte in Datenbank übernehmen</em>. Es wird jeweils nur der <strong>aktuellste</strong> Eintrag pro Patient übernommen. Über <strong>Reset</strong> können Sie alle Felder <strong>ohne</strong> Speicherung zurücksetzen.
            Überschreitet der Inhalt von Freitextfeldern das Zeichenlimit, wird dieser automatisch abgeschnitten. Numerische Werte werden beim Abspeichern auf <strong>zwei</strong> Nachkommastellen gerundet."))
  })
}

#' Function observe_bmi
#' @description
#' observe BMI, i.e. calculate BMI
#' @param session shiny sessions
#' @param input shiny input elements
#' @return NULL
observe_bmi <- function(session, input) {
  observe({
    weight <- as.numeric(input$weight)
    height <- as.numeric(input$height)
    bmi <- if (!is.na(weight) && !is.na(height) && height > 0) {
      round((weight / (height^2)), 2)
    } else {
      NA
    }
    if (!is.na(bmi)) updateTextInput(session, "bmi_display", value = bmi)
    else updateTextInput(session, "bmi_display", value = "n/a")
  })
}

#' Function observe_asc_measurable
#' @description
#' observe switch for asc. If disabled then disable also artoa field and reset
#' @param session shiny session
#' @param input shiny input elements
#' @return NULL
observe_asc_measurable <- function(session, input) {
  observeEvent(input$aorta_asc_mes, {
    if (input$aorta_asc_mes) {
      shinyjs::enable("aorta_asc")
    } else {
      shinyjs::disable("aorta_asc")
      updateTextInput(session, "aorta_asc", value = "")
    }
  })
}

#' Function observe_abd_measurable
#' @description
#' observe switch for abd If disabled then disable also artoa field and reset
#' @param session shiny session
#' @param input shiny input elements
#' @return NULL
observe_abd_measurable <- function(session, input) {
  observeEvent(input$aorta_abd_mes, {
    if (input$aorta_abd_mes) {
      shinyjs::enable("aorta_abd")
    } else {
      shinyjs::disable("aorta_abd")
      updateTextInput(session, "aorta_abd", value = "")
    }
  })
}

#' Function observe_fu
#' @description
#' observe follow up fields to make them disable or enable
#' @param input shiny input elements
#' @return NULL
observe_fu <- function(input) {
  observeEvent(input$followup_rec, {
    if (input$followup_rec) {
      shinyjs::enable("followup_date")
      shinyjs::enable("followup_therapy")
    } else {
      shinyjs::disable("followup_date")
      shinyjs::disable("followup_therapy")
    }
  })
}

#' Function observe_aorta_asc
#' @description
#' observe aorta_asc, i.e. replace input aorta_asc
#' @import stringr
#' @param session shiny sessions
#' @param input shiny input elements
#' @return NULL
observe_aorta_asc <- function(session, input) {
  observeEvent(input$aorta_asc, {
    updated_aorta_asc <- stringr::str_replace(input$aorta_asc, pattern = ",", replacement = ".")
    updated_aorta_asc <- stringr::str_replace_all(updated_aorta_asc, "[^0-9.]", "")
    updateTextInput(session, "aorta_asc", value = updated_aorta_asc)
  })
}

#' Function observe_aorta_abd
#' @description
#' observe aorta_abd, i.e. replace input aorta_abd
#' @import stringr
#' @param session shiny sessions
#' @param input shiny input elements
#' @return NULL
observe_aorta_abd <- function(session, input) {
  observeEvent(input$aorta_abd, {
    updated_aorta_abd <- stringr::str_replace(input$aorta_abd, pattern = ",", replacement = ".")
    updated_aorta_abd <- stringr::str_replace_all(updated_aorta_abd, "[^0-9.]", "")
    updateTextInput(session, "aorta_abd", value = updated_aorta_abd)
  })
}

#' Function observe_height
#' @description
#' observe body height, i.e. replace input height
#' @import stringr
#' @param session shiny sessions
#' @param input shiny input elements
#' @return NULL
observe_height <- function(session, input) {
  observeEvent(input$height, {
    updated_height <- stringr::str_replace(input$height, pattern = ",", replacement = ".")
    updated_height <- stringr::str_replace_all(updated_height, "[^0-9.]", "")
    updateTextInput(session, "height", value = updated_height)
  })
}

#' Function observe_weight
#' @description
#' observe body weight, i.e. replace input weight
#' @import stringr
#' @param session shiny sessions
#' @param input shiny input elements
#' @return NULL
observe_weight <- function(session, input) {
  observeEvent(input$weight, {
    updated_weight <- stringr::str_replace(input$weight, pattern = ",", replacement = ".")
    updated_weight <- stringr::str_replace_all(updated_weight, "[^0-9.]", "")
    updateTextInput(session, "weight", value = updated_weight)
  })
}

#' Function observe_name
#' @description
#' observe name given
#' @import stringr
#' @param session shiny sessions
#' @param input shiny input elements
#' @return NULL
observe_name <- function(session, input) {
  observeEvent(input$name_family, {
    updated_name_family <- stringr::str_replace_all(input$name_family, "[^A-Za-zäöüÄÖÜß -]", "")
    updated_name_family <- substr(updated_name_family, 0, 100)
    updateTextInput(session, "name_family", value = updated_name_family)
  })
}

#' Function observe_name
#' @description
#' observe comment
#' @import stringr
#' @param session shiny sessions
#' @param input shiny input elements
#' @return NULL
observe_comment <- function(session, input) {
  observeEvent(input$comment, {
    updated_comment <- substr(input$comment, 0, 300)
    updateTextInput(session, "comment", value = updated_comment)
  })
}

#' Function observe_followup_therapy
#' @description
#' observe followup_therapy
#' @import stringr
#' @param session shiny sessions
#' @param input shiny input elements
#' @return NULL
observe_followup_therapy <- function(session, input) {
  observeEvent(input$followup_therapy, {
    updated_followup_therapy <- substr(input$followup_therapy, 0, 100)
    updateTextInput(session, "followup_therapy", value = updated_followup_therapy)
  })
}

#' Function observe_pre_surgery
#' @description
#' observe pre_surgery
#' @import stringr
#' @param session shiny sessions
#' @param input shiny input elements
#' @return NULL
observe_pre_surgery <- function(session, input) {
  observeEvent(input$pre_surgery, {
    updated_pre_surgery <- substr(input$pre_surgery, 0, 100)
    updateTextInput(session, "pre_surgery", value = updated_pre_surgery)
  })
}

#' Function observe_dob
#' @description
#' observe_dob
#' @import stringr
#' @param session shiny sessions
#' @param input shiny input elements
#' @return NULL
observe_dob <- function(session, input) {
  observeEvent(input$birthdate, {
    updated_birthdate <- stringr::str_replace_all(input$birthdate, "[^0-9.]", "")
    updateTextInput(session, "birthdate", value = updated_birthdate)
  })
}

#' Function observe_first_name
#' @description
#' observe name
#' @import stringr
#' @param session shiny sessions
#' @param input shiny input elements
#' @return NULL
observe_first_name <- function(session, input) {
  observeEvent(input$name_given, {
    updated_name_given <- stringr::str_replace_all(input$name_given, "[^A-Za-zäöüÄÖÜß -]", "")
    updated_name_given <- substr(updated_name_given, 0, 100)
    updateTextInput(session, "name_given", value = updated_name_given)
  })
}

#' Function observe_dwh_updates
#' @description
#' observe dwh udpates, i.e. update choice selection list
#' @import stringr
#' @param session shiny sessions
#' @param r shiny reactive values
#' @return NULL
observe_dwh_updates <- function(session, r) {
  observeEvent(r$dwh_updates, {
    # Get existing patients from database and filter for most current row per ID
    ret <- get_patients()
    if (nrow(ret) > 0) {
      ret <- ret %>%
        group_by(id) %>%
        filter(`_psa_last_edited` == max(`_psa_last_edited`)) %>%
        ungroup() %>%
        arrange(desc(`_psa_last_edited`))
      updateSelectInput(
        session = session,
        inputId = "selectpatient",
        choices = unique(c("Neue Anlage", paste0(ret$name, ", ", ret$firstname,
                                " (", strftime(ret$dob, format = "%d.%m.%Y"), ") [ID: ", ret$id, "]"))),
        selected = if (r$selectedPatId != "") r$selectedPatId else "Neue Anlage"
      )
    }
  })
}

#' extract_id
#' @description
#' Extracts name, given name and birthdate from input. Can then be used to query database
#' @param input_string character
#' @import stringr
#' @return Extracted name, name_given and birthdate
#' @export
extract_id <- function(input_string) {
  pattern <- "\\[ID:\\s*(.*)\\]"  # Search for text in brackets
  matches <- stringr::str_match(input_string, pattern)
  # Check if ID was found
  if (is.null(matches) || any(is.na(matches))) {
    return(NULL)
  }
  return(matches[2])
}


#' Function observe_selection
#' @description
#' observe patient selection field
#' @import stringr
#' @param session shiny sessions
#' @param input shiny input values
#' @return NULL
observe_selection <- function(session, input) {
  observeEvent(input$selectpatient, {
    # Get ID for selected patient
    patientid <- extract_id(input$selectpatient)
    # Query database with selected ID
    ret <- get_patients()
    if (length(patientid) > 0) {
      row <- get_latest_entry(data = ret, patientid = patientid)
    } else {
      row <- NULL
      clear_input(session)
    }
    if (length(row) > 0) {
      # Update input fields with loaded data
      shinyalert(text = HTML("Sie haben eine/n bestehende/n Proband*in zur Aktualisierung ausgewählt. Es wird <strong>kein neuer Eintrag</strong> angelegt. Sind Sie sicher, dass Sie ein Update durchführen möchten?"),
                 type = "info", showConfirmButton = TRUE, showCancelButton = TRUE,
                 confirmButtonText = "Ja, Proband*in auswählen",
                 html = TRUE,
                 closeOnClickOutside = TRUE,
                 className = 'alert',
                 cancelButtonText = "Nein, neue Anlage", callbackR = function(x) {
                   if (x == TRUE) {
                     load_input_patientdata(session = session, ret = row)
                     showNotification(paste0(currentTime(), ": Patient ", row$name, ", ",
                                             row$firstname, " (", strftime(row$dob, format = "%d.%m.%Y"), ") ausgewählt."))
                     shinyjs::runjs("document.activeElement.blur();")
                   } else {
                     clear_input(session)
                     shinyjs::runjs("$('#upload_1-name_family').focus();")
                   }
                 })
    }
  })
}

#' Function observe_new
#' @description
#' observe clear button to clear all fields
#' @import stringr
#' @param session shiny sessions
#' @param input shiny input values
#' @param iv input validator
#' @return NULL
observe_new <- function(session, input, iv) {
  observeEvent(input$newpatient, {
    clear_input(session)
    iv$disable()
    showNotification(paste(currentTime(), ": Einträge zurückgesetzt."))
    shinyjs::runjs("$('#upload_1-name_family').focus();")
  })
}

#' Function observe_new
#' @description
#' observe save button to save to DB
#' @import stringr
#' @importFrom shinyalert shinyalert
#' @importFrom uuid UUIDgenerate
#' @param session shiny sessions
#' @param r reactive values
#' @param input shiny input values
#' @param output shiny output values
#' @param iv input validator
#' @param precision numeric precision for saving numbers
#' @include mod_database_ui.R
#' @return NULL
observe_save <- function(session, r, input, output, iv, precision = 2) {
  observeEvent(input$save, {
    iv$enable()
    if (!iv$is_valid()) {
      shinyalert(text = "Bitte überprüfen Sie die markierten Eingabefelder.", type = "error", callbackR = function(x) {
        shinyjs::runjs("$('#upload_1-name_family').focus();")
      })
      output$text_status_dwh <- renderText({
        paste(currentTime(), ": Bitte überprüfen Sie die markierten Eingabefelder.")
      })
    } else {
      # Create dataframe from input
      new_data <- data.frame(
        id = if (input$id == "") uuid::UUIDgenerate() else input$id,
        screening_date = input$screeningdate,
        name = input$name_family,
        firstname = input$name_given,
        postalcode = as.numeric(gsub("^([0-9]{5}).*", "\\1", input$postalcode)),
        dob = lubridate::dmy(input$birthdate),
        age_below_50 = ifelse(as.numeric(difftime(Sys.Date(),
                                                  as.Date(input$birthdate, format = "%d.%m.%Y"), units = "days")) / 365.25 < 50, 1, 0),
        gender = as.character(input$gender),
        weight_kg = round(as.numeric(input$weight), precision),
        height_m = round(as.numeric(input$height), precision),
        height_below_169 = ifelse(as.numeric(input$height) < 1.69, 1, 0),
        bmi = round((as.numeric(input$weight) / (as.numeric(input$height)^2)), 2),
        hypertension = input$hypertension,
        bicuspid_aortic_valve = input$bikuspid_valve,
        ctd = input$ctd,
        family_history_relevant = input$family,
        pre_surgery = input$pre_surgery,
        comment = input$comment,
        aorta_ascendens = round(as.numeric(input$aorta_asc), precision),
        aorta_abdominalis = round(as.numeric(input$aorta_abd), precision),
        follow_up_recommended = input$followup_rec,
        follow_up_date = ifelse(length(input$followup_date) > 0, as.character(input$followup_date), NA),
        follow_up_therapy = input$followup_therapy,
        create_time = if (input$create_time == "") as.character(currentTime()) else input$create_time,
        imported_from_excel = FALSE
      )

      ret <- write_to_PSA(new_data, session, r = r)
      # Check if DWH query was successful. Otherwise show error message to user
      check_DB_response(ret, r, output)
      updateTextInput(session, "create_time", value = new_data$create_time)
      updateTextInput(session, "id", value = new_data$id)
      clear_input(session)
      iv$disable()
    }
  })
}

