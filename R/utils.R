#' currentTime
#' @description Helper function to return current systime plus two hours
#' @return Current systime plus two hours
currentTime <- function() {
  return(lubridate::with_tz(Sys.time(), "Europe/Berlin"))
}

#' write_to_PSA
#' @description Wrapper to write to DWH PSA
#' @param df dataframe
#' @param schema character
#' @return .API_response object
write_to_PSA <- function(df, session, r) {
  # First, get logged in user and append to dataframe. Then, write dataframe to DWH
  df["create_user"] <- get_user_name(session)
  req <- PsaApiR::psa_request_db_write_default("aortenscreening", "data", df)
  ret <- PsaApiR::API()$make_request(req)
  return(ret)
}

#' get_user_name
#' @description get name of logged in user
#' @param session shiny object
#' @return chr
get_user_name <- function(session) {
  if(Sys.info()["user"] == "rstudio-connect") return(session$user)
  return(Sys.info()["user"][[1]])
}

#' get_input_validator
#' @description Checks certain fields for correct input
#' @param input rshiny input elements
#' @return TRUE if all input checks have passed, otherwise FALSE
get_input_validator <- function(input) {
  iv <- shinyvalidate::InputValidator$new()
  iv$add_rule("name_family", function(value) {
    if(value == "") {
      return("Geben Sie einen Nachnamen an")
    }
  })
  iv$add_rule("name_given", function(value) {
    if(value == "") {
      return("Geben Sie einen Vornamen an")
    }
  })
  iv$add_rule("weight", function(value) {
    if (is.na(as.numeric(value)) || as.numeric(value) < 20 || as.numeric(value) > 400) {
      return("Geben Sie eine ganze Zahl zwischen 20 und 400 ein")
    }
  })
  iv$add_rule("height", function(value) {
    if (is.na(as.double(value)) || as.double(value) < 0.3 || as.double(value) > 2.5) {
      return("Geben Sie eine Dezimalzahl zwischen 0.3 und 2.5 ein")
    }
  })
  iv$add_rule("gender", function(value) {
    if (! value %in% c("M", "F", "D", "U")) {
      return("Wählen Sie ein Geschlecht aus 'M', 'F', 'D', 'U'")
    }
  })
  iv$add_rule("postalcode", function(value) {
    if (!(is.na(value) || value == "" || value == "Keine Auswahl")) {
      # Check if ZIP contains 5 digits
      if ((is.na(value) || value == "") & !grepl("^\\d{5}(?:\\s.*)?", value)) {
        return("Geben Sie eine 5-stellige PLZ an")
      }
      # Transform and validate+
      plz <- as.integer(gsub("^([0-9]{5}).*", "\\1", value))
      if (plz < 10000 || plz > 99999) {
        return("Geben Sie eine gültige deutsche PLZ an")
      }
    }
  })
  iv$add_rule("birthdate", function(value) {
    dob_regex <- "^(0?[1-9]|[12][0-9]|3[01])\\.(0?[1-9]|1[0-2])\\.(19|20)\\d{2}$"
    is_valid <- grepl(dob_regex, value)
    if (!is_valid) {
      "Ungültiges Datum: Bitte im Format TT.MM.JJJJ (ab 1900) eingeben"
    } else {
      tryCatch({
        date <- as.Date(value, format = "%d.%m.%Y")
        if (date > Sys.Date()) {
          "Das Geburtsdatum liegt in der Zukunft"
        }
      }, error = function(e) "Ungültiges Datum")
    }
  })
  text_measmnt_error <- "Geben Sie eine Zahl innerhalb des Plausibilitätsbereichs oder nichts ein"
  regex_measmnt_input <- "^[0-9]*$"
  iv$add_rule("aorta_asc", function(value) {
    if (input$aorta_asc_mes) {
      if (is.na(as.numeric(value)) || as.numeric(value) < 15 || as.numeric(value) > 150) {
        return("Geben Sie eine ganze Zahl zwischen 15 und 150 ein")
      }
    }
  })
  iv$add_rule("aorta_abd", function(value) {
    if (input$aorta_abd_mes) {
      if (is.na(as.numeric(value)) || as.numeric(value) < 10 || as.numeric(value) > 150) {
        return("Geben Sie eine ganze Zahl zwischen 10 und 150 ein")
      }
    }
  })
  iv$add_rule("followup_date", function(value) {
    if (length(value) > 0 && !is.na(value) &&  as.character(value) != "") {
      tryCatch({
        date <- as.Date(value, format = "%d.%m.%Y")
        year <- lubridate::year(date)
        if (as.numeric(year) < 2024) {
          "Sprechstunde erst seit 2024 möglich"
        }
      }, error = function(e) "Ungültiges Datum")
    }
  })
  return(iv)
}

#' check_DB_response
#' @description Checks response from DB query and shows result accordingly
#' @importFrom shinyalert shinyalert
#' @param ret api response
#' @param r reactive values
#' @param output rshiny output elements
#' @return NULL
check_DB_response <- function(ret, r, output) {
  if (ret$m_code == 200) {
    r$dwh_updates <- r$dwh_updates + 1
    shinyalert(text = paste(currentTime(),
                            ":\nUpdate auf Datenbank erfolgreich ausgeführt."),
               type = "success", callbackR = function(x) {
                 shinyjs::runjs("$('#upload_1-name_family').focus();")
               })
    showNotification(paste(currentTime(), ": Patient erfolgreich gespeichert."))
  } else {
    shinyalert(text = paste(currentTime(),
                            ":\nFehler beim Schreiben in die Datenbank: ", ret$m_msg),
               type = "error", callbackR = function(x) {
                 shinyjs::runjs("$('#upload_1-name_family').focus();")
               })
    showNotification(paste(currentTime(), ": Fehler beim Schreiben in die Datenbank."))
  }
}

#' get_patients
#' @description
#' Gets all entries from aortenscreening.data from PSA
#' @return tibble
get_patients <- function() {
  req <- PsaApiR::Psa_Request_Export_Tbl()
  req$set_param("schema", "aortenscreening")
  req$set_param("tbl_name", "data")
  ret <- PsaApiR::API()$make_request(req)
  return(ret$m_data)
}

#' get_latest_entry
#' @description
#' Selects most current row for given ID
#' @param data tibble
#' @param patientid id number
#' @import dplyr
#' @return One row per patient
get_latest_entry <- function(data, patientid) {
  data %>%
    # Select most current row for given ID
    filter(id == patientid) %>%
    arrange(desc(`_psa_last_edited`)) %>%
    dplyr::slice(1)
}

#' load_input_patientdata
#' @description Replaces input fields in GUI with values from database
#' @param session rshiny sessions object
#' @param ret tibble row
#' @importFrom shinyWidgets updateSwitchInput
#' @return NULL
load_input_patientdata <- function(session, ret) {
  if(identical(ret, "void")) {
    clear_input(session)
  } else {
    if (!is.na(ret$postalcode)) {
      postalcodes <- get_postalcodes_germany()
      pc <- postalcodes[grep(paste0("^", ret$postalcode), postalcodes)]
      if (length(pc)== 0) ret$postalcode <- ""
      else ret$postalcode <- pc[[1]]
    } else {
      ret$postalcode <- ""
    }
    updateTextInput(session, inputId = "name_family", value = ret$name)
    updateTextInput(session, inputId = "name_given", value = ret$firstname)
    updateDateInput(session, inputId = "screeningdate", value = ret$screening_date)
    updateTextInput(session, inputId = "weight", value = ret$weight_kg)
    updateTextInput(session, inputId = "height", value = ret$height_m)
    updateTextInput(session, inputId = "birthdate", value = strftime(ret$dob, format = "%d.%m.%Y"))
    updateSelectizeInput(session, inputId = "postalcode", selected = ret$postalcode)
    updateTextInput(session, inputId = "comment", value = ret$comment)
    updateTextInput(session, inputId = "aorta_asc", value = ret$aorta_ascendens)
    updateTextInput(session, inputId = "gender", value = ret$gender)
    updateTextInput(session, inputId = "aorta_abd", value = ret$aorta_abdominalis)
    updateTextInput(session, inputId = "pre_surgery", value = ret$pre_surgery)
    suppressWarnings(updateDateInput(session, inputId = "followup_date", value = ret$follow_up_date))
    updateTextInput(session, inputId = "followup_therapy", value = ret$follow_up_therapy)
    updateCheckboxInput(session, inputId = "hypertension", value = ret$hypertension)
    updateCheckboxInput(session, inputId = "bikuspid_valve", value = ret$bicuspid_aortic_valve)
    updateSelectInput(session, inputId = "ctd", selected = ret$ctd)
    updateCheckboxInput(session, inputId = "family", value = ret$family_history_relevant)
    updateCheckboxInput(session, inputId = "followup_rec", value = ret$follow_up_recommended)
    updateTextInput(session, inputId = "create_time", value = ret$create_time)
    updateTextInput(session, inputId = "id", value = ret$id)
    if (is.na(ret$aorta_abdominalis) || ret$aorta_abdominalis == "") {
      shinyWidgets::updateSwitchInput(session, inputId = "aorta_abd_mes", value = FALSE)
    } else {
      shinyWidgets::updateSwitchInput(session, inputId = "aorta_abd_mes", value = TRUE)
    }
    if (is.na(ret$aorta_ascendens) || ret$aorta_ascendens == "") {
      shinyWidgets::updateSwitchInput(session, inputId = "aorta_asc_mes", value = FALSE)
    } else {
      shinyWidgets::updateSwitchInput(session, inputId = "aorta_asc_mes", value = TRUE)
    }
  }
}

#' clear_input
#' @description
#' Clears all input fields to enable new patient entry
#' @param session rshiny sessions object
#' @importFrom shinyWidgets updateSwitchInput
#' @return NULL
clear_input <- function(session) {
  updateSelectInput(session, inputId = "selectpatient", selected = "Neue Anlage")
  updateDateInput(session, inputId = "screeningdate", value = Sys.Date())
  updateTextInput(session, inputId = "name_family", value = "")
  updateTextInput(session, inputId = "name_given", value = "")
  updateTextInput(session, inputId = "weight", value = "")
  updateTextInput(session, inputId = "height", value = "")
  updateTextInput(session, inputId = "birthdate", value = "")
  updateTextInput(session, inputId = "postalcode", value = "")
  updateTextInput(session, inputId = "comment", value = "")
  updateTextInput(session, inputId = "gender", value = "")
  updateTextInput(session, inputId = "aorta_asc", value = "")
  updateTextInput(session, inputId = "aorta_abd", value = "")
  updateTextInput(session, inputId = "aorta_abd", value = "")
  updateTextInput(session, inputId = "pre_surgery", value = "")
  suppressWarnings(updateDateInput(session, inputId = "followup_date", value = NA))
  updateTextInput(session, inputId = "followup_therapy", value = "")
  updateCheckboxInput(session, inputId = "hypertension", value = FALSE)
  updateCheckboxInput(session, inputId = "bikuspid_valve", value = FALSE)
  updateSelectInput(session, inputId = "ctd", selected = "Keine")
  updateCheckboxInput(session, inputId = "family", value = FALSE)
  updateCheckboxInput(session, inputId = "followup_rec", value = FALSE)
  updateTextInput(session, inputId = "create_time", value = "")
  updateTextInput(session, inputId = "id", value = "")
  shinyWidgets::updateSwitchInput(session, inputId = "aorta_asc_mes", value = TRUE)
  shinyWidgets::updateSwitchInput(session, inputId = "aorta_abd_mes", value = TRUE)
}

#' Function get_postalcodes_germany
#' @description
#' getting list of german postal codes and city names
#'
#' @return char list
get_postalcodes_germany <- function() {
  path <- APIUtilizeR::get_inst_folder_path("Aortenscreening")
  postal_codes <- openxlsx::read.xlsx(sprintf("%s/app/www/georef-germany-postleitzahl.xlsx",
                                              path))
  return(c(postal_codes$`PLZ.Name.(long)`, ""))
}

#' Function init_count_values_bg
#' @description
#' Init background job for summary info text
#' @param output output elements
#' @param session rshiny session object
#' @return NULL
init_count_values_bg <- function(output, session) {
  # Define the reactive expression for patients and the summary values
  patient_summary <- reactive({
    invalidateLater(5000, session)
    patients_data <- get_patients()
    if (nrow(patients_data) == 0) return(dplyr::tibble(pats_today = "-", pats_total = "-", pats_rec = "-",
                                                       pats_last = "-"))
    patients_data <- patients_data %>%
      dplyr::group_by(id) %>%
      dplyr::filter(`_psa_last_edited` == max(`_psa_last_edited`, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::summarise(pats_today = sum(as.Date(create_time) >= as.Date(Sys.time()), na.rm = TRUE),
                       pats_total = n(),
                       pats_rec = sum(follow_up_recommended == TRUE, na.rm = TRUE),
                       pats_last = max(`_psa_last_edited`, na.rm = TRUE))
    return(patients_data)
  })
  output$count_today_ui <- renderUI({
    h4(style = "background-color: #1e282c; padding: 15px;", icon("hospital-user"),
       HTML(paste0("&nbsp; Heute: <strong>", as.character(patient_summary()$pats_today), "</strong>")))
  })
  output$count_all_ui <- renderUI({
    h4(style = "background-color: #1e282c; padding: 15px;", icon("users-line"),
       HTML(paste0("&nbsp; Gesamt: <strong>", as.character(patient_summary()$pats_total), "</strong>")))
  })
  output$count_rec_ui <- renderUI({
    h4(style = "background-color: #1e282c; padding: 15px;", icon("person-walking-arrow-loop-left"),
       HTML(paste0("&nbsp; Sprechstunden<br>empfohlen: <strong>", as.character(patient_summary()$pats_rec), "</strong>")))
  })
  output$last_t_ui <- renderUI({
    h4(style = "background-color: #1e282c; padding: 15px;", icon("user-clock"),
       HTML(paste0("&nbsp; <strong> ", as.character(patient_summary()$pats_last), "</strong>")))
  })
}


#' Function init_user_text
#' @description
#' Init user text
#' @param output shiny elements
#' @param session shiny session
#'
#' @return NULL
init_user_text <- function(session, output) {
  output$user <- renderUI({
    h5(style = "background-color: #eeeeee; padding: 15px; border-radius: 5px;", icon("user-lock"),
       HTML(paste0("&nbsp; <strong> ", get_user_name(session), "</strong>")))

  })
}
