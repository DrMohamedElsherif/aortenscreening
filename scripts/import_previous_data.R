library(openxlsx)
library(dplyr)
data <- openxlsx::read.xlsx("/home/christ/Aorten-Screening Komplett -17.03-v1.xlsx", detectDates = TRUE) %>% filter(!is.na(`Screening-Datum`))
data_mod <- data %>%
    dplyr::rename(
      id = "Pat-ID",
      screening_date = "Screening-Datum",
      name = "Name",
      firstname = "Vorname",
      postalcode_imported = "PLZ",
      dob = "Geburtsdatum",
      age_below_50 = "Alter.<.50.J",
      weight_kg = "Gewicht.(kg)",
      height_m = "Größe.(m)",
      height_below_169 = "<.1.69.m",
      bmi = "BMI",
      hypertension = "Art..Hypertonie",
      bicuspid_aortic_valve = "Bikuspide.Aortenklappe",
      ctd = "Bindegewebserkrankung/.welche",
      family_history_relevant = "pos..Familienanamnese",
      pre_surgery = "Vor-OP.(Aorta)",
      comment = "Kommentar",
      aorta_ascendens = "Aorta.ascendens.mm",
      aorta_abdominalis = "Aorta.abdominalis.mm",
      follow_up_recommended = "Empfehlung.Sprechstunde",
      follow_up_date = "FU.Datum.Sprechstunde",
      follow_up_therapy = "FU.Therapie",
      gender = "Geschlecht"
    ) %>%
  mutate(id = uuid::UUIDgenerate(n = n()),
         screening_date = as.Date(screening_date),
         dob = as.Date(dob),
         age_below_50 = as.logical(age_below_50),
         height_below_169 = as.logical(height_below_169),
         follow_up_recommended = as.logical(follow_up_recommended),
         create_user = "excel",
         imported_from_excel = TRUE,
         create_time = as.POSIXct(screening_date))

# GENDER
data_mod$gender[data_mod$gender == "m"] <- "M"
data_mod$gender[data_mod$gender == "w"] <- "F"
data_mod$gender[is.na(data_mod$gender)] <- "U"
print(unique(data_mod$gender))

# DOB
data_mod <- data_mod %>% filter(!is.na(dob))

# check plz
postalcodes <- openxlsx::read.xlsx(sprintf("./inst/app/www/georef-germany-postleitzahl.xlsx"))$`PLZ.Name.(long)`
n_plzs_na <- data_mod %>% dplyr::filter(is.na(postalcode_imported)) %>% nrow()
valid_plzs <- sapply(data_mod$postalcode_imported, function(x) {
  pcs <- postalcodes[grep(paste0("^", x), postalcodes)]
  if (length(pcs) > 0) return(x)
  print(x)
  return(NA)
})
na_plz <- sum(is.na(valid_plzs))
logger::log_info(sprintf("It seems like %s plzs are not from germany!", na_plz - n_plzs_na))
data_mod[["postalcode"]] <- valid_plzs

# fixes
data_mod$aorta_ascendens[data_mod$aorta_ascendens == "nicht möglich"] <- NA
data_mod$firstname[is.na(data_mod$firstname)] <- "nicht vorhanden"
data_mod$aorta_abdominalis[is.na(data_mod$aorta_abdominalis)] <- NA
data_mod$height_m <- as.numeric(gsub(",", ".", data_mod$height_m))
data_mod$ctd[data_mod$ctd == 0] <- "Keine"
data_mod$ctd[is.na(data_mod$ctd)] <- "Keine"
print(unique(data_mod$ctd))
# for precision issues
data_mod$bmi <- as.character(format(round(data_mod$bmi, 2), nsmall = 2))


req <- PsaApiR::psa_request_db_write_default("aortenscreening", "data", data_mod %>% tail(30000), append = TRUE)
ret <- PsaApiR::API()$make_request(req)
