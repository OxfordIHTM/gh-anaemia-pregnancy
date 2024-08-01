#'
#' Recode variables
#'

recode_anc_variables <- function(anc_data_processed) {
  anc_data_processed |>
    dplyr::mutate(
      total_n = n(),
      age_group = cut(
        x = age,
        breaks = c(-Inf, 15, 20, 25, 30, 35, 40, 45, Inf),
        labels = c(
          "under 15 years", "15 to 19 years", "20 to 24 years", "25 to 29 years", 
          "30 to 34 years", "35 to 39, years", 
          "40 to 44 years", "45 years and older"
        ),
        include.lowest = TRUE, right = FALSE
      ),
      anaemia_status = ifelse(haemoglobin < 11, "anaemia", "no anaemia"),
      anaemia_category = cut(
        x = haemoglobin,
        breaks = c(0, 7, 10, 11, Inf),
        labels = c(
          "severe anaemia", "moderate anaemia", "mild anaemia", "no anaemia"
        )
      ),
      education_level = factor(
        x = education_level, 
        levels = c(
          "Primary", "Junior High School", "Senior High School", 
          "Tertiary", "None"
        )
      )
    )
}


#'
#' Recode ANC model variables
#'

recode_anc_model_variables <- function(anc_data_recode) {
  anc_data_recode |>
    dplyr::mutate(
      early_childbearing = ifelse(age < 20, "Yes", "No") |>
        factor(levels = c("Yes", "No")),
      livelihoods = ifelse(profession %in% c("None", "Student"), "No", "Yes") |>
        factor(levels = c("Yes", "No")),
      higher_education = ifelse(
        education_level %in% c("None", "Primary"), "No", "Yes"
      ) |>
        factor(levels = c("Yes", "No")),
      location = ifelse(
        address %in% c("Anomabo", "Biriwa", "Yamoransa"), 
        "Within community", "Outside community"
      ) |>
        factor(levels = c("Within community", "Outside community"))
    )
}


#'
#' Create ANC model data
#'

create_anc_model_data <- function(anc_data_model_recode) {
  anc_data_model_recode |>
    dplyr::select(
      haemoglobin, anaemia_status, early_childbearing, 
      livelihoods, higher_education, marital_status, location
    ) |>
    dplyr::mutate(
      anaemia_status = ifelse(anaemia_status == "anaemia", 1, 0),
      early_childbearing = ifelse(early_childbearing == "Yes", 1, 0),
      livelihoods = ifelse(livelihoods == "Yes", 1, 0),
      higher_education = ifelse(higher_education == "Yes", 1, 0),
      marital_status = ifelse(marital_status == "married", 1, 0),
      location = ifelse(location == "Within community", 1, 0),
    ) |>
    na.omit()
}
