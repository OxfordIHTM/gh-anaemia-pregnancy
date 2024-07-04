#'
#' Create metadata for raw data
#' 
#' @param anc_data_raw A data.frame object containing raw ANC data
#' 
#' @returns A tibble of raw ANC data metadata composed of *field name*, 
#'   *field type*, and *field description* of `anc_data_raw`
#'   
#' @examples
#' create_metadata_raw(anc_data_raw)
#' 
#' @rdname create_metadata
#' @export
#'

create_metadata_raw <- function(anc_data_raw) {
  ## Get names of fields ----
  field_name <- names(anc_data_raw)
  
  ## Get class of fields ----
  field_type <- lapply(X = anc_data_raw, FUN = class) |>
    unlist()
  
  ## Field description ----
  field_description <- c(
    "Date of results",
    "Type of test performed",
    "Indicator/profile test is specific for",
    "Test result value",
    "Department in which attendee was seen",
    "Specialty in which attendee was seen",
    "Status of attendee",
    "Date of birth of attendee",
    "Age of attendee",
    "Profession of attendee",
    "Education level of attendee",
    "Marriage status of attendee",
    "Address of attendee"
  )
  
  field_values <- c(
    "date values",
    unique(anc_data_raw$Test) |> paste(collapse = "; "),
    unique(anc_data_raw$Profile) |> paste(collapse = "; "),
    unique(anc_data_raw$Result_Num) |> paste(collapse = "; "),
    unique(anc_data_raw$Department) |> paste(collapse = "; "),
    unique(anc_data_raw$AttSpecialty) |> paste(collapse = "; "),
    unique(anc_data_raw$AttStatus) |> paste(collapse = "; "),
    "date values",
    "age values",
    unique(anc_data_raw$Profession) |> paste(collapse = "; "),
    unique(anc_data_raw$EducationLevel) |> paste(collapse = "; "),
    unique(anc_data_raw$MarriageStatus) |> paste(collapse = "; "),
    unique(anc_data_raw$Address) |> paste(collapse = "; ")
  )
  
  data.frame(field_name, field_type, field_description, field_values) |>
    tibble::tibble()
}


#'
#' Create CSV for metadata for raw ANC data
#' 
#' @param anc_raw_data_metadata A data.frame object for ANC raw data metadata
#' 
#' @returns A file path to raw ANC data metadata CSV
#'
#' @examples
#' create_metadata_raw_csv(anc_data_raw_metadata)
#' 
#' @rdname create_metadata
#' @export
#'

create_metadata_raw_csv <- function(anc_data_raw_metadata) {
  write.csv(
    x = anc_data_raw_metadata,
    file = "metadata/anc_data_raw_metadata.csv",
    row.names = FALSE
  )
  
  "metadata/anc_data_raw_metadata.csv"
}


#'
#' Create metadata for processed ANC data
#' 
#' @param anc_data_processed A data.frame object for processed ANC data
#' 
#' @returns A tibble of processed ANC data metadata composed of *field name*, 
#'   *field type*, and *field description* of `anc_data_processed`
#'   
#' @examples
#' create_metadata_processed(anc_data_processed)
#' 
#' @rdname create_metadata
#' @export
#'

create_metadata_processed <- function(anc_data_processed) {
  ## Get names of fields ----
  field_name <- names(anc_data_processed)
  
  ## Get class of fields ----
  field_type <- lapply(X = anc_data_processed, FUN = class) |>
    unlist()
  
  ## Field description ----
  field_description <- c(
    "Unique identifier",
    "Month woman attended antenatal care",
    "Year woman attended antenatal care",
    "Age of woman attending antenatal care in years",
    "Profession of woman attending antenatal care",
    "Profession of woman attending antenatal care (summary)",
    "Education level of woman attending antenatal care",
    "Education level of woman attending antenatal care (summary)",
    "Marital status of woman attending antenatal care",
    "Address (in general settelment/neighbourhood terms)",
    "Haemoglobin value (in micrograms/L)",
    "Result of sickle cell test",
    "Result of thin blood film test for malaria parasite"
  )
  
  field_values <- c(
    "unique identifier values",
    "age values in years",
    "month values",
    "year: 2023 or 2024",
    unique(anc_data_processed$profession) |> paste(collapse = "; "),
    "summary categories for profession",
    unique(anc_data_processed$education_level) |> paste(collapse = "; "),
    "summary categories for education level",
    unique(anc_data_processed$marital_status) |> paste(collapse = "; "),
    unique(anc_data_processed$address) |> paste(collapse = "; "),
    "haemoglobin values in microgram/L",
    "positive = positive for sickle cell; negative = negative for sickle cell",
    "positive = positive for malaria; negative = negative for malaria"
  )
  
  data.frame(field_name, field_type, field_description, field_values) |>
    tibble::tibble()
}


#'
#' Create CSV for metadata for processed ANC data
#' 
#' @param anc_processed_data_metadata A data.frame object for ANC processsed 
#'   data metadata
#' 
#' @returns A file path to processed ANC data metadata CSV
#'
#' @examples
#' create_metadata_processed_csv(anc_data_processed_metadata)
#' 
#' @rdname create_metadata
#' @export
#'

create_metadata_processed_csv <- function(anc_data_processed_metadata) {
  write.csv(
    x = anc_data_processed_metadata,
    file = "metadata/anc_data_processed_metadata.csv",
    row.names = FALSE
  )
  
  "metadata/anc_data_processed_metadata.csv"
}

