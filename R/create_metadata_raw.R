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