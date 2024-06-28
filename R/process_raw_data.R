#'
#' Process raw ANC data
#' 
#' @param anc_data_raw A data.frame object of the raw ANC data
#' 
#' @returns A tibble of processed ANC data
#' 
#' @examples 
#' process_anc_data_raw(anc_data_raw)
#' 
#' @rdname process_anc
#' @export
#'

# process_anc_data_raw <- function(anc_data_raw) {
#   anc_data <- anc_data_raw |>
#     dplyr::select(-Test) |>
#     dplyr::mutate(sq_no = 1:nrow(anc_data_raw), .before = resDate) |>
#     dplyr::mutate(
#       Profile = dplyr::case_when(
#         stringr::str_detect(string = Profile, pattern = "HGB") ~ "haemoglobin",
#         stringr::str_detect(string = Profile, pattern = "Sickling") ~ "sickle_cell",
#         stringr::str_detect(string = Profile, pattern = "malaria") ~ "malaria"
#       ),
#       AttStatus = stringr::str_to_title(AttStatus)
#     ) |>
#     dplyr::group_by(Date_Of_Birth) |>
#     dplyr::mutate(unique_id = dplyr::cur_group_id(), .after = sq_no) |>
#     dplyr::ungroup() |>
#     dplyr::filter(!sq_no %in% c(245, 292, 227, 355, 106)) |>
#     dplyr::mutate(
#       unique_id = dplyr::case_when(
#         sq_no %in% 166:167 ~ 57,
#         sq_no %in% 186:187 ~ 571,
#         sq_no %in% 344:345 ~ 572,
#         sq_no %in% 30:31 ~ 86,
#         sq_no %in% 62:63 ~ 861,
#         sq_no %in% 161:162 ~ 862,
#         .default = unique_id
#       )
#     ) |>
#     dplyr::mutate(Profession = stringr::str_to_title(Profession)) |>
#     dplyr::filter(Profession != "Child") |>
#     dplyr::mutate(
#       Profession = dplyr::case_when(
#         Profession %in% c("Dress Making", "Tailor") ~ "Seamstress",
#         Profession == "Head Dresser" ~ "Hair Dresser",
#         Profession == "Busniss" ~ "Business Owner",
#         Profession == "Company" ~ "Company Employee",
#         Profession == "Treader" ~ "Trader",
#         Profession == "Decoration" ~ "Decorator",
#         Profession == "Fishmonga" ~ "Fishmonger",
#         .default = Profession
#       )
#     ) |>
#     dplyr::mutate(
#       EducationLevel = dplyr::case_when(
#         EducationLevel == "NONE" ~ "None",
#         EducationLevel == "PRIMARY" ~ "Primary",
#         stringr::str_detect(string = EducationLevel, pattern = "JHS|MIDDLE") ~ "Junior High School",
#         stringr::str_detect(string = EducationLevel, pattern = "SHS") ~ "Senior High School",
#         EducationLevel == "TERTIARY" ~ "Tertiary"
#       ),
#       MarriageStatus = stringr::str_to_lower(MarriageStatus),
#       Address = stringr::str_to_title(Address),
#       Address = dplyr::case_when(
#         Address == "Anobabo" ~ "Anomabo",
#         Address == "Egirfa" ~ "Egyirefa",
#         Address == "Yamoransah" ~ "Yamoransa",
#         .default = Address
#       )
#     ) |>
#     tidyr::pivot_wider(names_from = Profile, values_from = Result_Num) |>
#     dplyr::mutate(
#       sickle_cell = stringr::str_to_lower(sickle_cell),
#       malaria = ifelse(!is.na(malaria), "negative", malaria),
#       haemoglobin = as.numeric(haemoglobin)
#     ) |>
#     dplyr::rename(
#       result_date = resDate,
#       department = Department,
#       attendance_specialty = AttSpecialty,
#       attendance_status = AttStatus,
#       dob = Date_Of_Birth,
#       age = Age,
#       profession = Profession,
#       education_level = EducationLevel,
#       marital_status = MarriageStatus,
#       address = Address
#     )
#   
#   sickle_data <- anc_data |>
#     dplyr::filter(!is.na(sickle_cell)) |>
#     dplyr::select(-sq_no, -haemoglobin, -malaria)
#   
#   hb_data <- anc_data |>
#     dplyr::filter(!is.na(haemoglobin)) |>
#     dplyr::select(-sq_no, -sickle_cell, -malaria)
#   
#   mal_data <- anc_data |>
#     dplyr::filter(!is.na(malaria)) |>
#     dplyr::select(-sq_no, -sickle_cell, -haemoglobin)
#   
#   xx <- dplyr::full_join(
#     sickle_data, hb_data,
#     by = c("unique_id", "result_date", "department", "attendance_specialty", 
#       "attendance_status", "dob", "age", "profession", "education_level", 
#       "marital_status", "address")
#   ) |>
#     dplyr::full_join(
#       mal_data, 
#       by = c("unique_id", "result_date", "department", "attendance_specialty", 
#              "attendance_status", "dob", "age", "profession", "education_level", 
#              "marital_status", "address")
#     )
#   
#   xx
# }

process_anc_data_raw <- function(anc_data_raw) {
  ## Split data by Date of Birth ----
  yy <- anc_data_raw |>
    dplyr::select(-Test) |>
    dplyr::mutate(
      Profile = dplyr::case_when(
        stringr::str_detect(string = Profile, pattern = "HGB") ~ "haemoglobin",
        stringr::str_detect(string = Profile, pattern = "Sickling") ~ "sickle_cell",
        stringr::str_detect(string = Profile, pattern = "malaria") ~ "malaria"
      )
    ) |>
    split(f = anc_data_raw$Date_Of_Birth) |>
    (\(x) { names(x) <- seq_len(length(x)); x })()
  
  ## Get number of rows of each df in each date of birth grouping ----
  yy_rows <- lapply(yy, nrow)
  
  ## Create lists for each date of birth grouping based on number of rows ----
  yy1 <- yy[yy_rows == 1]
  yy2 <- yy[yy_rows == 2]
  yy3 <- yy[yy_rows == 3]
  yy4 <- yy[yy_rows == 4]
  yy5 <- yy[yy_rows == 5]
  yy6 <- yy[yy_rows == 6]
  yy7 <- yy[yy_rows == 7]
  
  ## Process dob groupings with only a single row of data (single test) ----
  zz1 <- dplyr::bind_rows(yy1, .id = "unique_id")
  
  zz2 <- c(yy2, yy2["26"], yy2["37"])
  names(zz2)[111] <- "210"
  names(zz2)[112] <- "211"
  
  zz2[["26"]]  <- zz2[["26"]][1, ]
  zz2[["210"]] <- zz2[["210"]][2, ]
  zz2[["37"]]  <- zz2[["37"]][1, ]
  zz2[["211"]] <- zz2[["211"]][2, ]
  
  zz2 <- dplyr::bind_rows(zz2, .id = "unique_id")
  
  zz3 <- c(yy3, yy3["94"], yy3["158"])
  names(zz3)[17] <- "212"
  names(zz3)[18] <- "213"
  
  zz3[["94"]] <- zz3[["94"]][1:2, ]
  zz3[["212"]] <- zz3[["212"]][3, ]
  
  zz3[["158"]] <- zz3[["158"]][1, ]
  zz3[["213"]] <- zz3[["213"]][2:3, ]
  
  zz3[["19"]]  <- zz3[["19"]][1:2, ]
  zz3[["38"]]  <- zz3[["38"]][2:3, ]
  zz3[["63"]]  <- zz3[["63"]][1:2, ]
  zz3[["100"]] <- zz3[["100"]][1:2, ]
  zz3[["105"]] <- zz3[["105"]][2:3, ]
  zz3[["201"]] <- zz3[["201"]][1:2, ]
  zz3[["203"]] <- zz3[["203"]][1:2, ]
  
  zz3 <- dplyr::bind_rows(zz3, .id = "unique_id")
  
  zz4 <- dplyr::bind_rows(yy4, .id = "unique_id") |>
    dplyr::slice(1:2)
  
  zz5 <- dplyr::bind_rows(yy5, .id = "unique_id") |>
    dplyr::slice(c(1, 4:5))
  
  zz6 <- c(yy6, yy6, yy6)
  names(zz6)[2] <- "214"
  names(zz6)[3] <- "215"
  
  zz6[["57"]] <- zz6[["57"]][1:2, ]
  zz6[["214"]] <- zz6[["214"]][3:4, ]
  zz6[["215"]] <- zz6[["215"]][5:6, ]
  
  zz6 <- dplyr::bind_rows(zz6, .id = "unique_id")
  
  zz7 <- c(yy7, yy7, yy7, yy7)
  names(zz7)[2] <- "216"
  names(zz7)[3] <- "217"
  names(zz7)[4] <- "218"
  
  zz7[["86"]] <- zz7[["86"]][1:2, ]
  zz7[["216"]] <- zz7[["216"]][3:4, ]
  zz7[["217"]] <- zz7[["217"]][5, ]
  zz7[["218"]] <- zz7[["218"]][6:7, ]
  
  zz7 <- dplyr::bind_rows(zz7, .id = "unique_id")
  
  anc_data <- rbind(zz1, zz2, zz3, zz4, zz5, zz6, zz7) |>
    dplyr::select(
      unique_id, Profile, Result_Num, Age, Profession, EducationLevel, 
      MarriageStatus, Address
    ) |>
    dplyr::mutate(unique_id = as.integer(unique_id)) |>
    tidyr::pivot_wider(names_from = Profile, values_from = Result_Num) |>
    dplyr::arrange(unique_id) |>
    dplyr::mutate(
      Profession = stringr::str_to_title(Profession),
      Profession = dplyr::case_when(
        Profession %in% c("Dress Making", "Tailor") ~ "Seamstress",
        Profession == "Head Dresser" ~ "Hair Dresser",
        Profession == "Busniss" ~ "Business Owner",
        Profession == "Company" ~ "Company Employee",
        Profession == "Treader" ~ "Trader",
        Profession == "Decoration" ~ "Decorator",
        Profession == "Fishmonga" ~ "Fishmonger",
        Profession == "Farming" ~ "Farmer",
        Profession == "Catre"~"Caterer",
        .default = Profession
      ),
      EducationLevel = dplyr::case_when(
        EducationLevel == "NONE" ~ "None",
        EducationLevel == "PRIMARY" ~ "Primary",
        stringr::str_detect(string = EducationLevel, pattern = "JHS|MIDDLE") ~ "Junior High School",
        stringr::str_detect(string = EducationLevel, pattern = "SHS") ~ "Senior High School",
        EducationLevel == "TERTIARY" ~ "Tertiary"
      ),
      MarriageStatus = stringr::str_to_lower(MarriageStatus),
      Address = stringr::str_to_title(Address),
      Address = dplyr::case_when(
        Address == "Anobabo" ~ "Anomabo",
        Address == "Egirfa" ~ "Egyirefa",
        Address == "Yamoransah" ~ "Yamoransa",
        .default = Address
      ),
      haemoglobin = as.numeric(haemoglobin),
      sickle_cell = stringr::str_to_lower(sickle_cell),
      malaria = ifelse(
        stringr::str_detect(string = malaria, pattern = "mps"),
        "negative", "positive"
      )
    ) |>
    dplyr::rename(
      age = Age,
      profession = Profession,
      education_level = EducationLevel,
      marital_status = MarriageStatus,
      address = Address
    )
  
  ## Filter out record with age of 1 year old ----
  anc_data <- anc_data |>
    dplyr::filter(age != 1)
  
  ## Return anc_data ----
  anc_data
}


#'
#' 
#'
#'
#'
create_anc_data_processed_csv <- function(anc_data_processed) {
  write.csv(
    x = anc_data_processed,
    file = "data/anc_data_processed.csv",
    row.names = FALSE
  )
  
  "data/anc_data_processed.csv"
}



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