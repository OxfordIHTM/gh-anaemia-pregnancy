#'
#'
#'
#'

process_anc_data_raw <- function(anc_data_raw) {
  anc_data <- anc_data_raw |>
    dplyr::select(-Test) |>
    dplyr::mutate(sq_no = 1:nrow(anc_data_raw), .before = resDate) |>
    dplyr::mutate(
      Profile = dplyr::case_when(
        stringr::str_detect(string = Profile, pattern = "HGB") ~ "haemoglobin",
        stringr::str_detect(string = Profile, pattern = "Sickling") ~ "sickle_cell",
        stringr::str_detect(string = Profile, pattern = "malaria") ~ "malaria"
      ),
      AttStatus = stringr::str_to_title(AttStatus)
    ) |>
    dplyr::group_by(Date_Of_Birth) |>
    dplyr::mutate(unique_id = dplyr::cur_group_id(), .after = sq_no) |>
    dplyr::ungroup() |>
    dplyr::filter(!sq_no %in% c(245, 292, 227, 355, 106)) |>
    dplyr::mutate(
      unique_id = dplyr::case_when(
        sq_no %in% 166:167 ~ 57,
        sq_no %in% 186:187 ~ 571,
        sq_no %in% 344:345 ~ 572,
        sq_no %in% 30:31 ~ 86,
        sq_no %in% 62:63 ~ 861,
        sq_no %in% 161:162 ~ 862,
        .default = unique_id
      )
    ) |>
    dplyr::mutate(Profession = stringr::str_to_title(Profession)) |>
    dplyr::filter(Profession != "Child") |>
    dplyr::mutate(
      Profession = dplyr::case_when(
        Profession %in% c("Dress Making", "Tailor") ~ "Seamstress",
        Profession == "Head Dresser" ~ "Hair Dresser",
        Profession == "Busniss" ~ "Business Owner",
        Profession == "Company" ~ "Company Employee",
        Profession == "Treader" ~ "Trader",
        Profession == "Decoration" ~ "Decorator",
        Profession == "Fishmonga" ~ "Fishmonger",
        .default = Profession
      )
    ) |>
    dplyr::mutate(
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
      )
    ) |>
    tidyr::pivot_wider(names_from = Profile, values_from = Result_Num) |>
    dplyr::mutate(
      sickle_cell = stringr::str_to_lower(sickle_cell),
      malaria = ifelse(!is.na(malaria), "negative", malaria),
      haemoglobin = as.numeric(haemoglobin)
    ) |>
    dplyr::rename(
      result_date = resDate,
      department = Department,
      attendance_specialty = AttSpecialty,
      attendance_status = AttStatus,
      dob = Date_Of_Birth,
      age = Age,
      profession = Profession,
      education_level = EducationLevel,
      marital_status = MarriageStatus,
      address = Address
    )
  
  sickle_data <- anc_data |>
    dplyr::filter(!is.na(sickle_cell)) |>
    dplyr::select(-sq_no, -haemoglobin, -malaria)
  
  hb_data <- anc_data |>
    dplyr::filter(!is.na(haemoglobin)) |>
    dplyr::select(-sq_no, -sickle_cell, -malaria)
  
  mal_data <- anc_data |>
    dplyr::filter(!is.na(malaria)) |>
    dplyr::select(-sq_no, -sickle_cell, -haemoglobin)
  
  xx <- dplyr::full_join(
    sickle_data, hb_data,
    by = c("unique_id", "result_date", "department", "attendance_specialty", 
      "attendance_status", "dob", "age", "profession", "education_level", 
      "marital_status", "address")
  ) |>
    dplyr::full_join(
      mal_data, 
      by = c("unique_id", "result_date", "department", "attendance_specialty", 
             "attendance_status", "dob", "age", "profession", "education_level", 
             "marital_status", "address")
    )
  
  xx
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