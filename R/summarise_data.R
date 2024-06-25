#'
#' Create summary table/s of dataset
#'

summarise_anc_data_univariate <- function(anc_data_recode,
                                          simplify = TRUE) {
  # cont_vars <- anc_data_recode |>
  #   dplyr::summarise(
  #     median_age = median(age, na.rm = TRUE),
  #     median_hb = median(haemoglobin, na.rm = TRUE)
  #   ) |>
  #   tidyr::pivot_longer(
  #     cols = dplyr::everything(),
  #     names_to = "var",
  #     values_to = "value1"
  #   ) |>
  #   cbind(
  #     anc_data_recode |>
  #       dplyr::summarise(
  #         conf_int_age = boxplot.stats(age)$conf |>
  #           round(digits = 1) |>
  #           paste(collapse = ", "),
  #         conf_int_hb = boxplot.stats(haemoglobin)$conf |>
  #           round(digits = 1) |>
  #           paste(collapse = ", ")
  #       ) |>
  #       tidyr::pivot_longer(
  #         cols = dplyr::everything(),
  #         names_to = "var",
  #         values_to = "value2"
  #       ) |>
  #       dplyr::select(value2)
  #   )
  
  summary_tab <- list(
    `Age` = anc_data_recode |>
      dplyr::summarise(
        `Median Age` = median(age, na.rm = TRUE)
      ) |>
      tidyr::pivot_longer(
        cols = dplyr::everything(),
        names_to = "var",
        values_to = "value1"
      ) |>
      cbind(
        anc_data_recode |>
          dplyr::summarise(
            conf_int_age = boxplot.stats(age)$conf |>
              round(digits = 1) |>
              paste(collapse = ", ")
          ) |>
          tidyr::pivot_longer(
            cols = dplyr::everything(),
            names_to = "var",
            values_to = "value2"
          ) |>
          dplyr::select(value2)
      ) |>
      dplyr::mutate(value = paste0(value1, " (", value2, ")")) |>
      dplyr::select(var, value),
    `Age Group` = anc_data_recode |>
      dplyr::count(age_group) |>
      dplyr::arrange(dplyr::desc(n)) |>
      dplyr::mutate(
        n = paste0(
          n, " (", 
          scales::label_percent()(n / unique(anc_data_recode$total_n)),
          ")"
        )
      ) |>
      dplyr::rename(var = age_group, value = n),
    `Haemoglobin` = anc_data_recode |>
      dplyr::summarise(
        `Median Hb` = median(haemoglobin, na.rm = TRUE)
      ) |>
      tidyr::pivot_longer(
        cols = dplyr::everything(),
        names_to = "var",
        values_to = "value1"
      ) |>
      cbind(
        anc_data_recode |>
          dplyr::summarise(
            conf_int_age = boxplot.stats(haemoglobin)$conf |>
              round(digits = 1) |>
              paste(collapse = ", ")
          ) |>
          tidyr::pivot_longer(
            cols = dplyr::everything(),
            names_to = "var",
            values_to = "value2"
          ) |>
          dplyr::select(value2)
      ) |>
      dplyr::mutate(value = paste0(value1, " (", value2, ")")) |>
      dplyr::select(var, value),
    `Anaemia Status` = anc_data_recode |>
      dplyr::count(anaemia_status) |>
      dplyr::arrange(dplyr::desc(n)) |>
      dplyr::mutate(
        n = paste0(
          n, " (", 
          scales::label_percent()(n / unique(anc_data_recode$total_n)),
          ")"
        )
      ) |>
      dplyr::rename(var = anaemia_status, value = n),
    `Anaemia Category` = anc_data_recode |>
      dplyr::count(anaemia_category) |>
      dplyr::arrange(dplyr::desc(n)) |>
      dplyr::mutate(
        n = paste0(
          n, " (", 
          scales::label_percent()(n / unique(anc_data_recode$total_n)),
          ")"
        )
      ) |>
      dplyr::rename(var = anaemia_category, value = n),
    Profession = anc_data_recode |>
      dplyr::count(profession) |>
      dplyr::arrange(dplyr::desc(n)) |>
      dplyr::mutate(
        n = paste0(
          n, " (", 
          scales::label_percent()(n / unique(anc_data_recode$total_n)),
          ")"
        )
      ) |>
      dplyr::rename(var = profession, value = n),
    `Education Level` = anc_data_recode |>
      dplyr::count(education_level) |>
      dplyr::arrange(dplyr::desc(n)) |>
      dplyr::mutate(
        n = paste0(
          n, " (", 
          scales::label_percent()(n / unique(anc_data_recode$total_n)),
          ")"
        )
      ) |>
      dplyr::rename(var = education_level, value = n),
    `Marital Status` = anc_data_recode |>
      dplyr::count(marital_status) |>
      dplyr::arrange(dplyr::desc(n)) |>
      dplyr::mutate(
        n = paste0(
          n, " (", 
          scales::label_percent()(n / unique(anc_data_recode$total_n)),
          ")"
        )
      ) |>
      dplyr::rename(var = marital_status, value = n),
    `Address` = anc_data_recode |>
      dplyr::count(address) |>
      dplyr::arrange(dplyr::desc(n)) |>
      dplyr::mutate(
        n = paste0(
          n, " (", 
          scales::label_percent()(n / unique(anc_data_recode$total_n)),
          ")"
        )
      ) |>
      dplyr::rename(var = address, value = n),
    `Sickle Cell` = anc_data_recode |>
      dplyr::count(sickle_cell) |>
      dplyr::arrange(dplyr::desc(n)) |>
      dplyr::mutate(
        n = paste0(
          n, " (", 
          scales::label_percent()(n / unique(anc_data_recode$total_n)),
          ")"
        )
      ) |>
      dplyr::rename(var = sickle_cell, value = n),
    `Malaria` = anc_data_recode |>
      dplyr::count(malaria) |>
      dplyr::arrange(dplyr::desc(n)) |>
      dplyr::mutate(
        n = paste0(
          n, " (", 
          scales::label_percent()(n / unique(anc_data_recode$total_n)),
          ")"
        )
      ) |>
      dplyr::rename(var = malaria, value = n)
  )
  
  summary_tab <- lapply(
    X = summary_tab,
    FUN = function(x) tibble::tibble(
      rbind(
        x[!is.na(x$var), ],
        x[is.na(x$var), ]
      )
    )
  )
  
  if (simplify) {
    summary_tab <- summary_tab |>
      dplyr::bind_rows(.id = "var_set")
  }
  
  summary_tab
}

