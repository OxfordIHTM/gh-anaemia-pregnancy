#'
#' Create summary table/s of dataset
#'

summarise_anc_data_univariate <- function(anc_data_recode,
                                          simplify = TRUE) {
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
      #dplyr::arrange(dplyr::desc(n)) |>
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
      #dplyr::arrange(dplyr::desc(n)) |>
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


#'
#' Create summary 2 x 2 table for outcome and a continuous variable predictor
#'

summarise_bivariate_continuous <- function(df, 
                                           outcome, 
                                           predictor,
                                           predictor_label = predictor) {
  df |>
    dplyr::filter(!is.na(.data[[outcome]])) |>
    dplyr::group_by(.data[[outcome]]) |>
    dplyr::summarise(
      var = predictor_label,
      value = paste0(
        median(.data[[predictor]], na.rm = TRUE), " (", 
        paste(
          round(boxplot.stats(.data[[predictor]])$conf, digits = 1), 
          collapse = ", "), ")"
      ),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = dplyr::all_of(outcome), 
      values_from = value
    )
}

#'
#' Create summary 2 x 2 table for outcome and a categorical variable predictor
#'

summarise_bivariate_categorical <- function(df,
                                            outcome,
                                            predictor,
                                            predictor_label = predictor) {
  df_outcome_total <- df |>
    dplyr::filter(!is.na(.data[[outcome]])) |>
    dplyr::group_by(.data[[outcome]]) |>
    dplyr::summarise(total_n = n())
    
  df |>
    dplyr::filter(!is.na(.data[[outcome]])) |>
    dplyr::group_by(.data[[outcome]]) |>
    dplyr::count(.data[[outcome]], .data[[predictor]]) |>
    dplyr::ungroup() |>
    dplyr::left_join(df_outcome_total, by = outcome) |>
    dplyr::mutate(
      value = paste0(
        stringr::str_pad(n, width = 3, side = "left"), 
        #" (",
        stringr::str_pad(
          paste0(" (", scales::label_percent(accuracy = 0.1)(n / total_n)),
          width = 5, side = "left"
        ), 
        ")"
      )
    ) |>
    dplyr::select(-n, -total_n) |>
    tidyr::pivot_wider(
      names_from = dplyr::all_of(outcome), 
      values_from = value
    ) |>
    dplyr::rename_with(
      .fn = function(x) c("var", "anaemia", "no anaemia")
    )
}

#'
#' Create bivariate summary table
#'

summarise_bivariate <- function(df,
                                outcome,
                                predictor, 
                                predictor_label = predictor,
                                predictor_type = c("continuous", "categorical")) {
  predictor_type <- match.arg(predictor_type)
  
  eval(
    parse(
      text = paste0(
        "summarise_bivariate_", predictor_type, 
        "(df = df, outcome = outcome, predictor = predictor, predictor_label = predictor_label)"
      )
    )
  )
}

#'
#' Create overall summary table - bivariate
#'

summarise_anc_data_bivariate <- function(anc_data_recode,
                                         outcome = "anaemia_status",
                                         predictor = c("age", "age_group", 
                                                       "haemoglobin", 
                                                       "profession", 
                                                       "education_level", 
                                                       "marital_status", 
                                                       "address", 
                                                       "sickle_cell", 
                                                       "malaria"),
                                         predictor_label = c("Age", "Age Group",
                                                             "Haemoglobin",
                                                             "Profession",
                                                             "Education Level",
                                                             "Marital Status",
                                                             "Address",
                                                             "Sickle Cell",
                                                             "Malaria"),
                                         predictor_type = c("continuous",
                                                            "categorical",
                                                            "continuous",
                                                            rep("categorical", 6)),
                                         simplify = TRUE) {
  summary_tab <- Map(
    f = summarise_bivariate,
    df = rep(list(anc_data_recode), length(predictor)),
    outcome = rep(as.list(outcome), length(predictor)),
    predictor = as.list(predictor),
    predictor_type = as.list(predictor_type)
  )
  
  names(summary_tab) <- predictor_label
  
  if (simplify) {
    summary_tab <- summary_tab |>
      dplyr::bind_rows(.id = "var_set")
  }
  
  summary_tab
}
