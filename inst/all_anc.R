####data cleaning####
anc_data_processed07 <- read_csv("data/anc_data_processed07.csv")
#all data
anc_data_processed_subset <- anc_data_processed07 %>%
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
    anaemia_category = case_when(
      haemoglobin >= 10 & haemoglobin < 11 ~ "Mild Anaemia",
      haemoglobin >= 7 & haemoglobin < 10 ~ "Moderate Anaemia",
      haemoglobin < 7 ~ "Severe Anaemia",
      haemoglobin >= 11 ~ "Non-anaemic",
      TRUE ~ NA_character_
    )
  )


####power####
# 
calculate_power <- function(observed_table, chi_square_statistic, df) {
  n <- sum(observed_table)
  w <- sqrt(chi_square_statistic / n)
  power_result <- pwr.chisq.test(w = w, N = n, df = df, sig.level = 0.05)
  return(power_result$power)
}
####chisqur,pvalue,powe####
# Age group summary
ag_cross_tab <- table(anc_data_processed_subset$age_group, anc_data_processed_subset$anaemia_status)
ag_chi_square_test <- chisq.test(ag_cross_tab)
ag_power <- calculate_power(ag_cross_tab, ag_chi_square_test$statistic, ag_chi_square_test$parameter)
ag_result <- data.frame(
  Variable = "Age Group",
  Chi_Square = ag_chi_square_test$statistic,
  DF = ag_chi_square_test$parameter,
  P_Value = ag_chi_square_test$p.value,
  Power = ag_power
)

# Education summary
ed_cross_tab <- table(anc_data_processed_subset$education_level_summary, anc_data_processed_subset$anaemia_status)
ed_chi_square_test <- chisq.test(ed_cross_tab)
ed_power <- calculate_power(ed_cross_tab, ed_chi_square_test$statistic, ed_chi_square_test$parameter)
ed_result <- data.frame(
  Variable = "Education Level",
  Chi_Square = ed_chi_square_test$statistic,
  DF = ed_chi_square_test$parameter,
  P_Value = ed_chi_square_test$p.value,
  Power = ed_power
)

# Profession summary
po_cross_tab <- table(anc_data_processed_subset$profession_summary1, anc_data_processed_subset$anaemia_status)
po_chi_square_test <- chisq.test(po_cross_tab)
po_power <- calculate_power(po_cross_tab, po_chi_square_test$statistic, po_chi_square_test$parameter)
po_result <- data.frame(
  Variable = "Profession",
  Chi_Square = po_chi_square_test$statistic,
  DF = po_chi_square_test$parameter,
  P_Value = po_chi_square_test$p.value,
  Power = po_power
)

# Location summary
lo_cross_tab <- table(anc_data_processed_subset$address_group, anc_data_processed_subset$anaemia_status)
lo_chi_square_test <- chisq.test(lo_cross_tab)
lo_power <- calculate_power(lo_cross_tab, lo_chi_square_test$statistic, lo_chi_square_test$parameter)
lo_result <- data.frame(
  Variable = "Location",
  Chi_Square = lo_chi_square_test$statistic,
  DF = lo_chi_square_test$parameter,
  P_Value = lo_chi_square_test$p.value,
  Power = lo_power
)

# Marital summary
ma_cross_tab <- table(anc_data_processed_subset$marital_status, anc_data_processed_subset$anaemia_status)
ma_chi_square_test <- chisq.test(ma_cross_tab)
ma_power <- calculate_power(ma_cross_tab, ma_chi_square_test$statistic, ma_chi_square_test$parameter)
ma_result <- data.frame(
  Variable = "Marital Status",
  Chi_Square = ma_chi_square_test$statistic,
  DF = ma_chi_square_test$parameter,
  P_Value = ma_chi_square_test$p.value,
  Power = ma_power
)

# Sickle cell summary
si_cross_tab <- table(anc_data_processed_subset$sickle_cell, anc_data_processed_subset$anaemia_status)
si_chi_square_test <- chisq.test(si_cross_tab)
si_power <- calculate_power(si_cross_tab, si_chi_square_test$statistic, si_chi_square_test$parameter)
si_result <- data.frame(
  Variable = "Sickle Cell",
  Chi_Square = si_chi_square_test$statistic,
  DF = si_chi_square_test$parameter,
  P_Value = si_chi_square_test$p.value,
  Power = si_power
)



# Combine results
combined_results <- bind_rows(
  ed_result,
  po_result,
  lo_result,
  ma_result,
  si_result
)

print(combined_results)