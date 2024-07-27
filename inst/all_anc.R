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
    education_level_summary1= case_when(
      education_level %in% c( "None", "Primary") ~ "no education and Primary",
      education_level %in% c("Junior High School", "Senior High School", "Tertiary") ~ "Junior high school and higher",
      TRUE ~ "NA"
    ),
    profession_group= case_when(
      profession %in% c("None","Student" ) ~ "non Income",
      profession %in% c("Teacher", "Company Employee", "Midwife","Undertaker", "Trader", "Seamstress", "Hair Dresser", "Fishmonger", "Farmer", "Decorator", "Caterer", "Business Owner") ~ "Employed",
      TRUE ~ "NA"
    ),
    location_group= case_when(
      address %in% c( "Makassium","Saltpond Zongo","Anomabo", "Biriwa", "Abandze", "Yamoransa","Kormantse","Ekon","Cape Coast") ~ "Urban",
      address %in% c("Amoanda","Buranamoah","Moree","Asafora", "Aketekyiwa","Egyirefa","Pomasi(Pomase)", "Waakrom", "Afrago Junction", "Amissakrom", "Egyierefa","Insanfo(NSANFO)","Ekotokrom","Amissakrom","Eguase") ~ "Rural Town",
      TRUE ~ "NA"
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


####univariable before grouping####--------------------------------------------------------------------------------------

# education levels
education_counts <- table(anc_data_processed_subset$education_level)
print(education_counts)
education_percentages <- prop.table(education_counts) * 100
education <- data.frame(
  Education_Level = names(education_counts),
  Count_and_Percentage = paste(education_counts, "(", round(education_percentages, 2), "%)", sep = "")
)


# profession
profession_counts <- table(anc_data_processed_subset$profession)
print(profession_counts)
profession_percentages <- prop.table(profession_counts) * 100
Profession <- data.frame(
  Profession_Level = names(profession_counts),
  Count_and_Percentage = paste(profession_counts, "(", round(profession_percentages, 2), "%)", sep = "")
)
#address
address_counts <- table(anc_data_processed_subset$address)
print(address_counts)
address_percentages <- prop.table(address_counts) * 100
Address <- data.frame(
  Address_Level = names(address_counts),
  Count_and_Percentage = paste(address_counts, "(", round(address_percentages, 2), "%)", sep = "")
)
print(Address)

#marital status
marital_status_counts <- table(anc_data_processed_subset$marital_status)
print(marital_status_counts)
marital_status_percentages <- prop.table(marital_status_counts) * 100
Marital_Status <- data.frame(
  Marital_Status_Level = names(marital_status_counts),
  Count_and_Percentage = paste(marital_status_counts, "(", round(marital_status_percentages, 2), "%)", sep = "")
)
print(Marital_Status)


#sickle cell
sickle_cell_counts <- table(anc_data_processed_subset$sickle_cell)
print(sickle_cell_counts)
sickle_cell_percentages <- prop.table(sickle_cell_counts) * 100
Sickle_Cell <- data.frame(
  Sickle_Cell_Level = names(sickle_cell_counts),
  Count_and_Percentage = paste(sickle_cell_counts, "(", round(sickle_cell_percentages, 2), "%)", sep = "")
)
print(Sickle_Cell)

####univariable grouping data####
#education summary
education_level_summary_counts <- table(anc_data_processed_subset$education_level_summary)
print(education_level_summary_counts)
education_level_summary_percentages <- prop.table(education_level_summary_counts) * 100
education_level_summary <- data.frame(
  Education_level_Summary_Level = names(education_level_summary_counts),
  Count_and_Percentage = paste(education_level_summary_counts, "(", round(education_level_summary_percentages, 2), "%)", sep = "")
)
print(education_level_summary)
#non and primary, junior high and higher
education_level_summary1_counts <- table(anc_data_processed_subset$education_level_summary1)
print(education_level_summary1_counts)
education_level_summary1_percentages <- prop.table(education_level_summary1_counts) * 100
education_level_summary1 <- data.frame(
  Education_level_Summary1 = names(education_level_summary1_counts),
  Count_and_Percentage = paste(education_level_summary1_counts, "(", round(education_level_summary1_percentages, 2), "%)", sep = "")
)
print(education_level_summary1)





#profession summary
profession_summary1_counts <- table(anc_data_processed_subset$profession_summary1)
print(profession_summary1_counts)
profession_summary1_percentages <- prop.table(profession_summary1_counts) * 100
profession_summary1 <- data.frame(
  Profession_Summary1 = names(profession_summary1_counts),
  Count_and_Percentage = paste(profession_summary1_counts, "(", round(profession_summary1_percentages, 2), "%)", sep = "")
)
print(profession_summary1)

#profession group(emproye)
profession_group_counts <- table(anc_data_processed_subset$profession_group)
print(profession_group_counts)
profession_group_percentages <- prop.table(profession_group_counts) * 100
profession_group <- data.frame(
  Profession_group = names(profession_group_counts),
  Count_and_Percentage = paste(profession_group_counts, "(", round(profession_group_percentages, 2), "%)", sep = "")
)
print(profession_group)


#location by urban or rural
location_counts <- table(anc_data_processed_subset$location_group)
print(location_counts)
location_percentages <- prop.table(location_counts) * 100
Location <- data.frame(
  Location = names(location_counts),
  Count_and_Percentage = paste(location_counts, "(", round(location_percentages, 2), "%)", sep = "")
)
print(Location)

#by distance
address_group2_counts <- table(anc_data_processed_subset$address_group2)
print(address_group2_counts)
address_group2_percentages <- prop.table(address_group2_counts) * 100
Address_group2 <- data.frame(
  Address_Level = names(address_group2_counts),
  Count_and_Percentage = paste(address_group2_counts, "(", round(address_group2_percentages, 2), "%)", sep = "")
)
print(Address_group2)
####no grouping bivariate analysis####
#education 
ed_l_cross_tab <- table(anc_data_processed_subset$education_level, anc_data_processed_subset$anaemia_status)
print(ed_l_cross_tab)
# Calculate percentages
ed_l_percentages <- prop.table(ed_l_cross_tab, margin = 2) * 100
ed_l_summary <- as.data.frame.matrix(ed_l_cross_tab)
ed_l_summary$Education_Level <- rownames(ed_l_summary)
rownames(ed_l_summary) <- NULL
for (col in colnames(ed_cross_tab)) {
  ed_l_summary[[col]] <- paste0(ed_l_summary[[col]], " (", round(ed_l_percentages[, col], 2), "%)")
}

ed_l_summary <- ed_l_summary %>%
  select(Education_Level, everything())
print(ed_l_summary)



#profession 
po_tab <- table(anc_data_processed_subset$profession, anc_data_processed_subset$anaemia_status)
print(po_tab)
po_percentages0 <- prop.table(po_tab, margin = 2) * 100
po_summary0 <- as.data.frame.matrix(po_tab)
po_summary0$Profession <- rownames(po_summary0)
rownames(po_summary0) <- NULL
for (col in colnames(po_tab)) {
  po_summary0[[col]] <- paste0(po_summary0[[col]], " (", round(po_percentages0[, col], 2), "%)")
}
po_summary0 <- po_summary0 %>%
  select(Profession, everything())
print(po_summary0)



#adress
ad_cross_tab0 <- table(anc_data_processed_subset$address, anc_data_processed_subset$anaemia_status)
print(ad_cross_tab0)
ad_percentages0 <- prop.table(ad_cross_tab0, margin = 2) * 100
ad_summary0 <- as.data.frame.matrix(ad_cross_tab0)
ad_summary0$Address <- rownames(ad_summary0)
rownames(ad_summary0) <- NULL
for (col in colnames(ad_cross_tab0)) {
  ad_summary0[[col]] <- paste0(ad_summary0[[col]], " (", round(ad_percentages0[, col], 2), "%)")
}
ad_summary0 <- ad_summary0 %>%
  select(Address, everything())
print(ad_summary0)


####summary of grouping bivariate analysis####------------------------------------------------------------------------------------
#age group summary
ag_cross_tab <- table(anc_data_processed_subset$age_group, anc_data_processed_subset$anaemia_status)
print(ag_cross_tab)
ag_chi_square_test <- chisq.test(ag_cross_tab)
print(ag_chi_square_test)
# Calculate percentages
ag_percentages <- prop.table(ag_cross_tab, margin = 2) * 100
ag_summary <- as.data.frame.matrix(ag_cross_tab)
ag_summary$Age_Group <- rownames(ag_summary)
rownames(ag_summary) <- NULL
for (col in colnames(ag_cross_tab)) {
  ag_summary[[col]] <- paste0(ag_summary[[col]], " (", round(ag_percentages[, col], 2), "%)")
}
ag_summary <- ag_summary %>%
  select(Age_Group, everything())
print(ag_summary)


#education summary
ed_cross_tab <- table(anc_data_processed_subset$education_level_summary, anc_data_processed_subset$anaemia_status)
print(ed_cross_tab)
ed_chi_square_test <- chisq.test(ed_cross_tab)
print(ed_chi_square_test)
# Calculate percentages
ed_percentages <- prop.table(ed_cross_tab, margin = 2) * 100
ed_summary <- as.data.frame.matrix(ed_cross_tab)
ed_summary$Education_Level <- rownames(ed_summary)
rownames(ed_summary) <- NULL
for (col in colnames(ed_cross_tab)) {
  ed_summary[[col]] <- paste0(ed_summary[[col]], " (", round(ed_percentages[, col], 2), "%)")
}

ed_summary <- ed_summary %>%
  select(Education_Level, everything())
print(ed_summary)


#education level summary1(no and primary together)
ed1_cross_tab <- table(anc_data_processed_subset$education_level_summary1, anc_data_processed_subset$anaemia_status)
print(ed1_cross_tab)
ed1_chi_square_test <- chisq.test(ed1_cross_tab)
print(ed1_chi_square_test)
# Calculate percentages
ed1_percentages <- prop.table(ed1_cross_tab, margin = 2) * 100
ed1_summary <- as.data.frame.matrix(ed1_cross_tab)
ed1_summary$Education_Level <- rownames(ed1_summary)
rownames(ed1_summary) <- NULL
for (col in colnames(ed_cross_tab)) {
  ed1_summary[[col]] <- paste0(ed1_summary[[col]], " (", round(ed1_percentages[, col], 2), "%)")
}

ed1_summary <- ed1_summary %>%
  select(Education_Level, everything())
print(ed1_summary)




#profession summary
po_cross_tab <- table(anc_data_processed_subset$profession_summary, anc_data_processed_subset$anaemia_status)
print(po_cross_tab)
po_chi_square_test <- chisq.test(po_cross_tab)
print(po_chi_square_test)
po_percentages <- prop.table(po_cross_tab, margin = 2) * 100
po_summary <- as.data.frame.matrix(po_cross_tab)
po_summary$Profession_Summary <- rownames(po_summary)
rownames(po_summary) <- NULL
for (col in colnames(po_cross_tab)) {
  po_summary[[col]] <- paste0(po_summary[[col]], " (", round(po_percentages[, col], 2), "%)")
}
po_summary <- po_summary %>%
  select(Profession_Summary, everything())
print(po_summary)


#profession group
pog_cross_tab <- table(anc_data_processed_subset$profession_group, anc_data_processed_subset$anaemia_status)
print(pog_cross_tab)
pog_chi_square_test <- chisq.test(pog_cross_tab)
print(pog_chi_square_test)
pog_percentages <- prop.table(pog_cross_tab, margin = 2) * 100
pog_summary <- as.data.frame.matrix(pog_cross_tab)
pog_summary$Profession_Group <- rownames(pog_summary)
rownames(pog_summary) <- NULL
for (col in colnames(pog_cross_tab)) {
  pog_summary[[col]] <- paste0(pog_summary[[col]], " (", round(pog_percentages[, col], 2), "%)")
}
pog_summary <- pog_summary %>%
  select(Profession_Group, everything())
print(pog_summary)


#location town
lo_cross_tab <- table(anc_data_processed_subset$location_group, anc_data_processed_subset$anaemia_status)
print(lo_cross_tab)
lo_chi_square_test <- chisq.test(lo_cross_tab)
print(lo_chi_square_test)
lo_percentages <- prop.table(lo_cross_tab, margin = 2) * 100
lo_summary <- as.data.frame.matrix(lo_cross_tab)
lo_summary$Location_Group <- rownames(lo_summary)
rownames(lo_summary) <- NULL
for (col in colnames(lo_cross_tab)) {
  lo_summary[[col]] <- paste0(lo_summary[[col]], " (", round(lo_percentages[, col], 2), "%)")
}
lo_summary <- lo_summary %>%
  select(Location_Group, everything())
print(lo_summary)


#location distance
ad_cross_tab <- table(anc_data_processed_subset$address_group, anc_data_processed_subset$anaemia_status)
print(ad_cross_tab)
ad_chi_square_test <- chisq.test(ad_cross_tab)
print(ad_chi_square_test)
ad_percentages <- prop.table(ad_cross_tab, margin = 2) * 100
ad_summary <- as.data.frame.matrix(ad_cross_tab)
ad_summary$Address_Group <- rownames(ad_summary)
rownames(ad_summary) <- NULL
for (col in colnames(ad_cross_tab)) {
  ad_summary[[col]] <- paste0(ad_summary[[col]], " (", round(ad_percentages[, col], 2), "%)")
}
ad_summary <- ad_summary %>%
  select(Address_Group, everything())
print(ad_summary)


#marital summary
ma_cross_tab <- table(anc_data_processed_subset$marital_status, anc_data_processed_subset$anaemia_status)
print(ma_cross_tab)
ma_chi_square_test <- chisq.test(ma_cross_tab)
print(ma_chi_square_test)
ma_percentages <- prop.table(ma_cross_tab, margin = 2) * 100
ma_summary <- as.data.frame.matrix(ma_cross_tab)
ma_summary$Marital_Status <- rownames(ma_summary)
rownames(ma_summary) <- NULL
for (col in colnames(ma_cross_tab)) {
  ma_summary[[col]] <- paste0(ma_summary[[col]], " (", round(ma_percentages[, col], 2), "%)")
}
ma_summary <- ma_summary %>%
  select(Marital_Status, everything())
print(ma_summary)
