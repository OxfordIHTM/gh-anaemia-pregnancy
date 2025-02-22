####data cleaning####
anc_data_processed07 <- read_csv("data/anc_data_processed0728.csv")
#all data
anc_data_processed_subset <- anc_data_processed0728 %>%
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
    location_group2= case_when(
      address_group %in% c( "<5km") ~ "nearby",
      address_group %in% c("5-10km",">10km") ~ "long dostance",
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

####make table for different data####
# Group by address_group and summarize the names of addresses
address_group_summary <- anc_data_processed_subset %>%
  group_by(address_group) %>%
  summarize(addresses = paste(unique(address), collapse = ", "))


# Group by profession_group and summarize the names of profession
profession_group_summary <- anc_data_processed_subset %>%
  group_by(profession_group) %>%
  summarize(addresses = paste(unique(profession), collapse = ", "))


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





####univariable grouping data####-----------------------------------------------------------------------------------------
#anaemia catergory
an_c_counts <- table(anc_data_processed_subset$anaemia_category)
print(an_c_counts)
an_c_percentages <- prop.table(an_c_counts) * 100
an_c_s <- data.frame(
  Anaemia_catergory = names(an_c_counts),
  Count_and_Percentage = paste(an_c_counts, "(", round(an_c_percentages, 2), "%)", sep = "")
)

# age group
age_group_counts <- table(anc_data_processed_subset$age_group)
print(age_group_counts)
age_group_percentages <- prop.table(age_group_counts) * 100
age_group_s <- data.frame(
  Age_group_s = names(age_group_counts),
  Count_and_Percentage = paste(age_group_counts, "(", round(age_group_percentages, 2), "%)", sep = "")
)

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



####Mean, medain,sd.95%CI####
#total data
# Function to calculate 95% confidence interval
calculate_95ci <- function(mean, sd, n) {
  error <- qnorm(0.975) * sd / sqrt(n)
  lower_bound <- mean - error
  upper_bound <- mean + error
  return(c(lower_bound, upper_bound))
}
overall_stats <- anc_data_processed_subset %>%
  summarize(
    median_age = median(age, na.rm = TRUE),
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    youngest_age = min(age, na.rm = TRUE),
    oldest_age = max(age, na.rm = TRUE),
    n = sum(!is.na(age))  
  ) %>%
  rowwise() %>%
  mutate(
    ci = list(calculate_95ci(mean_age, sd_age, n)),
    ci_lower = ci[1],
    ci_upper = ci[2]
  ) %>%
  ungroup() %>%
  select(-ci)  
print(overall_stats)



##Total Hb
Hb_summary <- anc_data_processed_subset %>%
  summarize(
    median_hb=median(haemoglobin,na.rm=TRUE),
    mean_hb = mean(haemoglobin, na.rm = TRUE),
    sd_hb=sd(haemoglobin,na.rm=TRUE),
    min_hb = min(haemoglobin, na.rm = TRUE),
    max_hb = max(haemoglobin, na.rm = TRUE)
  )

# Print the summary
print(Hb_summary)


##Anaemia part of age 
#in anaemia STATUS
AN_age_summary <- anc_data_processed_subset %>%
  group_by(anaemia_status) %>%
  summarize(
    median_age = median(age, na.rm = TRUE),
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    youngest_age = min(age, na.rm = TRUE),
    oldest_age = max(age, na.rm = TRUE),
    n = sum(!is.na(age))  
  ) %>%
  rowwise() %>%
  mutate(
    ci = list(calculate_95ci(mean_age, sd_age, n)),
    ci_lower = ci[1],
    ci_upper = ci[2]
  ) %>%
  ungroup() %>%
  select(-ci)  

# Print the summary
print(AN_age_summary)

#in anaemia caterogy
age_summary <- anc_data_processed_subset %>%
  group_by(anaemia_category) %>%
  summarize(
    median_age=median(age,na.rm=TRUE),
    mean_age = mean(age, na.rm = TRUE),
    sd_age=sd(age,na.rm=TRUE),
    youngest_age = min(age, na.rm = TRUE),
    oldest_age = max(age, na.rm = TRUE)
  )

# Print the summary
print(age_summary)













####no grouping bivariate analysis####--------------------------------------------------------------------------------------------
#education 
ed_l_cross_tab <- table(anc_data_processed_subset$education_level, anc_data_processed_subset$anaemia_status)
print(ed_l_cross_tab)
# Calculate percentages
ed_l_percentages <- prop.table(ed_l_cross_tab, margin = 2) * 100
ed_l_summary <- as.data.frame.matrix(ed_l_cross_tab)
ed_l_summary$Education_Level <- rownames(ed_l_summary)
rownames(ed_l_summary) <- NULL
for (col in colnames(ed_l_cross_tab)) {
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
#### age and haemoglobin relationship####
# Correlation analysis
correlation <- cor(anc_data_processed_subset$age, anc_data_processed_subset$haemoglobin, use = "complete.obs")
print(paste("Correlation between age and haemoglobin:", correlation))

# Linear regression analysis
lm_model <- lm(haemoglobin ~ age, data = anc_data_processed_subset)
lm_summary <- summary(lm_model)
print(lm_summary)

# Plotting the relationship
ggplot(anc_data_processed_subset, aes(x = age, y = haemoglobin)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Relationship between Age and Hemoglobin Levels",
       x = "Age",
       y = "Hemoglobin Level") +
  theme_minimal()

# Display the regression results in a tidy format
lm_tidy <- tidy(lm_model)
print(lm_tidy)

####age group and histogram####

# Filter, count, and plot
anc_data_processed_subset %>%
  dplyr::filter(!is.na(anaemia_status)) %>%
  dplyr::count(anaemia_status, age_group) %>%
  ggplot(mapping = aes(x = age_group, y = n)) +
  geom_col(fill = oxthema::get_oxford_colours("sky"), alpha = 0.8) +
  scale_y_continuous(
    breaks = seq(from = 0, to = 40, by = 10)
  ) +
  labs(
    title = "Age Distribution",
    subtitle = "Age group of women attending ANC in 2023",
    x = "Age group",
    y = "Number of women"
  ) +
  coord_flip() +
  facet_wrap(. ~ anaemia_status, nrow = 2) +
  oxthema::theme_oxford(grid = "Xx")

####grouping bivariate analysis####------------------------------------------------------------------------------------
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




# Education summary
ed_cross_tab <- table(anc_data_processed_subset$education_level_summary, anc_data_processed_subset$anaemia_status)
print(ed_cross_tab)
ed_chi_square_test <- chisq.test(ed_cross_tab)
print(ed_chi_square_test)
p_value <- ed_chi_square_test$p.value
effect_size <- sqrt(ed_chi_square_test$statistic / sum(ed_cross_tab))
power_result <- pwr.chisq.test(w = effect_size, N = sum(ed_cross_tab), df = ed_chi_square_test$parameter, sig.level = 0.05)
power <- power_result$power
ed_percentages <- prop.table(ed_cross_tab, margin = 2) * 100
ed_summary <- as.data.frame.matrix(ed_cross_tab)
ed_summary$Education_Level <- rownames(ed_summary)
rownames(ed_summary) <- NULL
for (col in colnames(ed_cross_tab)) {
  ed_summary[[col]] <- paste0(ed_summary[[col]], " (", round(ed_percentages[, col], 2), "%)")
}

ed_summary <- ed_summary %>%
  select(Education_Level, everything())
ed_summary <- ed_summary %>%
  mutate(p_value = p_value, power = power)

print(ed_summary)



# Education level summary1 (no and primary together)
ed1_cross_tab <- table(anc_data_processed_subset$education_level_summary1, anc_data_processed_subset$anaemia_status)
print(ed1_cross_tab)

# Perform the chi-square test
ed1_chi_square_test <- chisq.test(ed1_cross_tab)
print(ed1_chi_square_test)

# Extract chi-square statistic and p-value
chi_square_stat <- ed1_chi_square_test$statistic
p_value <- ed1_chi_square_test$p.value

# Calculate power
effect_size <- sqrt(chi_square_stat / sum(ed1_cross_tab))
power_result <- pwr.chisq.test(w = effect_size, N = sum(ed1_cross_tab), df = ed1_chi_square_test$parameter, sig.level = 0.05)
power <- power_result$power

# Calculate percentages
ed1_percentages <- prop.table(ed1_cross_tab, margin = 2) * 100
ed1_summary <- as.data.frame.matrix(ed1_cross_tab)
ed1_summary$Education_Level <- rownames(ed1_summary)
rownames(ed1_summary) <- NULL
for (col in colnames(ed1_cross_tab)) {
  ed1_summary[[col]] <- paste0(ed1_summary[[col]], " (", round(ed1_percentages[, col], 2), "%)")
}

ed1_summary <- ed1_summary %>%
  select(Education_Level, everything())
ed1_summary <- ed1_summary %>%
  mutate(chi_square_stat = chi_square_stat, p_value = p_value, power = power)
print(ed1_summary)



# Helper function to calculate and add chi-square statistic, p-value, and power
add_test_results <- function(cross_tab) {
  chi_square_test <- chisq.test(cross_tab)
  chi_square_stat <- chi_square_test$statistic
  p_value <- chi_square_test$p.value
  effect_size <- sqrt(chi_square_stat / sum(cross_tab))
  power_result <- pwr.chisq.test(w = effect_size, N = sum(cross_tab), df = chi_square_test$parameter, sig.level = 0.05)
  power <- power_result$power
  return(list(chi_square_stat = chi_square_stat, p_value = p_value, power = power))
}

# Education Level Summary 1
ed1_cross_tab <- table(anc_data_processed_subset$education_level_summary1, anc_data_processed_subset$anaemia_status)
ed1_test_results <- add_test_results(ed1_cross_tab)
ed1_percentages <- prop.table(ed1_cross_tab, margin = 2) * 100
ed1_summary <- as.data.frame.matrix(ed1_cross_tab)
ed1_summary$Education_Level <- rownames(ed1_summary)
rownames(ed1_summary) <- NULL
for (col in colnames(ed1_cross_tab)) {
  ed1_summary[[col]] <- paste0(ed1_summary[[col]], " (", round(ed1_percentages[, col], 2), "%)")
}
ed1_summary <- ed1_summary %>%
  select(Education_Level, everything()) %>%
  mutate(chi_square_stat = ed1_test_results$chi_square_stat, p_value = ed1_test_results$p_value, power = ed1_test_results$power)
print(ed1_summary)

# Profession Summary
po_cross_tab <- table(anc_data_processed_subset$profession_summary, anc_data_processed_subset$anaemia_status)
po_test_results <- add_test_results(po_cross_tab)
po_percentages <- prop.table(po_cross_tab, margin = 2) * 100
po_summary <- as.data.frame.matrix(po_cross_tab)
po_summary$Profession_Summary <- rownames(po_summary)
rownames(po_summary) <- NULL
for (col in colnames(po_cross_tab)) {
  po_summary[[col]] <- paste0(po_summary[[col]], " (", round(po_percentages[, col], 2), "%)")
}
po_summary <- po_summary %>%
  select(Profession_Summary, everything()) %>%
  mutate(chi_square_stat = po_test_results$chi_square_stat, p_value = po_test_results$p_value, power = po_test_results$power)
print(po_summary)

# Profession Summary 1
pos1_cross_tab <- table(anc_data_processed_subset$profession_summary1, anc_data_processed_subset$anaemia_status)
pos1_test_results <- add_test_results(pos1_cross_tab)
pos1_percentages <- prop.table(pos1_cross_tab, margin = 2) * 100
pos1_summary <- as.data.frame.matrix(pos1_cross_tab)
pos1_summary$Profession_Summary1 <- rownames(pos1_summary)
rownames(pos1_summary) <- NULL
for (col in colnames(pos1_cross_tab)) {
  pos1_summary[[col]] <- paste0(pos1_summary[[col]], " (", round(pos1_percentages[, col], 2), "%)")
}
pos1_summary <- pos1_summary %>%
  select(Profession_Summary1, everything()) %>%
  mutate(chi_square_stat = pos1_test_results$chi_square_stat, p_value = pos1_test_results$p_value, power = pos1_test_results$power)
print(pos1_summary)

# Profession Group
pog_cross_tab <- table(anc_data_processed_subset$profession_group, anc_data_processed_subset$anaemia_status)
pog_test_results <- add_test_results(pog_cross_tab)
pog_percentages <- prop.table(pog_cross_tab, margin = 2) * 100
pog_summary <- as.data.frame.matrix(pog_cross_tab)
pog_summary$Profession_Group <- rownames(pog_summary)
rownames(pog_summary) <- NULL
for (col in colnames(pog_cross_tab)) {
  pog_summary[[col]] <- paste0(pog_summary[[col]], " (", round(pog_percentages[, col], 2), "%)")
}
pog_summary <- pog_summary %>%
  select(Profession_Group, everything()) %>%
  mutate(chi_square_stat = pog_test_results$chi_square_stat, p_value = pog_test_results$p_value, power = pog_test_results$power)
print(pog_summary)

# Location Town
lo_cross_tab <- table(anc_data_processed_subset$address_group, anc_data_processed_subset$anaemia_status)
lo_test_results <- add_test_results(lo_cross_tab)
lo_percentages <- prop.table(lo_cross_tab, margin = 2) * 100
lo_summary <- as.data.frame.matrix(lo_cross_tab)
lo_summary$Location_Group <- rownames(lo_summary)
rownames(lo_summary) <- NULL
for (col in colnames(lo_cross_tab)) {
  lo_summary[[col]] <- paste0(lo_summary[[col]], " (", round(lo_percentages[, col], 2), "%)")
}
lo_summary <- lo_summary %>%
  select(Location_Group, everything()) %>%
  mutate(chi_square_stat = lo_test_results$chi_square_stat, p_value = lo_test_results$p_value, power = lo_test_results$power)
print(lo_summary)


# Location Distance(5,10)
ad_cross_tab <- table(anc_data_processed_subset$address_group, anc_data_processed_subset$anaemia_status)
ad_test_results <- add_test_results(ad_cross_tab)
ad_percentages <- prop.table(ad_cross_tab, margin = 2) * 100
ad_summary <- as.data.frame.matrix(ad_cross_tab)
ad_summary$Address_Group <- rownames(ad_summary)
rownames(ad_summary) <- NULL
for (col in colnames(ad_cross_tab)) {
  ad_summary[[col]] <- paste0(ad_summary[[col]], " (", round(ad_percentages[, col], 2), "%)")
}
ad_summary <- ad_summary %>%
  select(Address_Group, everything()) %>%
  mutate(chi_square_stat = ad_test_results$chi_square_stat, p_value = ad_test_results$p_value, power = ad_test_results$power)
print(ad_summary)

# Location Distance(1,47)
ad3_cross_tab <- table(anc_data_processed_subset$address_group3, anc_data_processed_subset$anaemia_status)
ad3_test_results <- add_test_results(ad3_cross_tab)
ad3_percentages <- prop.table(ad3_cross_tab, margin = 2) * 100
ad3_summary <- as.data.frame.matrix(ad3_cross_tab)
ad3_summary$Address_Group3 <- rownames(ad3_summary)
rownames(ad3_summary) <- NULL
for (col in colnames(ad_cross_tab)) {
  ad3_summary[[col]] <- paste0(ad3_summary[[col]], " (", round(ad3_percentages[, col], 2), "%)")
}
ad3_summary <- ad3_summary %>%
  select(Address_Group3, everything()) %>%
  mutate(chi_square_stat = ad3_test_results$chi_square_stat, p_value = ad3_test_results$p_value, power = ad3_test_results$power)
print(ad3_summary)


# Marital Summary
ma_cross_tab <- table(anc_data_processed_subset$marital_status, anc_data_processed_subset$anaemia_status)
ma_test_results <- add_test_results(ma_cross_tab)
ma_percentages <- prop.table(ma_cross_tab, margin = 2) * 100
ma_summary <- as.data.frame.matrix(ma_cross_tab)
ma_summary$Marital_Status <- rownames(ma_summary)
rownames(ma_summary) <- NULL
for (col in colnames(ma_cross_tab)) {
  ma_summary[[col]] <- paste0(ma_summary[[col]], " (", round(ma_percentages[, col], 2), "%)")
}
ma_summary <- ma_summary %>%
  select(Marital_Status, everything()) %>%
  mutate(chi_square_stat = ma_test_results$chi_square_stat, p_value = ma_test_results$p_value, power = ma_test_results$power)
print(ma_summary)
####Boxplot####
#Education
anc_data_processed_subset$education_level_summary <- factor(anc_data_processed_subset$education_level_summary,
                                                         levels = c("Primary/Junior High School", "Senior High School and higher", "None", "No Data"),
                                                         labels = c("Primary/Junior High School", "Senior High School and higher","No Education", "No Data"))



# Create the plot with modified labels for education_level
ggplot(anc_data_processed_subset, aes(x = education_level_summary, y = haemoglobin, fill = education_level_summary)) +
  geom_boxplot() +
  labs(title = "Boxplot of Haemoglobin Levels by Education Level",
       x = "Education Level",
       y = "Haemoglobin Level (g/dL)",
       fill = "Education Level") +  # Change legend title
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")


# Create the plot with modified labels for employment
ggplot(anc_data_processed_subset, aes(x = profession_group, y = haemoglobin, fill = profession_group)) +
  geom_boxplot() +
  labs(title = "Boxplot of Haemoglobin Levels by Employment",
       x = "Employment",
       y = "Haemoglobin Level (g/dL)",
       fill = "Empoyment") +  # Change legend title
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")



# Create the plot with modified labels for location_group(town)
ggplot(anc_data_processed_subset, aes(x = location_group, y = haemoglobin, fill = location_group)) +
  geom_boxplot() +
  labs(title = "Boxplot of Haemoglobin Levels by Location",
       x = "Location",
       y = "Haemoglobin Level (g/dL)",
       fill = "Location") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")


# Create the plot with modified labels for distance
ggplot(anc_data_processed_subset, aes(x = address_group, y = haemoglobin, fill = address_group)) +
  geom_boxplot() +
  labs(title = "Boxplot of Haemoglobin Levels by Distance",
       x = "Location",
       y = "Haemoglobin Level (g/dL)",
       fill = "Location") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")


#marital status and Hb
ggplot(anc_data_processed_subset, aes(x = marital_status, y = haemoglobin, fill = marital_status)) +
  geom_boxplot() +
  labs(title = "Boxplot of Haemoglobin Levels by Marital status",
       x = "Marital Status",
       y = "Haemoglobin Level (g/dL)",
       fill= "Marital Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")


####other relationship####---------------------------------------------------------------------------------------
# Education level and profession
# Ensure no missing values before analysis
anc_data_processed_subset <- anc_data_processed_subset %>%
  drop_na(education_level_summary1,location_group)%>%
  drop_na(profession_group, location_group)%>%
  drop_na(profession_group, address_group)

# Education level and profession

anc_data_processed_subset <- anc_data_processed_subset %>%
  drop_na(education_level_summary, profession_group)
  
ed_p_cross_tab <- table(anc_data_processed_subset$education_level_summary, anc_data_processed_subset$profession_group)
ed_p_test_results <- add_test_results(ed_p_cross_tab)
ed_p_percentages <- prop.table(ed_p_cross_tab, margin = 2) * 100
ed_p_summary <- as.data.frame.matrix(ed_p_cross_tab)
ed_p_summary$education_level_summary <- rownames(ed_p_summary)
rownames(ed_p_summary) <- NULL
for (col in colnames(ed_p_cross_tab)) {
  ed_p_summary[[col]] <- paste0(ed_p_summary[[col]], " (", round(ed_p_percentages[, col], 2), "%)")
}
ed_p_summary <- ed_p_summary %>%
  select(education_level_summary, everything()) %>%
  mutate(chi_square_stat = ed_p_test_results$chi_square_stat, p_value = ed_p_test_results$p_value, power = ed_p_test_results$power)

print(ed_p_summary)



#education level and location(town)
anc_data_processed_subset <- anc_data_processed_subset %>%
  drop_na(education_level_summary1,location_group)
# Education level and location group
ed_lg_cross_tab <- table(anc_data_processed_subset$education_level_summary1, anc_data_processed_subset$location_group)
ed_lg_test_results <- add_test_results(ed_lg_cross_tab)
ed_lg_percentages <- prop.table(ed_lg_cross_tab, margin = 2) * 100
ed_lg_summary <- as.data.frame.matrix(ed_lg_cross_tab)
ed_lg_summary$education_level_summary1 <- rownames(ed_lg_summary)
rownames(ed_lg_summary) <- NULL

# Combine counts and percentages into a single column
for (col in colnames(ed_lg_cross_tab)) {
  ed_lg_summary[[col]] <- paste0(ed_lg_summary[[col]], " (", round(ed_lg_percentages[, col], 2), "%)")
}

# Add chi-square statistic, p-value, and power to the summary
ed_lg_summary <- ed_lg_summary %>%
  select(education_level_summary1, everything()) %>%
  mutate(chi_square_stat = ed_lg_test_results$chi_square_stat, p_value = ed_lg_test_results$p_value, power = ed_lg_test_results$power)

# Print the summary table
print(ed_lg_summary)



# Profession group and location group
pg_lg_cross_tab <- table(anc_data_processed_subset$profession_group, anc_data_processed_subset$location_group)
pg_lg_test_results <- add_test_results(pg_lg_cross_tab)
pg_lg_percentages <- prop.table(pg_lg_cross_tab, margin = 2) * 100
pg_lg_summary <- as.data.frame.matrix(pg_lg_cross_tab)
pg_lg_summary$profession_group <- rownames(pg_lg_summary)
rownames(pg_lg_summary) <- NULL
for (col in colnames(pg_lg_cross_tab)) {
  pg_lg_summary[[col]] <- paste0(pg_lg_summary[[col]], " (", round(pg_lg_percentages[, col], 2), "%)")
}

pg_lg_summary <- pg_lg_summary %>%
  select(profession_group, everything()) %>%
  mutate(chi_square_stat = pg_lg_test_results$chi_square_stat, p_value = pg_lg_test_results$p_value, power = pg_lg_test_results$power)
print(pg_lg_summary)


# Profession group and address group
pg_ag_cross_tab <- table(anc_data_processed_subset$profession_group, anc_data_processed_subset$address_group)
pg_ag_test_results <- add_test_results(pg_ag_cross_tab)
pg_ag_percentages <- prop.table(pg_ag_cross_tab, margin = 2) * 100
pg_ag_summary <- as.data.frame.matrix(pg_ag_cross_tab)
pg_ag_summary$profession_group <- rownames(pg_ag_summary)
rownames(pg_ag_summary) <- NULL

# Combine counts and percentages into a single column
for (col in colnames(pg_ag_cross_tab)) {
  pg_ag_summary[[col]] <- paste0(pg_ag_summary[[col]], " (", round(pg_ag_percentages[, col], 2), "%)")
}

# Add chi-square statistic, p-value, and power to the summary
pg_ag_summary <- pg_ag_summary %>%
  select(profession_group, everything()) %>%
  mutate(chi_square_stat = pg_ag_test_results$chi_square_stat, p_value = pg_ag_test_results$p_value, power = pg_ag_test_results$power)

# Print the summary table
print(pg_ag_summary)

# Education level summary and address group
el_ag_cross_tab <- table(anc_data_processed_subset$education_level_summary, anc_data_processed_subset$address_group)
el_ag_test_results <- add_test_results(el_ag_cross_tab)
el_ag_percentages <- prop.table(el_ag_cross_tab, margin = 2) * 100
el_ag_summary <- as.data.frame.matrix(el_ag_cross_tab)
el_ag_summary$education_level_summary <- rownames(el_ag_summary)
rownames(el_ag_summary) <- NULL

# Combine counts and percentages into a single column
for (col in colnames(el_ag_cross_tab)) {
  el_ag_summary[[col]] <- paste0(el_ag_summary[[col]], " (", round(el_ag_percentages[, col], 2), "%)")
}

# Add chi-square statistic, p-value, and power to the summary
el_ag_summary <- el_ag_summary %>%
  select(education_level_summary, everything()) %>%
  mutate(chi_square_stat = el_ag_test_results$chi_square_stat, p_value = el_ag_test_results$p_value, power = el_ag_test_results$power)

# Print the summary table
print(el_ag_summary)



####Hb and other variable####
####Mean 95%CI by Hb####
#education
meanci_by_ed <- anc_data_processed_subset %>%
  group_by(education_level_summary) %>%
  summarise(
    mean_hemoglobin = mean(haemoglobin, na.rm = TRUE),
    sd_hemoglobin = sd(haemoglobin, na.rm = TRUE),
    n = n(),
    lower_ci = mean(haemoglobin, na.rm = TRUE) - qt(0.975, df = n() - 1) * (sd(haemoglobin, na.rm = TRUE) / sqrt(n())),
    upper_ci = mean(haemoglobin, na.rm = TRUE) + qt(0.975, df = n() - 1) * (sd(haemoglobin, na.rm = TRUE) / sqrt(n()))
  )
colnames(meanci_by_ed)[1] <- "variable"

print(colnames(meanci_by_ed))
# Print the results
print(meanci_by_ed)

#profession
meanci_by_profession <- anc_data_processed_subset %>%
  group_by(profession_summary1) %>%
  summarise(
    mean_hemoglobin = mean(haemoglobin, na.rm = TRUE),
    sd_hemoglobin = sd(haemoglobin, na.rm = TRUE),
    n = n(),
    lower_ci = mean(haemoglobin, na.rm = TRUE) - qt(0.975, df = n() - 1) * (sd(haemoglobin, na.rm = TRUE) / sqrt(n())),
    upper_ci = mean(haemoglobin, na.rm = TRUE) + qt(0.975, df = n() - 1) * (sd(haemoglobin, na.rm = TRUE) / sqrt(n()))
  )


colnames(meanci_by_profession)[1] <- "variable"

print(colnames(meanci_by_profession))
# Print the results
print(meanci_by_profession)

#location
meanci_by_lo <- anc_data_processed_subset %>%
  group_by(location_group) %>%
  summarise(
    mean_hemoglobin = mean(haemoglobin, na.rm = TRUE),
    sd_hemoglobin = sd(haemoglobin, na.rm = TRUE),
    n = n(),
    lower_ci = mean(haemoglobin, na.rm = TRUE) - qt(0.975, df = n() - 1) * (sd(haemoglobin, na.rm = TRUE) / sqrt(n())),
    upper_ci = mean(haemoglobin, na.rm = TRUE) + qt(0.975, df = n() - 1) * (sd(haemoglobin, na.rm = TRUE) / sqrt(n()))
  )
colnames(meanci_by_lo)[1] <- "variable"

print(colnames(meanci_by_lo))
# Print the results
print(meanci_by_lo)


#marital 
meanci_by_mar <- anc_data_processed_subset %>%
  group_by(marital_status) %>%
  summarise(
    mean_hemoglobin = mean(haemoglobin, na.rm = TRUE),
    sd_hemoglobin = sd(haemoglobin, na.rm = TRUE),
    n = n(),
    lower_ci = mean(haemoglobin, na.rm = TRUE) - qt(0.975, df = n() - 1) * (sd(haemoglobin, na.rm = TRUE) / sqrt(n())),
    upper_ci = mean(haemoglobin, na.rm = TRUE) + qt(0.975, df = n() - 1) * (sd(haemoglobin, na.rm = TRUE) / sqrt(n()))
  )
colnames(meanci_by_mar)[1] <- "variable"

print(colnames(meanci_by_mar))
# Print the results
print(meanci_by_mar)


#sickle cell
meanci_by_si <- anc_data_processed_subset %>%
  group_by(sickle_cell) %>%
  summarise(
    mean_hemoglobin = mean(haemoglobin, na.rm = TRUE),
    sd_hemoglobin = sd(haemoglobin, na.rm = TRUE),
    n = n(),
    lower_ci = mean(haemoglobin, na.rm = TRUE) - qt(0.975, df = n() - 1) * (sd(haemoglobin, na.rm = TRUE) / sqrt(n())),
    upper_ci = mean(haemoglobin, na.rm = TRUE) + qt(0.975, df = n() - 1) * (sd(haemoglobin, na.rm = TRUE) / sqrt(n()))
  )
colnames(meanci_by_si)[1] <- "variable"

print(colnames(meanci_by_si))
# Print the results
print(meanci_by_si)


#marital 
meanci_by_mal <- anc_data_processed_subset %>%
  group_by(malaria) %>%
  summarise(
    mean_hemoglobin = mean(haemoglobin, na.rm = TRUE),
    sd_hemoglobin = sd(haemoglobin, na.rm = TRUE),
    n = n(),
    lower_ci = mean(haemoglobin, na.rm = TRUE) - qt(0.975, df = n() - 1) * (sd(haemoglobin, na.rm = TRUE) / sqrt(n())),
    upper_ci = mean(haemoglobin, na.rm = TRUE) + qt(0.975, df = n() - 1) * (sd(haemoglobin, na.rm = TRUE) / sqrt(n()))
  )

colnames(meanci_by_mal)[1] <- "variable"

print(colnames(meanci_by_mal))
# Print the results
print(meanci_by_mal)