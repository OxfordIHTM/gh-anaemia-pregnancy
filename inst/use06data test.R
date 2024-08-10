####data cleaning####
anc_data_processed06 <- read_csv("data/anc_data_processed06.csv")
#all data
anc_data_processed06<- anc_data_processed06 %>%
  dplyr::mutate(
    total_n = n(),
    early_childbearing= cut(
      x = age,
      breaks = c( 15, 20, Inf),
      labels = c(
        "15 to 19 years","20 years and older"
      ),
      include.lowest = TRUE, right = FALSE
    ),
    education_level_summary1= case_when(
      education_level %in% c( "None", "Primary","Junior High School") ~ "No education and Basic education",
      education_level %in% c( "Senior High School", "Tertiary") ~ "Senior high school and higher",
      TRUE ~ "NA"
    ),
    education_level_summary2= case_when(
      education_level %in% c( "None") ~ "No education ",
      education_level%in%c( "Primary","Junior High School")~"Basic education",
      education_level %in% c( "Senior High School", "Tertiary") ~ "Senior high school and higher",
      TRUE ~ "NA"
    ),
    profession_group= case_when(
      profession %in% c("None","Student" ) ~ "Unemployed",
      profession %in% c("Teacher", "Company Employee", "Midwife","Undertaker", "Trader", "Seamstress", "Hair Dresser", "Fishmonger", "Farmer", "Decorator", "Caterer", "Business Owner") ~ "Employed",
      TRUE ~ "NA"
    ),
    location_group2= case_when(
      address%in% c( "Abandze", "Aketekyiwa",
                            "Buranamoah", "Eguase", "Kormantse", "Moree", "Pomasi (Pomase)", "Waakrom", "Yamoransa"," Afrago Junction", "Amissakrom", "Amoanda", "Cape Coast", "Egyirefa", "Ekon", "Ekotokrom",
                     "Insanfo (Nsanfo)", "Makassium", "Saltpond Zongo") ~ ">5km",
      address %in% c("Biriwa", "Anomabo", "Asafora") ~ "<5km",
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


#####univariable analysis####
#early childbearing
ec_counts <- table(anc_data_processed06$early_childbearing)
print(ec_counts)
ec_percentages <- prop.table(ec_counts) * 100
ec_s <- data.frame(
  Early_childbearing = names(ec_counts),
  Count_and_Percentage = paste(ec_counts, "(", round(ec_percentages, 2), "%)", sep = "")
)
print(ec_s)
#anaemia status
an_counts <- table(anc_data_processed06$anaemia_status)
print(an_counts)
an_percentages <- prop.table(an_counts) * 100
an_s <- data.frame(
  Anaemia_status = names(an_counts),
  Count_and_Percentage = paste(an_counts, "(", round(an_percentages, 2), "%)", sep = "")
)
print(an_s)
#anaemia catergory
an_c_counts <- table(anc_data_processed06$anaemia_category)
print(an_c_counts)
an_c_percentages <- prop.table(an_c_counts) * 100
an_c_s <- data.frame(
  Anaemia_catergory = names(an_c_counts),
  Count_and_Percentage = paste(an_c_counts, "(", round(an_c_percentages, 2), "%)", sep = "")
)
print(an_c_s)

# age group
age_group_counts <- table(anc_data_processed06$age_group)
print(age_group_counts)
age_group_percentages <- prop.table(age_group_counts) * 100
age_group <- data.frame(
  Age_Group = names(age_group_counts),
  Count_and_Percentage = paste(age_group_counts, "(", round(age_group_percentages, 2), "%)", sep = "")
)
print(age_group)
# education levels
education_counts <- table(anc_data_processed06$education_level)
print(education_counts)
education_percentages <- prop.table(education_counts) * 100
education <- data.frame(
  Education_Level = names(education_counts),
  Count_and_Percentage = paste(education_counts, "(", round(education_percentages, 2), "%)", sep = "")
)


# profession
profession_counts <- table(anc_data_processed06$profession)
print(profession_counts)
profession_percentages <- prop.table(profession_counts) * 100
Profession <- data.frame(
  Profession_Level = names(profession_counts),
  Count_and_Percentage = paste(profession_counts, "(", round(profession_percentages, 2), "%)", sep = "")
)
#address
address_counts <- table(anc_data_processed06$address)
print(address_counts)
address_percentages <- prop.table(address_counts) * 100
Address <- data.frame(
  Address_Level = names(address_counts),
  Count_and_Percentage = paste(address_counts, "(", round(address_percentages, 2), "%)", sep = "")
)
print(Address)

#marital status
marital_status_counts <- table(anc_data_processed06$marital_status)
print(marital_status_counts)
marital_status_percentages <- prop.table(marital_status_counts) * 100
Marital_Status <- data.frame(
  Marital_Status_Level = names(marital_status_counts),
  Count_and_Percentage = paste(marital_status_counts, "(", round(marital_status_percentages, 2), "%)", sep = "")
)
print(Marital_Status)


#sickle cell
sickle_cell_counts <- table(anc_data_processed06$sickle_cell)
print(sickle_cell_counts)
sickle_cell_percentages <- prop.table(sickle_cell_counts) * 100
Sickle_Cell <- data.frame(
  Sickle_Cell_Level = names(sickle_cell_counts),
  Count_and_Percentage = paste(sickle_cell_counts, "(", round(sickle_cell_percentages, 2), "%)", sep = "")
)
print(Sickle_Cell)




####Mean,median of age and Hb####
calculate_95ci <- function(mean, sd, n) {
  error <- qnorm(0.975) * sd / sqrt(n)
  lower_bound <- mean - error
  upper_bound <- mean + error
  return(c(lower_bound, upper_bound))
}
overall_stats <- anc_data_processed06 %>%
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
Hb_summary <- anc_data_processed06 %>%
  summarize(
    median_hb=median(haemoglobin,na.rm=TRUE),
    mean_hb = mean(haemoglobin, na.rm = TRUE),
    sd_hb=sd(haemoglobin,na.rm=TRUE),
    min_hb = min(haemoglobin, na.rm = TRUE),
    max_hb = max(haemoglobin, na.rm = TRUE)
  )

# Print the summary
print(Hb_summary)

#### age and haemoglobin relationship####
# Correlation analysis
correlation <- cor(anc_data_processed06$age, anc_data_processed06$haemoglobin, use = "complete.obs")
print(paste("Correlation between age and haemoglobin:", correlation))

# Linear regression analysis
lm_model <- lm(haemoglobin ~ age, data = anc_data_processed06)
lm_summary <- summary(lm_model)
print(lm_summary)

# Plotting the relationship
ggplot(anc_data_processed06, aes(x = age, y = haemoglobin)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Relationship between Age and Haemoglobin Levels",
       x = "Age(year)",
       y = "Haemoglobin Level(g/dL)") +
  theme_minimal()


# Create a contingency table
table_data <- table(anc_data_processed06$sickle_cell, anc_data_processed06$anaemia_status)

# Display the contingency table
print(table_data)

# Calculate the Odds Ratio using epitools
or_result <- oddsratio(table_data)
print(or_result)

# Perform Fisher's Exact Test
fisher_result <- fisher.test(table_data)
print(fisher_result)

####Hb and other variable
# Calculate means, standard deviations, confidence intervals, and p-value
meanci_by_ec <- anc_data_processed06 %>%
  group_by(early_childbearing) %>%
  summarise(
    mean_hemoglobin = mean(haemoglobin, na.rm = TRUE),
    sd_hemoglobin = sd(haemoglobin, na.rm = TRUE),
    n = n(),
    lower_ci = mean(haemoglobin, na.rm = TRUE) - qt(0.975, df = n() - 1) * (sd(haemoglobin, na.rm = TRUE) / sqrt(n())),
    upper_ci = mean(haemoglobin, na.rm = TRUE) + qt(0.975, df = n() - 1) * (sd(haemoglobin, na.rm = TRUE) / sqrt(n()))
  )

# Rename the first column
colnames(meanci_by_ec)[1] <- "variable"

# Print the column names and the summarized data
print(colnames(meanci_by_ec))
print(meanci_by_ec)

# Conduct a t-test to calculate the p-value
t_test_result <- t.test(haemoglobin ~ early_childbearing, data = anc_data_processed06)
p_value <- t_test_result$p.value

# Print the p-value
print(paste("P-value:", p_value))

# Create a box plot for hemoglobin levels by early childbearing status
ggplot(anc_data_processed06, aes(x = early_childbearing, y = haemoglobin, fill = early_childbearing)) +
  geom_boxplot() +
  labs(
    title = "Box Plot of Haemoglobin Levels by Early Childbearing",
    x = "Early Childbearing",
    y = "Hemoglobin Level"
  ) +
  scale_fill_manual(values = c("#FF9999", "#66B2FF")) +  # Customize colors here
  theme_minimal()

# Remove rows with NA values in location_group2 and haemoglobin
anc_data_processed06_clean <- anc_data_processed06 %>%
  filter(!is.na(location_group2) & !is.na(haemoglobin))

# Calculate means, standard deviations, confidence intervals, and p-value
meanci_by_location <- anc_data_processed06_clean %>%
  group_by(location_group2) %>%
  summarise(
    mean_hemoglobin = mean(haemoglobin, na.rm = TRUE),
    sd_hemoglobin = sd(haemoglobin, na.rm = TRUE),
    n = n(),
    lower_ci = mean(haemoglobin, na.rm = TRUE) - qt(0.975, df = n() - 1) * (sd(haemoglobin, na.rm = TRUE) / sqrt(n())),
    upper_ci = mean(haemoglobin, na.rm = TRUE) + qt(0.975, df = n() - 1) * (sd(haemoglobin, na.rm = TRUE) / sqrt(n()))
  )

# Rename the first column
colnames(meanci_by_location)[1] <- "variable"

# Print the column names and the summarized data
print(colnames(meanci_by_location))
print(meanci_by_location)

# Conduct a t-test to calculate the p-value
t_test_result <- t.test(haemoglobin ~ location_group2, data = anc_data_processed06_clean)
p_value <- t_test_result$p.value

# Print the p-value
print(paste("P-value:", p_value))

ggplot(anc_data_processed06_clean, aes(x = location_group2, y = haemoglobin, fill = location_group2)) +
  geom_boxplot() +
  labs(title = "Boxplot of Haemoglobin Levels by Distance",
       x = "Distance",
       y = "Haemoglobin Level (g/dL)",
       fill = "Distance") +  # Change legend title
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")


# Calculate means, standard deviations, confidence intervals, and p-value
meanci_by_education <- anc_data_processed06_clean %>%
  group_by(education_level_summary1) %>%
  summarise(
    mean_hemoglobin = mean(haemoglobin, na.rm = TRUE),
    sd_hemoglobin = sd(haemoglobin, na.rm = TRUE),
    n = n(),
    lower_ci = mean(haemoglobin, na.rm = TRUE) - qt(0.975, df = n() - 1) * (sd(haemoglobin, na.rm = TRUE) / sqrt(n())),
    upper_ci = mean(haemoglobin, na.rm = TRUE) + qt(0.975, df = n() - 1) * (sd(haemoglobin, na.rm = TRUE) / sqrt(n()))
  )

# Rename the first column
colnames(meanci_by_education)[1] <- "variable"

# Print the column names and the summarized data
print(colnames(meanci_by_education))
print(meanci_by_education)

# Conduct a t-test to calculate the p-value
t_test_result <- t.test(haemoglobin ~ education_level_summary1, data = anc_data_processed06_clean)
p_value <- t_test_result$p.value

# Print the p-value
print(paste("P-value:", p_value))

# Create a box plot for haemoglobin levels by education level summary
ggplot(anc_data_processed06_clean, aes(x = education_level_summary1, y = haemoglobin, fill = education_level_summary1)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of Haemoglobin Levels by Education Level Summary",
    x = "Education Level",
    y = "Haemoglobin Level (g/dL)",
    fill = "Education Level Summary"  # Change legend title
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")


# Calculate means, standard deviations, confidence intervals, and p-value
meanci_by_education <- anc_data_processed06 %>%
  group_by(education_level_summary2) %>%
  summarise(
    mean_hemoglobin = mean(haemoglobin, na.rm = TRUE),
    sd_hemoglobin = sd(haemoglobin, na.rm = TRUE),
    n = n(),
    lower_ci = mean(haemoglobin, na.rm = TRUE) - qt(0.975, df = n() - 1) * (sd(haemoglobin, na.rm = TRUE) / sqrt(n())),
    upper_ci = mean(haemoglobin, na.rm = TRUE) + qt(0.975, df = n() - 1) * (sd(haemoglobin, na.rm = TRUE) / sqrt(n()))
  )

# Rename the first column
colnames(meanci_by_education)[1] <- "variable"

# Print the column names and the summarized data
print(colnames(meanci_by_education))
print(meanci_by_education)

# Conduct a t-test to calculate the p-value
t_test_result <- t.test(haemoglobin ~ education_level_summary2, data = anc_data_processed06_clean)
p_value <- t_test_result$p.value

# Print the p-value
print(paste("P-value:", p_value))

# Create a box plot for haemoglobin levels by education level summary
ggplot(anc_data_processed06, aes(x = education_level_summary2, y = haemoglobin, fill = education_level_summary2)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of Haemoglobin Levels by Education Level Summary",
    x = "Education Level",
    y = "Haemoglobin Level (g/dL)",
    fill = "Education Level Summary"  # Change legend title
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")
