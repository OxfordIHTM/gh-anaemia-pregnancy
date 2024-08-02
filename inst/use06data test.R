####data cleaning####
anc_data_processed06 <- read_csv("data/anc_data_processed06.csv")
#all data
data<- anc_data_processed06 %>%
  dplyr::mutate(
    total_n = n(),
    age_group = cut(
      x = age,
      breaks = c( 15, 20, 25, 30, 35, 40, 45),
      labels = c(
        "15 to 19 years", "20 to 24 years", "25 to 29 years", 
        "30 to 34 years", "35 to 39, years", 
        "40 to 44 years"
      ),
      include.lowest = TRUE, right = FALSE
    ),
    education_level_summary1= case_when(
      education_level %in% c( "None", "Primary") ~ "no education and Primary",
      education_level %in% c("Junior High School", "Senior High School", "Tertiary") ~ "Junior high school and higher",
      TRUE ~ "NA"
    ),
    profession_group= case_when(
      profession %in% c("None","Student" ) ~ "Unemployed",
      profession %in% c("Teacher", "Company Employee", "Midwife","Undertaker", "Trader", "Seamstress", "Hair Dresser", "Fishmonger", "Farmer", "Decorator", "Caterer", "Business Owner") ~ "Employed",
      TRUE ~ "NA"
    ),
    location_group= case_when(
      address %in% c( "Makassium","Saltpond Zongo","Anomabo", "Biriwa", "Abandze", "Yamoransa","Kormantse","Ekon","Cape Coast") ~ "Urban",
      address %in% c("Amoanda","Buranamoah","Moree","Asafora", "Aketekyiwa","Egyirefa","Pomasi(Pomase)", "Waakrom", "Afrago Junction", "Amissakrom", "Egyierefa","Insanfo(NSANFO)","Ekotokrom","Amissakrom","Eguase") ~ "Rural Town",
      TRUE ~ "NA"
    ),
    location_group2= case_when(
      address%in% c( "Anomabo", "Asafora", "Biriwa","Abandze", "Aketekyiwa",
                            "Buranamoah", "Eguase", "Kormantse", "Moree", "Pomasi (Pomase)", "Waakrom", "Yamoransa") ~ "<10km",
      address %in% c(" Afrago Junction", "Amissakrom", "Amoanda", "Cape Coast", "Egyirefa", "Ekon", "Ekotokrom",
      "Insanfo (Nsanfo)", "Makassium", "Saltpond Zongo") ~ ">10km",
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
# education levels
education_counts <- table(data$education_level)
print(education_counts)
education_percentages <- prop.table(education_counts) * 100
education <- data.frame(
  Education_Level = names(education_counts),
  Count_and_Percentage = paste(education_counts, "(", round(education_percentages, 2), "%)", sep = "")
)


# profession
profession_counts <- table(data$profession)
print(profession_counts)
profession_percentages <- prop.table(profession_counts) * 100
Profession <- data.frame(
  Profession_Level = names(profession_counts),
  Count_and_Percentage = paste(profession_counts, "(", round(profession_percentages, 2), "%)", sep = "")
)
#address
address_counts <- table(data$address)
print(address_counts)
address_percentages <- prop.table(address_counts) * 100
Address <- data.frame(
  Address_Level = names(address_counts),
  Count_and_Percentage = paste(address_counts, "(", round(address_percentages, 2), "%)", sep = "")
)
print(Address)

#marital status
marital_status_counts <- table(data$marital_status)
print(marital_status_counts)
marital_status_percentages <- prop.table(marital_status_counts) * 100
Marital_Status <- data.frame(
  Marital_Status_Level = names(marital_status_counts),
  Count_and_Percentage = paste(marital_status_counts, "(", round(marital_status_percentages, 2), "%)", sep = "")
)
print(Marital_Status)


#sickle cell
sickle_cell_counts <- table(data$sickle_cell)
print(sickle_cell_counts)
sickle_cell_percentages <- prop.table(sickle_cell_counts) * 100
Sickle_Cell <- data.frame(
  Sickle_Cell_Level = names(sickle_cell_counts),
  Count_and_Percentage = paste(sickle_cell_counts, "(", round(sickle_cell_percentages, 2), "%)", sep = "")
)
print(Sickle_Cell)


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
  labs(title = "Relationship between Age and Haemoglobin Levels",
       x = "Age(year)",
       y = "Haemoglobin Level(g/dL)") +
  theme_minimal()
#####byErnest ####
## Processing code for re-classification of age ----
df <- data |>
  dplyr::mutate(
    early_childbearing = ifelse(age < 20, "Yes", "No") |>
      factor(levels = c("Yes", "No"))
  )

## Check re-classification ----
table(df$early_childbearing)
## Processing code for re-classification of profession ----
df <- df |>
  dplyr::mutate(
    livelihoods = ifelse(profession %in% c("None", "Student"), "No", "Yes") |>
      factor(levels = c("Yes", "No"))
  )

## Check re-classification ----
table(df$livelihoods)
