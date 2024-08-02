####data cleaning####
anc_data_processed06 <- read_csv("data/anc_data_processed06.csv")
#all data
anc_data_processed06<- anc_data_processed06 %>%
  dplyr::mutate(
    total_n = n(),
    age_group = cut(
      x = age,
      breaks = c( 15, 20, 25, 30, 35, 40, 45, Inf),
      labels = c(
        "15 to 19 years", "20 to 24 years", "25 to 29 years", 
        "30 to 34 years", "35 to 39, years", 
        "40 to 44 years","45 years and older"
      ),
      include.lowest = TRUE, right = FALSE
    ),
    education_level_summary1= case_when(
      education_level %in% c( "None", "Primary","Junior High School") ~ "no education and Primary",
      education_level %in% c( "Senior High School", "Tertiary") ~ "Senior high school and higher",
      TRUE ~ "NA"
    ),
    profession_group= case_when(
      profession %in% c("None","Student" ) ~ "Unemployed",
      profession %in% c("Teacher", "Company Employee", "Midwife","Undertaker", "Trader", "Seamstress", "Hair Dresser", "Fishmonger", "Farmer", "Decorator", "Caterer", "Business Owner") ~ "Employed",
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
#anaemia catergory
an_c_counts <- table(anc_data_processed06$anaemia_category)
print(an_c_counts)
an_c_percentages <- prop.table(an_c_counts) * 100
an_c_s <- data.frame(
  Anaemia_catergory = names(an_c_counts),
  Count_and_Percentage = paste(an_c_counts, "(", round(an_c_percentages, 2), "%)", sep = "")
)
print()

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

####Mean,median####
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



###
## Processing code for re-classification of age ----
df <- anc_data_recode |>
  dplyr::mutate(
    early_childbearing = ifelse(age < 20, "Yes", "No") |>
      factor(levels = c("Yes", "No"))
  )

## Check re-classification ----
table(df$early_childbearing)


df <- anc_data_processed06 |>
  dplyr::mutate(
    livelihoods = ifelse(profession %in% c("None", "Student"), "No", "Yes") |>
      factor(levels = c("Yes", "No"))
  )

## Check re-classification ----
table(df$livelihoods)


## Processing code for re-classification of education level ----
df <- df |>
  dplyr::mutate(
    secondary_education = ifelse(
      education_level %in% c("None", "Primary"), "No", "Yes"
    ) |>
      factor(levels = c("Yes", "No"))
  )

## Check re-classification ----
table(df$secondary_education)



## Tabulate marital status ----
table(df$marital_status)





## Tabulate sickle cell ----
table(df$sickle_cell)


## Processing code for re-classification of location ----
df <- df |>
  dplyr::mutate(
    location = ifelse(
      address %in% c("Anomabo", "Biriwa", "Asafora"), 
      "Within community", "Outside community"
    ) |>
      factor(levels = c("Within community", "Outside community"))
  )

## Check re-classification ----
table(df$location)