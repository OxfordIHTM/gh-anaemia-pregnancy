####only 2023 DATA SET###

#only 2023 data
anc_data_processed_subset_2023 <- anc_data_processed07 %>%
  filter(age != 1,year <2024) %>%
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
    profession_group= case_when(
      profession %in% c( "Teacher", "Company Employee", "Midwife") ~ "Employed",
      profession %in% c("Undertaker", "Trader", "Seamstress", "Hair Dresser", "Fishmonger", "Farmer", "Decorator", "Caterer", "Business Owner","Student") ~ "Self-Employed",
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


  

##-----------------------------------------------------------------------------------------------------------------------##
#### Mean, Standard Deviation,Median,Interquartile Range, IQR####

overall_stats <- anc_data_processed_subset_2023 %>%
  summarize(
    mean_age = mean(age, na.rm = TRUE),
    youngest_age = min(age, na.rm = TRUE),
    oldest_age = max(age, na.rm = TRUE)
  )

print("Overall Age Statistics:")
print(overall_stats)

#age mean, Standard Deviation,Median,Interquartile Range, IQR
mean(anc_data_processed_subset_2023$age)
sd(anc_data_processed_subset_2023$age)
median(anc_data_processed_subset_2023$age)
IQR(anc_data_processed_subset_2023$age)
#Shapiro-Wilk test
shapiro.test(anc_data_processed_subset_2023$age)

#in anaemia caterogy
age_summary <- anc_data_processed_subset_2023 %>%
  group_by(anaemia_category) %>%
  summarize(
    youngest_age = min(age, na.rm = TRUE),
    oldest_age = max(age, na.rm = TRUE)
  )

# Print the summary
print(age_summary)





#---in anaemia group
anaemia_data_2023<-anc_data_processed_subset_2023%>% filter(haemoglobin<11)
mean(anaemia_data_2023$age)
sd(anaemia_data_2023$age)
median(anaemia_data_2023$age)
IQR(anaemia_data_2023$age)

#Shapiro-Wilk test
shapiro.test(anaemia_data_2023$age)

group_astats <- anaemia_data_2023 %>%
  summarize(
    mean_age = mean(age, na.rm = TRUE),
    youngest_age = min(age, na.rm = TRUE),
    oldest_age = max(age, na.rm = TRUE)
  )

print("Age Statistics by Anaemia Status:")
print(group_astats)






#---in nonanaemia group
nonanaemia_data_2023<-anc_data_processed_subset_2023%>% filter(haemoglobin>=11)
mean(nonanaemia_data_2023$age)
sd(nonanaemia_data_2023$age)
median(nonanaemia_data_2023$age)
IQR(nonanaemia_data_2023$age)
#Shapiro-Wilk test
shapiro.test(nonanaemia_data_2023$age)


group_nstats <- nonanaemia_data_2023 %>%
  summarize(
    mean_age = mean(age, na.rm = TRUE),
    youngest_age = min(age, na.rm = TRUE),
    oldest_age = max(age, na.rm = TRUE)
  )

print("Age Statistics by Anaemia Status:")
print(group_nstats)




#hb mean, Standard Deviation,Median,Interquartile Range, IQR
mean(anc_data_processed_subset_2023$haemoglobin,na.rm=TRUE)
sd(anc_data_processed_subset_2023$haemoglobin,na.rm=TRUE)
median(anc_data_processed_subset_2023$haemoglobin,na.rm=TRUE)
min(anc_data_processed_subset_2023$haemoglobin,na.rm=TRUE)
max(anc_data_processed_subset_2023$haemoglobin,na.rm=TRUE)
IQR(anc_data_processed_subset_2023$haemoglobin,na.rm=TRUE)
median(anc_data_processed_subset_2023$haemoglobin,na.rm=TRUE)

#Shapiro-Wilk test
shapiro.test(anc_data_processed_subset_2023$haemoglobin)





#haemoglobin in anaemia mean, Standard Deviation,Median,Interquartile Range, IQR
mean(anaemia_data_2023$haemoglobin,na.rm=TRUE)
sd(anaemia_data_2023$haemoglobin,na.rm=TRUE)
median(anaemia_data_2023$haemoglobin,na.rm=TRUE)
IQR(anaemia_data_2023$haemoglobin,na.rm=TRUE)
#Shapiro-Wilk test
shapiro.test(anaemia_data_2023$haemoglobin)

#haemoglobin in nonanaemia mean, Standard Deviation,Median,Interquartile Range, IQR
mean(nonanaemia_data_2023$haemoglobin,na.rm=TRUE)
sd(nonanaemia_data_2023$haemoglobin,na.rm=TRUE)
median(nonanaemia_data_2023$haemoglobin,na.rm=TRUE)
IQR(nonanaemia_data_2023$haemoglobin,na.rm=TRUE)
#Shapiro-Wilk test
shapiro.test(nonanaemia_data_2023$haemoglobin)



# Mean, Standard Deviation,Median,Interquartile Range, IQR

#age mean, Standard Deviation,Median,Interquartile Range, IQR
mean(anc_data_processed_subset_2023$age)
sd(anc_data_processed_subset_2023$age)
median(anc_data_processed_subset_2023$age)
IQR(anc_data_processed_subset_2023$age)
#Shapiro-Wilk test
shapiro.test(anc_data_processed_subset_2023$age)

####variable summary####
# Calculate the number of people with different education levels
education_counts <- table(anc_data_processed_subset_2023$education_level)
print(education_counts)

# Calculate the percentage of people with different education levels
education_percentages <- prop.table(education_counts) * 100

# Combine counts and percentages into a single table
education_summary <- data.frame(
  Education_Level = names(education_counts),
  Count_and_Percentage = paste(education_counts, "(", round(education_percentages, 2), "%)", sep = "")
)

print(education_summary)

# Calculate the number of people in each profession group
profession_counts <- table(anc_data_processed_subset_2023$profession_summary1)
print(profession_counts)
profession_percentages <- prop.table(profession_counts) * 100
profession_summary <- data.frame(
  Profession = names(profession_counts),
  Count_and_Percentage = paste(profession_counts, "(", round(profession_percentages, 2), "%)", sep = "")
)
print(profession_summary)

# Calculate the number of people in each profession
profession_c <- table(anc_data_processed_subset_2023$profession)
print(profession_c)
profession_per <- prop.table(profession_c) * 100
profession <- data.frame(
  Profession = names(profession_c),
  Count_and_Percentage = paste(profession_c, "(", round(profession_per, 2), "%)", sep = "")
)
print(profession)

# Calculate the number of people in each location group
location_counts <- table(anc_data_processed_subset_2023$location_group)
print(location_counts)
location_percentages <- prop.table(location_counts) * 100
location_summary <- data.frame(
  Location_Group = names(location_counts),
  Count_and_Percentage = paste(location_counts, "(", round(location_percentages, 2), "%)", sep = "")
)
print(location_summary)

# Calculate the number of people in each location 
address_counts <- table(anc_data_processed_subset_2023$address)
print(address_counts)
address_percentages <- prop.table(address_counts) * 100
address_summary <- data.frame(
  Address_Group = names(address_counts),
  Count_and_Percentage = paste(address_counts, "(", round(address_percentages, 2), "%)", sep = "")
)
print(address_summary)


# Calculate the number of people in each marital status category
marital_counts <- table(anc_data_processed_subset_2023$marital_status)
print(marital_counts)
marital_percentages <- prop.table(marital_counts) * 100
marital_summary <- data.frame(
  Marital_Status = names(marital_counts),
  Count_and_Percentage = paste(marital_counts, "(", round(marital_percentages, 2), "%)", sep = "")
)

print(marital_summary)

# Calculate the number of people in sickle cell
sicklecell_counts <- table(anc_data_processed_subset_2023$sickle_cell)
print(sicklecell_counts)
sicklecell_percentages <- prop.table(sicklecell_counts) * 100
sicklecell_summary <- data.frame(
  sicklecell = names(sicklecell_counts),
  Count_and_Percentage = paste(sicklecell_counts, "(", round(sicklecell_percentages, 2), "%)", sep = "")
)

print(sicklecell_summary)


##-----------------------------------------------------------------------------------------------------##
####anaemia status and different varable####
# anaemia status and age group
anaemia_age_group_2023 <- anc_data_processed_subset_2023 %>%
  group_by(anaemia_status, age_group) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(anaemia_status) %>%
  mutate(percent = count / sum(count) * 100) %>%
  ungroup()



anaemia1_age_group_2023 <- anc_data_processed_subset_2023 %>%
  group_by(anaemia_category, age_group) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(anaemia_category) %>%
  mutate(percent = count / sum(count) * 100) %>%
  ungroup()





# anaemia status and education group
anaemia_education_group_2023 <- anc_data_processed_subset_2023 %>%
  group_by(anaemia_status, education_level_summary) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(anaemia_status) %>%
  mutate(percent = count / sum(count) * 100) %>%
  ungroup()
# 
anaemia_education_group_2023_long <- anaemia_education_group_2023  %>%
  pivot_longer(cols = c(count, percent), names_to = "measure", values_to = "value")

education_group_2023_table <- anaemia_education_group_2023_long  %>%
  pivot_wider(names_from = c(anaemia_status, measure), values_from = value)
print(education_group_2023_table)





# anaemia status and education group
anaemia_education_2023 <- anc_data_processed_subset_2023 %>%
  group_by(anaemia_status, education_level) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(anaemia_status) %>%
  mutate(percent = count / sum(count) * 100) %>%
  ungroup()
# 
anaemia_education_2023_long <- anaemia_education_2023  %>%
  pivot_longer(cols = c(count, percent), names_to = "measure", values_to = "value")

education_2023_table <- anaemia_education_2023_long  %>%
  pivot_wider(names_from = c(anaemia_status, measure), values_from = value)
print(education_2023_table)
write_xlsx(education_2023_table, "education.xlsx")


anaemia_profession_group_2023 <- anc_data_processed_subset_2023 %>%
  filter(!is.na(anaemia_status) & !is.na(profession_summary1)) %>%  # 去除NA值
  group_by(anaemia_status, profession_summary1) %>%
  summarize(count = n(), .groups = 'drop') %>%  # 使用.drop來避免分組信息被保留
  group_by(anaemia_status) %>%
  mutate(percent = count / sum(count) * 100) %>%
  ungroup()

# 檢查結果
print(anaemia_profession_group_2023)
# 
anaemia_profession_group_2023_long <- anaemia_profession_group_2023  %>%
  pivot_longer(cols = c(count, percent), names_to = "measure", values_to = "value")

profession_group_2023_table <- anaemia_profession_group_2023_long  %>%
  pivot_wider(names_from = c(anaemia_status, measure), values_from = value)
print(profession_group_2023_table)



# anaemia status and profession 
anaemia_profession_2023 <- anc_data_processed_subset_2023 %>%
  group_by(anaemia_status, profession) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(anaemia_status) %>%
  mutate(percent = count / sum(count) * 100) %>%
  ungroup()
# 
anaemia_profession_2023_long <- anaemia_profession_2023  %>%
  pivot_longer(cols = c(count, percent), names_to = "measure", values_to = "value")
profession_2023_table <- anaemia_profession_2023_long  %>%
  pivot_wider(names_from = c(anaemia_status, measure), values_from = value)
print(profession_2023_table)




# anaemia status and location summary
anaemia_location_group_2023 <- anc_data_processed_subset_2023 %>%
  group_by(anaemia_status, location_group) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(anaemia_status) %>%
  mutate(percent = count / sum(count) * 100) %>%
  ungroup()
# long
anaemia_location_group_2023_long <- anaemia_location_group_2023  %>%
  pivot_longer(cols = c(count, percent), names_to = "measure", values_to = "value")
location_group_2023_table <- anaemia_location_group_2023_long  %>%
  pivot_wider(names_from = c(anaemia_status, measure), values_from = value)
print(location_group_2023_table)



# Calculate counts and percentages
anaemia_location_2023 <- anc_data_processed_subset_2023 %>%
  group_by(anaemia_status, address) %>%
  summarize(count = n(), .groups = 'drop') %>%
  group_by(anaemia_status) %>%
  mutate(percent = count / sum(count) * 100) %>%
  ungroup()

# Combine count and percent into a single column
anaemia_location_2023 <- anaemia_location_2023 %>%
  mutate(count_and_percent = paste(count, "(", round(percent, 2), "%)", sep = ""))

# Create the final table
location_2023_table <- anaemia_location_2023 %>%
  select(address, anaemia_status, count_and_percent) %>%
  pivot_wider(names_from = anaemia_status, values_from = count_and_percent)

print(location_2023_table)






# anaemia status and marital group
anaemia_marital_2023 <- anc_data_processed_subset_2023 %>%
  group_by(anaemia_status, marital_status) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(anaemia_status) %>%
  mutate(percent = count / sum(count) * 100) %>%
  ungroup()
# long
anaemia_marital_2023_long <- anaemia_marital_2023  %>%
  pivot_longer(cols = c(count, percent), names_to = "measure", values_to = "value")
marital_2023_table <- anaemia_marital_2023_long  %>%
  pivot_wider(names_from = c(anaemia_status, measure), values_from = value)

print(marital_2023_table)
write_xlsx(marital_2023_table, "marital.xlsx")


# anaemia status and sickle cell
anaemia_sicklecell_2023 <- anc_data_processed_subset_2023 %>%
  group_by(anaemia_status, sickle_cell) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(anaemia_status) %>%
  mutate(percent = count / sum(count) * 100) %>%
  ungroup()
# 
anaemia_sicklecell_2023_long <- anaemia_sicklecell_2023  %>%
  pivot_longer(cols = c(count, percent), names_to = "measure", values_to = "value")
sicklecell_2023_table <- anaemia_sicklecell_2023_long  %>%
  pivot_wider(names_from = c(anaemia_status, measure), values_from = value)
print(sicklecell_2023_table)
write_xlsx(sicklecell_2023_table, "sicklecell.xlsx")



# anaemia status and malaria
anaemia_malaria_2023 <- anc_data_processed_subset_2023 %>%
  group_by(anaemia_status, malaria) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(anaemia_status) %>%
  mutate(percent = count / sum(count) * 100) %>%
  ungroup()
# 
anaemia_malaria_2023_long <- anaemia_malaria_2023  %>%
  pivot_longer(cols = c(count, percent), names_to = "measure", values_to = "value")
malaria_2023_table <- anaemia_malaria_2023_long  %>%
  pivot_wider(names_from = c(anaemia_status, measure), values_from = value)
print(malaria_2023_table)
write_xlsx(malaria_2023_table, "malaria.xlsx")





# Create overall summary table ----
median_age_tab <- anc_data_processed_subset_2023 %>%
  dplyr::summarise(
    var = "median_age",
    median_age = median(age, na.rm = TRUE)
  )


#--------------------------------------------------------------------------
####95%CI####
#all
all_age_ci <- t.test(anc_data_processed_subset_2023$age)
cat("95% Confidence Interval for Age using t.test: [", all_age_ci$conf.int[1], ", ", all_age_ci$conf.int[2], "]\n")
#Hb
all_hb_ci <- t.test(anc_data_processed_subset_2023$haemoglobin)
cat("95% Confidence Interval for Age using t.test: [", all_hb_ci$conf.int[1], ", ", all_hb_ci$conf.int[2], "]\n")


#anaemia
an_age_ci <- t.test(anaemia_data_2023$age)
cat("95% Confidence Interval for Age using t.test: [", an_age_ci$conf.int[1], ", ", an_age_ci$conf.int[2], "]\n")
#Hb
an_hb_ci <- t.test(anaemia_data_2023$haemoglobin)
cat("95% Confidence Interval for Age using t.test: [", an_hb_ci$conf.int[1], ", ", an_hb_ci$conf.int[2], "]\n")


#nonanaemia
nonan_age_ci <- t.test(nonanaemia_data_2023$age)
cat("95% Confidence Interval for Age using t.test: [", nonan_age_ci$conf.int[1], ", ", nonan_age_ci$conf.int[2], "]\n")
#Hb
nonan_hb_ci <- t.test(nonanaemia_data_2023$haemoglobin)
cat("95% Confidence Interval for Age using t.test: [", nonan_hb_ci$conf.int[1], ", ", nonan_hb_ci$conf.int[2], "]\n")


###---------------------------------------------------------------------------------------------------------------
####Analysis:linear,chi-square####
#linaer 
linear_model <- lm(haemoglobin ~ age, data = anc_data_processed_subset_2023)
# Summarize the model
summary(linear_model)
#Age and anaemia status t test
t_test <- t.test(age ~ anaemia_status, data = anc_data_processed_subset_2023)
print(t_test)

wilcox_test <- wilcox.test(age ~ anaemia_status, data = anc_data_processed_subset_2023)
print(wilcox_test)

#chi square test
####summary of bivariate anaylisis####
#age group summary
ag_cross_tab <- table(anc_data_processed_subset_2023$age_group, anc_data_processed_subset_2023$anaemia_status)
print(ag_cross_tab)
ag_chi_square_test <- chisq.test(ag_cross_tab)
print(ag_chi_square_test)



#education summary
ed_cross_tab <- table(anc_data_processed_subset_2023$education_level_summary, anc_data_processed_subset_2023$anaemia_status)
print(ed_cross_tab)
ed_chi_square_test <- chisq.test(ed_cross_tab)
print(ed_chi_square_test)

#profession summary
po_cross_tab <- table(anc_data_processed_subset_2023$profession_summary, anc_data_processed_subset_2023$anaemia_status)
print(po_cross_tab)
po_chi_square_test <- chisq.test(po_cross_tab)
print(po_chi_square_test)

#location summary3  #p=0.0393 X-squared = 8.9179, df = 3, p-value = 0.0304
pro_cross_tab <- table(anc_data_processed_subset_2023$address_group3, anc_data_processed_subset_2023$anaemia_status)
print(pro_cross_tab)
pro_chi_square_test <- chisq.test(pro_cross_tab)
print(pro_chi_square_test)

#location town 
# Create the contingency table
lot_cross_tab <- table(anc_data_processed_subset_2023$location_group, anc_data_processed_subset_2023$anaemia_status)
print(lot_cross_tab)
lot_chi_square_test <- chisq.test(lot_cross_tab)
print(lot_chi_square_test)

# Compute the percentage for each cell
lot_cross_tab_percentage <- prop.table(lot_cross_tab, margin = 1) * 100
print(lot_cross_tab_percentage)

# Combine the count and percentage into a single table for easier interpretation
lot_cross_tab_combined <- cbind(pro_cross_tab, round(lot_cross_tab_percentage, 2))
print(lot_cross_tab_combined)



#location summary
lo_cross_tab <- table(anc_data_processed_subset_2023$address_group, anc_data_processed_subset_2023$anaemia_status)
print(lo_cross_tab)
lo_chi_square_test <- chisq.test(lo_cross_tab)
print(lo_chi_square_test)
#location summary3  #p=0.0393 X-squared = 8.9179, df = 3, p-value = 0.0304
lo_cross_tab <- table(anc_data_processed_subset_2023$address_group3, anc_data_processed_subset_2023$anaemia_status)
print(lo_cross_tab)
lo_chi_square_test <- chisq.test(lo_cross_tab)
print(lo_chi_square_test)

anc_data_processed_subset_2023$anaemia_status <- as.factor(ifelse(anc_data_processed_subset_2023$anaemia_status == "Yes", 1, 0))
#logical regression
model <- glm(anaemia_status ~ address_group3, data = anc_data_processed_subset_2023, family = binomial)
# Step 4: Extract coefficients
coefficients <- summary(model)$coefficients





#marital summary
ma_cross_tab <- table(anc_data_processed_subset_2023$marital_status, anc_data_processed_subset_2023$anaemia_status)
print(ma_cross_tab)
ma_chi_square_test <- chisq.test(ma_cross_tab)
print(ma_chi_square_test)


#sickle cell summary
si_cross_tab <- table(anc_data_processed_subset_2023$sickle_cell, anc_data_processed_subset_2023$anaemia_status)
print(si_cross_tab)
si_chi_square_test <- chisq.test(si_cross_tab)
print(si_chi_square_test)


###---------------------------------------------------------------------------------------------------------------
####Visualzation####
# education
ggplot(anc_data_processed_subset_2023, aes(x = education_level_summary, fill = anaemia_status)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Anaemia Status by Education",
       x = "Education Level",
       y = "Proportion",
       fill = "Anaemia Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# profession
ggplot(anc_data_processed_subset_2023, aes(x = profession_summary, fill = anaemia_status)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Anaemia Status by Profession",
       x = "Profession",
       y = "Proportion",
       fill = "Anaemia Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

anc_data_processed_subset_2023 <- anc_data_processed_subset_2023 %>%
  mutate(profession_summary2 = ifelse(is.na(profession_summary2), "No Data", profession_summary2),
         anaemia_status = ifelse(is.na(anaemia_status), "No Data", anaemia_status))

# Create the plot with modified labels
ggplot(anc_data_processed_subset_2023, aes(x = profession_summary2, fill = anaemia_status)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Anaemia Status by Profession",
       x = "Profession",
       y = "Proportion",
       fill = "Anaemia Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = c("No Data" = "No Data")) +  # Adjust labels for x-axis if necessary
  scale_fill_discrete(labels = c("No Data" = "No Data"))  # Adjust labels for fill legend

# distance 
ggplot(anc_data_processed_subset_2023, aes(x = address_group, fill = anaemia_status)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Anaemia Status by Distance",
       x = "Distance",
       y = "Proportion",
       fill = "Anaemia Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# marital status
ggplot(anc_data_processed_subset_2023, aes(x = marital_status, fill = anaemia_status)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Anaemia Status by Marital status",
       x = "Marital status",
       y = "Proportion",
       fill = "Anaemia Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# sickle cell
ggplot(anc_data_processed_subset_2023, aes(x = sickle_cell, fill = anaemia_status)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Anaemia Status by Sickle Cell ",
       x = "Sickle Cell Test Result",
       y = "Proportion",
       fill = "Anaemia Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# malaria 
ggplot(anc_data_processed_subset_2023, aes(x = malaria, fill = anaemia_status)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Anaemia Status by Malaria  ",
       x = "Malaria Test Result",
       y = "Proportion",
       fill = "Anaemia Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# pearson
pearson_cor <- cor.test(anc_data_processed_subset_2023$age, anc_data_processed_subset_2023$haemoglobin, method = "pearson")
print(pearson_cor)
spearman_cor <- cor.test(anc_data_processed_subset_2023$age, anc_data_processed_subset_2023$haemoglobin, method = "spearman")
print(spearman_cor)



###-----------------------------------------------------------------------------------------------------
####power####
calculate_power <- function(observed_table, chi_square_statistic, df) {
  n <- sum(observed_table)
  w <- sqrt(chi_square_statistic / n)
  power_result <- pwr.chisq.test(w = w, N = n, df = df, sig.level = 0.05)
  return(power_result$power)
}
age_anaemia_table <- table(anc_data_processed_subset_2023$age_group, anc_data_processed_subset_2023$anaemia_status)

# Perform the chi-square test
age_anaemia_chi_square <- chisq.test(age_anaemia_table)

# Calculate percentages
age_anaemia_percentages <- prop.table(age_anaemia_table, margin = 1) * 100

# Combine counts and percentages into a single table
age_anaemia_summary <- as.data.frame.matrix(age_anaemia_table)
age_anaemia_summary$Age_Group <- rownames(age_anaemia_summary)
rownames(age_anaemia_summary) <- NULL

# Combine counts and percentages into a single column
for (col in colnames(age_anaemia_table)) {
  age_anaemia_summary[[col]] <- paste0(age_anaemia_summary[[col]], " (", round(age_anaemia_percentages[, col], 2), "%)")
}

# Reorder columns to put Age_Group first
age_anaemia_summary <- age_anaemia_summary %>%
  select(Age_Group, everything())

# Print the summary table
print(age_anaemia_summary)

# Print the chi-square test results
age_anaemia_test_result <- data.frame(
  Variable = "Age Group",
  Chi_Square = age_anaemia_chi_square$statistic,
  DF = age_anaemia_chi_square$parameter,
  P_Value = age_anaemia_chi_square$p.value,
  stringsAsFactors = FALSE
)

print(age_anaemia_test_result)

# Education summary
ed_cross_tab <- table(anc_data_processed_subset_2023$education_level_summary, anc_data_processed_subset_2023$anaemia_status)
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
po_cross_tab <- table(anc_data_processed_subset_2023$profession_summary1, anc_data_processed_subset_2023$anaemia_status)
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
lo_cross_tab <- table(anc_data_processed_subset_2023$location_group, anc_data_processed_subset_2023$anaemia_status)
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
ma_cross_tab <- table(anc_data_processed_subset_2023$marital_status, anc_data_processed_subset_2023$anaemia_status)
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
si_cross_tab <- table(anc_data_processed_subset_2023$sickle_cell, anc_data_processed_subset_2023$anaemia_status)
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
  si_result,
)

print(combined_results)
##-----------------------------------------------------------------------------------------------------------------------------
#### histogram####
#age distribution histogram
anc_data_processed_subset_2023 |>
  dplyr::filter(!is.na(anaemia_status)) |>
  dplyr::count(anaemia_status, age_group) |>
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

# Create scatter plot
ggplot(anc_data_processed_subset_2023, aes(x = age, y = haemoglobin)) +
  geom_point(color = "blue", size = 2) +
  labs(title = "Scatter Plot of Age and Hemoglobin Levels",
       x = "Age (years)",
       y = "Hemoglobin Level (g/dL)") +
  theme_minimal()



anc_data_processed_subset_2023 |>
  dplyr::filter(!is.na(anaemia_status)) |>
  ggplot(mapping = aes(x = anaemia_status, y = age)) +
  geom_violin(
    fill = oxthema::get_oxford_colours("sky"), alpha = 0.8,
    colour = oxthema::get_oxford_colours("Oxford blue")
  ) + 
  labs(
    title = "Age Distribution by Anaemia Status",
    subtitle = "Women attending ANC in 2023",
    x = "",
    y = "Age (years)"
  ) + 
  oxthema::theme_oxford(grid = "Yy")
##------------------------------------------------------------------------------------------
####boxplot####
#Education
# Replace NA values and "No value" with "No Data"
anc_data_processed_subset_2023 <- anc_data_processed_subset_2023 %>%
  mutate(profession_summary1 = ifelse(is.na(profession_summary1) | profession_summary1 == "No value", "No Data", profession_summary1),
         education_level_summary = ifelse(is.na(education_level_summary) | education_level_summary == "No value", "No Data", education_level_summary),
         location_group = ifelse(is.na(location_group) | location_group == "No value", "No Data", location_group),
         marital_status = ifelse(is.na(marital_status) | marital_status == "No value", "No Data", marital_status),
         sickle_cell = ifelse(is.na(sickle_cell) | sickle_cell == "No value", "No Data", sickle_cell),
         anaemia_status = ifelse(is.na(anaemia_status), "No Data", anaemia_status))

anc_data_processed_subset_2023$education_level <- factor(anc_data_processed_subset_2023$education_level_summary,
                                                         levels = c("Primary/Junior High School", "Senior High School and higher", "None", "No Data"),
                                                         labels = c("Primary/Junior High School", "Senior High School and higher","No Education", "No Data"))



# Create the plot with modified labels for education_level
ggplot(anc_data_processed_subset_2023, aes(x = education_level_summary, y = haemoglobin, fill = education_level_summary)) +
  geom_boxplot() +
  labs(title = "Boxplot of Haemoglobin Levels by Education Level",
       x = "Education Level",
       y = "Haemoglobin Level (g/dL)",
       fill = "Education Level") +  # Change legend title
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

#profession and Hb
levels(anc_data_processed_subset_2023$profession_summary1) <- c("Employed", "Self-employed (Formal)", "Self-employed (Informal)", "No Income","No data")
ggplot(anc_data_processed_subset_2023, aes(x = profession_summary1, y = haemoglobin, fill = profession_summary1)) +
  geom_boxplot() +
  labs(title = "Boxplot of Haemoglobin Levels by Profession",
       x = "Profession",
       y = "Haemoglobin Level (g/dL)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")


# Replace NA values with "No Data"
anc_data_processed_subset_2023 <- anc_data_processed_subset_2023 %>%
  mutate(profession_summary1 = ifelse(is.na(profession_summary1), "No Data", profession_summary1),
         anaemia_status = ifelse(is.na(anaemia_status), "No Data", anaemia_status))

# Update factor levels to change the labels
anc_data_processed_subset_2023$profession_summary1 <- factor(anc_data_processed_subset_2023$profession_summary1,
                                                             levels = c("Employed", "Self-employed with formal training", "Self-employed with no formal training", "None", "No Data"),
                                                             labels = c("Employed", "Self-employed with formal training", "Self-employed with no formal training", "No Income", "No Data"))

# Create the plot with modified labels
ggplot(anc_data_processed_subset_2023, aes(x = profession_summary1, y = haemoglobin, fill = profession_summary1)) +
  geom_boxplot() +
  labs(title = "Boxplot of Haemoglobin Levels by Profession",
       x = "Profession",
       y = "Haemoglobin Level (g/dL)",
       fill = "Profession") +  # Change legend title
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")






# Create the plot with modified labels for location_group
ggplot(anc_data_processed_subset_2023, aes(x = location_group, y = haemoglobin, fill = location_group)) +
  geom_boxplot() +
  labs(title = "Boxplot of Haemoglobin Levels by Location",
       x = "Location",
       y = "Haemoglobin Level (g/dL)",
       fill = "Location") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

#marital status and Hb
ggplot(anc_data_processed_subset_2023, aes(x = marital_status, y = haemoglobin, fill = marital_status)) +
  geom_boxplot() +
  labs(title = "Boxplot of Haemoglobin Levels by Marital status",
       x = "Marital Status",
       y = "Haemoglobin Level (g/dL)",
       fill= "Marital Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

#sickle cell and Hb
ggplot(anc_data_processed_subset_2023, aes(x = sickle_cell, y = haemoglobin, fill = sickle_cell)) +
  geom_boxplot() +
  labs(title = "Boxplot of Haemoglobin Levels by Sickle Cell Test result",
       x = "Sickle Cell Test Result",
       y = "Haemoglobin Level (g/dL)",
       fill= "Sickle Cell Test result") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")


#malaria and Hb
ggplot(anc_data_processed_subset_2023, aes(x = malaria, y = haemoglobin, fill = malaria)) +
  geom_boxplot() +
  labs(title = "Boxplot of Haemoglobin Levels by Malaria Test result",
       x = "Malaria Test Result",
       y = "Haemoglobin Level (g/dL)",
       fill= "Malaria Test Result ") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")
####-----------------------------------------------------------------------------------
####Mean 95%CI by Hb####
#education
meanci_by_ed <- anc_data_processed_subset_2023 %>%
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
meanci_by_profession <- anc_data_processed_subset_2023 %>%
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
meanci_by_lo <- anc_data_processed_subset_2023 %>%
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
meanci_by_mar <- anc_data_processed_subset_2023 %>%
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
meanci_by_si <- anc_data_processed_subset_2023 %>%
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
meanci_by_mal <- anc_data_processed_subset_2023 %>%
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

knitr::kable(
  x = rbind(
    meanci_by_ed,
    meanci_by_profession,
    meanci_by_lo,
    meanci_by_mar,
    meanci_by_si,
    meanci_by_mal
  ),
  col.names = c("Variable", "mean_hemoglobin", "sd_hemoglobin","lower_ci","upper_ci")
) |>
  kableExtra::kable_paper()


