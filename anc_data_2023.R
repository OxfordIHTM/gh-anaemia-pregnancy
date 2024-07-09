#only 2023 
anc_data_processed07 <- read_csv("data/anc_data_processed07.csv")
anc_data_processed_subset_2023 <- anc_data_processed07 %>%
  filter(age != 1, year < 2024) %>%
  dplyr::mutate(
    total_n = n(),
    age_group = cut(
      x = age,
      breaks = c(-Inf, 15, 20, 25, 30, 35, 40, 45, Inf),
      labels = c(
        "under 15 years", "15 to 19 years", "20 to 24 years", "25 to 29 years", 
        "30 to 34 years", "35 to 39 years", 
        "40 to 44 years", "45 years and older"
      ),
      include.lowest = TRUE, right = FALSE
    ),
    anaemia_status = ifelse(haemoglobin <= 11, TRUE, FALSE),
    
  )

  
##-----------------------------------------------------------------------------------------------------------------------##
# Mean, Standard Deviation,Median,Interquartile Range, IQR

#age mean, Standard Deviation,Median,Interquartile Range, IQR
mean(anc_data_processed_subset_2023$age)
sd(anc_data_processed_subset_2023$age)
median(anc_data_processed_subset_2023$age)
IQR(anc_data_processed_subset_2023$age)
#Shapiro-Wilk test
shapiro.test(anc_data_processed_subset_2023$age)


#---in anaemia group
anaemia_data_2023<-anc_data_processed_subset_2023%>% filter(haemoglobin<=11)
mean(anaemia_data_2023$age)
sd(anaemia_data_2023$age)
median(anaemia_data_2023$age)
IQR(anaemia_data_2023$age)
#Shapiro-Wilk test
shapiro.test(anaemia_data_2023$age)


#---in nonanaemia group
nonanaemia_data_2023<-anc_data_processed_subset_2023%>% filter(haemoglobin>11)
mean(nonanaemia_data_2023$age)
sd(nonanaemia_data_2023$age)
median(nonanaemia_data_2023$age)
IQR(nonanaemia_data_2023$age)
#Shapiro-Wilk test
shapiro.test(nonanaemia_data_2023$age)


#---in mildanaemia group------?/?
mildanaemia_data_2023<-anc_data_processed_subset_2023%>% filter(haemoglobin >= 11 & haemoglobin < 12 )
mean(mildanaemia_data_2023$age)
sd(mildanaemia_data_2023$age)
median(mildanaemia_data_2023$age)
IQR(mildanaemia_data_2023$age)
#Shapiro-Wilk test
shapiro.test(mildanaemia_data_2023$age)


#---in moderateanaemia group------?/?
moanaemia_data_2023<-anc_data_processed_subset_2023%>% filter(haemoglobin >= 7 & haemoglobin < 11 )
mean(moanaemia_data_2023$age)
sd(moanaemia_data_2023$age)
median(moanaemia_data_2023$age)
IQR(moanaemia_data_2023$age)
#Shapiro-Wilk test
shapiro.test(moanaemia_data_2023$age)


#---in severe anaemia group------?/?
seanaemia_data_2023<-anc_data_processed_subset_2023%>% filter(haemoglobin < 7 )
mean(seanaemia_data_2023$age)
sd(seanaemia_data_2023$age)
median(seanaemia_data_2023$age)
IQR(seanaemia_data_2023$age)
#Shapiro-Wilk test
shapiro.test(seanaemia_data_2023$age)


#hb mean, Standard Deviation,Median,Interquartile Range, IQR
mean(anc_data_processed_subset_2023$haemoglobin,na.rm=TRUE)
sd(anc_data_processed_subset_2023$haemoglobin,na.rm=TRUE)
median(anc_data_processed_subset_2023$haemoglobin,na.rm=TRUE)
IQR(anc_data_processed_subset_2023$haemoglobin,na.rm=TRUE)
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





##-----------------------------------------------------------------------------------------------------##

# anaemia status and age group
anaemia_age_group_2023 <- anc_data_processed_subset_2023 %>%
  group_by(anaemia_status, age_group) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(anaemia_status) %>%
  mutate(percent = count / sum(count) * 100) %>%
  ungroup()
# long
anaemia_age_group_2023_long <- anaemia_age_group_2023  %>%
  pivot_longer(cols = c(count, percent), names_to = "measure", values_to = "value")

age_group_2023_table <- anaemia_age_group_2023_long  %>%
  pivot_wider(names_from = c(anaemia_status, measure), values_from = value)

print(age_group_2023_table)
write_xlsx(age_group_2023_table, "age_group.xlsx")



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
write_xlsx(education_group_2023_table, "education_group.xlsx")




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


# anaemia status and profession group
anaemia_profession_group_2023 <- anc_data_processed_subset_2023 %>%
  group_by(anaemia_status, profession_summary) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(anaemia_status) %>%
  mutate(percent = count / sum(count) * 100) %>%
  ungroup()
# 
anaemia_profession_group_2023_long <- anaemia_profession_group_2023  %>%
  pivot_longer(cols = c(count, percent), names_to = "measure", values_to = "value")

profession_group_2023_table <- anaemia_profession_group_2023_long  %>%
  pivot_wider(names_from = c(anaemia_status, measure), values_from = value)
print(profession_group_2023_table)
write_xlsx(profession_group_2023_table, "profession_group.xlsx")


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
write_xlsx(profession_2023_table, "profession.xlsx")



# anaemia status and location summary
anaemia_location_group_2023 <- anc_data_processed_subset_2023 %>%
  group_by(anaemia_status, address_group) %>%
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

write_xlsx(location_group_2023_table, "location_group.xlsx")


# anaemia status and location
anaemia_location_2023 <- anc_data_processed_subset_2023 %>%
  group_by(anaemia_status, address) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  group_by(anaemia_status) %>%
  mutate(percent = count / sum(count) * 100) %>%
  ungroup()
# long
anaemia_location_2023_long <- anaemia_location_2023  %>%
  pivot_longer(cols = c(count, percent), names_to = "measure", values_to = "value")
location_2023_table <- anaemia_location_2023_long  %>%
  pivot_wider(names_from = c(anaemia_status, measure), values_from = value)

print(location_2023_table)
write_xlsx(location_2023_table, "location.xlsx")





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


##--------------------------------------------------------------------------

#95%CI
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
#Analysis
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


#location summary
lo_cross_tab <- table(anc_data_processed_subset_2023$address_group, anc_data_processed_subset_2023$anaemia_status)
print(lo_cross_tab)
lo_chi_square_test <- chisq.test(lo_cross_tab)
print(lo_chi_square_test)



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

#sickle cell summary
mal_cross_tab <- table(anc_data_processed_subset_2023$malaria, anc_data_processed_subset_2023$anaemia_status)
print(mal_cross_tab)
mal_chi_square_test <- chisq.test(mal_cross_tab)
print(mal_chi_square_test)
###---------------------------------------------------------------------------------------------------------------
#Visualzation
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
#
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

# Age group summary
ag_cross_tab <- table(anc_data_processed_subset_2023$age_group, anc_data_processed_subset_2023$anaemia_status)
ag_chi_square_test <- chisq.test(ag_cross_tab)
ag_result <- data.frame(
  Variable = "Age Group",
  Chi_Square = ag_chi_square_test$statistic,
  DF = ag_chi_square_test$parameter,
  P_Value = ag_chi_square_test$p.value
)

# Education summary
ed_cross_tab <- table(anc_data_processed_subset_2023$education_level_summary, anc_data_processed_subset_2023$anaemia_status)
ed_chi_square_test <- chisq.test(ed_cross_tab)
ed_result <- data.frame(
  Variable = "Education Level",
  Chi_Square = ed_chi_square_test$statistic,
  DF = ed_chi_square_test$parameter,
  P_Value = ed_chi_square_test$p.value
)

# Profession summary
po_cross_tab <- table(anc_data_processed_subset_2023$profession_summary, anc_data_processed_subset_2023$anaemia_status)
po_chi_square_test <- chisq.test(po_cross_tab)
po_result <- data.frame(
  Variable = "Profession",
  Chi_Square = po_chi_square_test$statistic,
  DF = po_chi_square_test$parameter,
  P_Value = po_chi_square_test$p.value
)

# Location summary
lo_cross_tab <- table(anc_data_processed_subset_2023$address_group, anc_data_processed_subset_2023$anaemia_status)
lo_chi_square_test <- chisq.test(lo_cross_tab)
lo_result <- data.frame(
  Variable = "Location",
  Chi_Square = lo_chi_square_test$statistic,
  DF = lo_chi_square_test$parameter,
  P_Value = lo_chi_square_test$p.value
)

# Marital summary
ma_cross_tab <- table(anc_data_processed_subset_2023$marital_status, anc_data_processed_subset_2023$anaemia_status)
ma_chi_square_test <- chisq.test(ma_cross_tab)
ma_result <- data.frame(
  Variable = "Marital Status",
  Chi_Square = ma_chi_square_test$statistic,
  DF = ma_chi_square_test$parameter,
  P_Value = ma_chi_square_test$p.value
)

# Sickle cell summary
si_cross_tab <- table(anc_data_processed_subset_2023$sickle_cell, anc_data_processed_subset_2023$anaemia_status)
si_chi_square_test <- chisq.test(si_cross_tab)
si_result <- data.frame(
  Variable = "Sickle Cell",
  Chi_Square = si_chi_square_test$statistic,
  DF = si_chi_square_test$parameter,
  P_Value = si_chi_square_test$p.value
)

# Malaria summary
mal_cross_tab <- table(anc_data_processed_subset_2023$malaria, anc_data_processed_subset_2023$anaemia_status)
mal_chi_square_test <- chisq.test(mal_cross_tab)
mal_result <- data.frame(
  Variable = "Malaria",
  Chi_Square = mal_chi_square_test$statistic,
  DF = mal_chi_square_test$parameter,
  P_Value = mal_chi_square_test$p.value
)

# 
combined_results <- bind_rows(
  ag_result,
  ed_result,
  po_result,
  lo_result,
  ma_result,
  si_result,
  mal_result
)

print(combined_results)
##-----------------------------------------------------------------------------------------------------------------------------



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
#boxplot

#profession and Hb
levels(anc_data_processed_subset_2023$profession_summary) <- c("Employed", "Self-employed (Formal)", "Self-employed (Informal)", "No Data")
ggplot(anc_data_processed_subset_2023, aes(x = profession_summary, y = haemoglobin, fill = profession_summary)) +
  geom_boxplot() +
  labs(title = "Boxplot of Haemoglobin Levels by Profession",
       x = "Profession",
       y = "Haemoglobin Level (g/dL)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

#education and Hb
ggplot(anc_data_processed_subset_2023, aes(x = education_level_summary, y = haemoglobin, fill = education_level_summary)) +
  geom_boxplot() +
  labs(title = "Boxplot of Haemoglobin Levels by Education",
       x = "Education",
       y = "Haemoglobin Level (g/dL)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

#location and Hb
ggplot(anc_data_processed_subset_2023, aes(x = address_group, y = haemoglobin, fill = address_group)) +
  geom_boxplot() +
  labs(title = "Boxplot of Haemoglobin Levels by Distance",
       x = "Location(Distance)",
       y = "Haemoglobin Level (g/dL)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")


#marital status and Hb
ggplot(anc_data_processed_subset_2023, aes(x = marital_status, y = haemoglobin, fill = marital_status)) +
  geom_boxplot() +
  labs(title = "Boxplot of Haemoglobin Levels by Marital status",
       x = "Marital Status",
       y = "Haemoglobin Level (g/dL)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

#sickle cell and Hb
ggplot(anc_data_processed_subset_2023, aes(x = sickle_cell, y = haemoglobin, fill = sickle_cell)) +
  geom_boxplot() +
  labs(title = "Boxplot of Haemoglobin Levels by Sickle Cell Test result",
       x = "Sickle Cell Test Result",
       y = "Haemoglobin Level (g/dL)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")


#malaria and Hb
ggplot(anc_data_processed_subset_2023, aes(x = malaria, y = haemoglobin, fill = malaria)) +
  geom_boxplot() +
  labs(title = "Boxplot of Haemoglobin Levels by Malaria Test result",
       x = "Malaria Test Result",
       y = "Haemoglobin Level (g/dL)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")