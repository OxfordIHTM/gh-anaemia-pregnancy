
#####-------------------------------------------------------------------------------------------########

source(here::here("packages.R"))

anc_data_processed_subset <- anc_data_processed %>%
  filter(age != 1) %>%
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
    profession_group= case_when(
      profession %in% c( "Teacher", "Company Employee", "Midwife") ~ "Employed",
      profession %in% c("Undertaker", "Trader", "Seamstress", "Hair Dresser", "Fishmonger", "Farmer", "Decorator", "Caterer", "Business Owner","Student") ~ "Self-Employed",
      TRUE ~ "NA"
    )
  )

###Q22+Q34+Q35 Age answer
###data clean
anc_data_clean<-read.csv(here::here("data/anc_data_processed.csv"))%>% 
  dplyr::filter(age > 1)


##Anaemia status yes and no, mild,moderate and severe
detect_anaemia_pregnant(hb = anc_data_clean$haemoglobin * 10)



anc_data_clean$anaemia_category<-detect_anaemia_pregnant(hb = anc_data_clean$haemoglobin * 10)
detect_anaemia <- function(haemoglobin_levels) {
  anaemia_threshold <- 11
  is_anaemic <- haemoglobin_levels < anaemia_threshold
  return(is_anaemic)
}


anc_data_clean$anaemia_status <- detect_anaemia(anc_data_clean$haemoglobin)

anc_data_clean$anaemia_status<-c(detect_anaemia(anc_data_clean$haemoglobin))


#table of anaemia staus
anaemia_table <- table(anc_data_clean$anaemia_status)
anaemia_proportion <- prop.table(anaemia_table)
anaemia_summary <- data.frame(
  Status = names(anaemia_table),
  Count = as.integer(anaemia_table),
  Proportion = round(anaemia_proportion, 2)
)
print(anaemia_summary)

#table of anaemia catergory
anaemiac_table <- table(anc_data_clean$anaemia_category)
anaemiac_proportion <- prop.table(anaemiac_table)
anaemiac_summary <- data.frame(
  Status = names(anaemiac_table),
  Count = as.integer(anaemiac_table),
  Proportion = round(anaemiac_proportion, 2)
)
print(anaemiac_summary)



#boxplot Comparison of haemoglobin by Anemia Status
ggplot(anc_data_clean, aes(x = anaemia_status, y = haemoglobin, fill = anaemia_status)) +
  geom_boxplot() +
  labs(title = "Comparison of Haemoglobin by Anemia Status", 
       x = "Anemia Status", y = "haemoglobin") +
  scale_fill_manual(values = c("True" = "red", "False" = "blue")) +
  theme_minimal()



# statistic Hb  five number summary 
fivenum(anc_data_clean$haemoglobin)

# statistic Hb  five number summary 
fivenum(filtered_anaemic_data$haemoglobin)

##Age variable ---total 

# statistic age  five number summary 
fivenum(anc_data_clean$age)

##Age Histogram
ggplot(anc_data_clean, aes(x = age)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of age ", x = "Age", y = "number") +
  theme_minimal()

##Age Histogram in one picture  
ggplot(anc_data_clean, aes(x = age, fill = anaemia_status)) +
  geom_histogram(position = "identity", alpha = 0.7, binwidth = 5,fill="blue") +
  facet_wrap(~ anaemia_status, ncol = 1) +
  labs(title = "Histogram of Age by Anemia Status", 
       x = "Age", y = "Count") +
  scale_fill_manual(values = c("Yes" = "lightcoral", "No" = "lightblue")) +
  theme_minimal()

# Age Boxplot
ggplot(anc_data_clean, aes(x = "", y =age, fill = age)) +
  geom_boxplot(outlier.colour = "orange", fill = "orange", outlier.shape = 8) +
  labs(title = "Boxplot of Age ", x = "", y = "Age") +
  theme_minimal()

##five number summary in anaemic 
fivenum(filtered_anaemic_data$age)


# histogram of age
ggplot(data = anc_data_clean, mapping = aes(x = age, fill = anaemia_status)) +
  geom_bar(position = "dodge")


#histogram of profession

ggplot(data = anc_data_clean, mapping = aes(x = profession, fill = anaemia_status)) +
  geom_bar(position = "dodge")

# histogram of education level
ggplot(data = anc_data_clean, mapping = aes(x = education_level, fill = anaemia_status)) +
  geom_bar(position = "dodge")


# histogram of sickle cell
ggplot(data = anc_data_clean, mapping = aes(x = sickle_cell, fill = anaemia_status)) +
  geom_bar(position = "dodge")


# histogram of marital status
ggplot(data = anc_data_clean, mapping = aes(x = marital_status, fill = anaemia_status)) +
  geom_bar(position = "dodge")


#histogram
ggplot(anc_data_clean, aes(x = age, fill = anaemia_status)) +
  geom_histogram(position = "identity", alpha = 0.7, binwidth = 5) +
  facet_wrap(~ anaemia_status, ncol = 1) +
  labs(title = "Histogram of Age by Anemia Status", x = "Age", y = "Count") +
  scale_fill_manual(values = c("Yes" = "lightcoral", "No" = "lightblue")) +
  theme_minimal()

#######------------------------------------------------------------------------------------------#####


### Q23
##profession
#number of all profession
occupation_stats <- anc_data_clean %>%
  group_by(profession) %>%
  summarise(
    count = n()  
  )
# percentage
total_count <- sum(occupation_stats$count)
occupation_stats <- occupation_stats %>%
  mutate(percentage = count / total_count * 100)

#table
kable(occupation_stats, format = "markdown", col.names = c("profession", "number", "%"))
##pie chart
ggplot(occupation_stats, aes(x = "", y = percentage, fill = profession)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Proportion of profession", x = "", y = "Percentage") +
  theme_minimal() +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), position = position_stack(vjust = 0.5))

#####------------------------------------------------------------------------------------#####
##Q24
##education level
#number of all education level
educationlevel <- anc_data_clean %>%
  group_by(education_level) %>%
  summarise(
    count = n()  
  )
# percentage
total_count <- sum(educationlevel$count)
educationlevel <- educationlevel %>%
  mutate(percentage = count / total_count * 100)

#table
kable(educationlevel, format = "markdown", col.names = c("education level", "number", "%"))

##linear regression of education level and haemoglobin
model <- lm(haemoglobin ~ education_level, data = anc_data_clean)
summary(model)

###---------------------------------------------------------------------------------------------##
##Q25
##marriage status
#number of all marriage
marriage <- anc_data_clean %>%
  group_by(marital_status) %>%
  summarise(
    count = n()  
  )
# percentage
total_count <- sum(marriage$count)
marriage <- marriage %>%
  mutate(percentage = count / total_count * 100)

#table
kable(marriage, format = "markdown", col.names = c("marriage", "number", "%"))

##linear regression of marital status and haemoglobin
model <- lm(haemoglobin ~ marital_status, data = anc_data_clean)
summary(model)


###-----------------------------------------------------------------------------------------------------###
###Q26
##Address
#number of all address
address_stats <- anc_data_clean %>%
  group_by(address) %>%
  summarise(
    count = n()  
  )
# percentage
total_count <- sum(address_stats$count)
address_stats <- address_stats%>%
  mutate(percentage = count / total_count * 100)

#table
kable(address_stats, format = "markdown", col.names = c("address", "number", "%"))

##linear regression of address and haemoglobin
model <- lm(haemoglobin ~ address, data = anc_data_clean)
summary(model)

##---------------------------------------------------------------------------------------------------###
#Q27
##sickle cell
#number of all sickle cell
sickle_cell_stats <- anc_data_clean %>%
  group_by(sickle_cell) %>%
  summarise(
    count = n()  
  )
# percentage
total_count <- sum(sickle_cell_stats$count)
sickle_cell_stats <- sickle_cell_stats %>%
  mutate(percentage = count / total_count * 100)

#table
kable(sickle_cell_stats, format = "markdown", col.names = c("sickle cell", "number", "%"))


##linear regression of sickle cell and haemoglobin
model <- lm(haemoglobin ~ sickle_cell, data = anc_data_clean)
summary(model)


####------------------------------------------------------------------------------------------------##

##Q28
##malaria

malaria_stats <- anc_data_clean %>%
  group_by(malaria) %>%
  summarise(
    count = n()  
  )
# percentage
total_count <- sum(malaria_stats$count)
malaria_stats <- malaria_stats %>%
  mutate(percentage = count / total_count * 100)

#table
kable(malaria_stats, format = "markdown", col.names = c("malaria", "number", "%"))



###----------------------------------------------------------------------------------------------###

##Anaemic descriptive data
anc_data_processed_in_%>%
  dplyr::filter(age > 1)

age_threshold <- 1

filtered_anaemic_data <- subset(anc_data_processed_in_, age> age_threshold&haemoglobin< 11)

write.csv(filtered_anaemic_data, "filtered_anaemic_data.csv", row.names = FALSE)

# statistic
fivenum(filtered_anaemic_data$age)



##anaemic women Age Histogram
ggplot(filtered_anaemic_data, aes(x = age)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of age ", x = "Age", y = "number") +
  theme_minimal()


## Anaemic women  Age Boxplot
ggplot(filtered_anaemic_data, aes(x = "", y =age, fill = age)) +
  geom_boxplot(outlier.colour = "orange", fill = "orange", outlier.shape = 8) +
  labs(title = "Boxplot of Age ", x = "", y = "Age") +
  theme_minimal()




##profession
#number of all profession
occupation_anaemic_stats <-filtered_anaemic_data %>%
  group_by(profession) %>%
  summarise(
    count = n()  
  )
# percentage
total_count <- sum(occupation_anaemic_stats$count)
occupation_anaemic_stats <- occupation_stats %>%
  mutate(percentage = count / total_count * 100)

#table
kable(occupation_anaemic_stats, format = "markdown", col.names = c("profession", "number", "%"))
##pie chart
ggplot(occupation_stats, aes(x = "", y = percentage, fill = profession)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Proportion of profession of anaemic women", x = "", y = "Percentage") +
  theme_minimal() +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), position = position_stack(vjust = 0.5))



##education level
#number of all education level
anaemiceducationlevel <- filtered_anaemic_data %>%
  group_by(education_level) %>%
  summarise(
    count = n()  
  )
# percentage
total_count <- sum(anaemiceducationlevel$count)
anaemiceducationlevel <- anaemiceducationlevel %>%
  mutate(percentage = count / total_count * 100)

#table
kable(anaemiceducationlevel, format = "markdown", col.names = c("education level of anaemic women", "number", "%"))

##marriage status
#number of all marriage
marriage <- filtered_anaemic_data %>%
  group_by(marital_status) %>%
  summarise(
    count = n()  
  )
# percentage
total_count <- sum(marriage$count)
marriage <- marriage %>%
  mutate(percentage = count / total_count * 100)

#table
kable(marriage, format = "markdown", col.names = c("marital status", "number", "%"))

##Address
#number of all address
address_stats <- filtered_anaemic_data %>%
  group_by(address) %>%
  summarise(
    count = n()  
  )
# percentage
total_count <- sum(address_stats$count)
address_stats <- address_stats%>%
  mutate(percentage = count / total_count * 100)

#table
kable(address_stats, format = "markdown", col.names = c("address", "number", "%"))


###------------------------------------------------------------------------------------------###

###Hb

#five summary
fivenum(anc_data_clean$haemoglobin)
#boxplot
boxplot(anc_data_clean$haemoglobin,main = "Box Plot of Hb among women", col.main = "blue",ylab="haemoglobin level")



# Anaemia

anc_data_processed_in_%>%
  dplyr::filter(age > 1)


anaemia_data <- anc_data_clean[anc_data_clean$anaemia_status == TRUE, ]
haemoglobin_anaemia <- anaemia_data$haemoglobin



# five number summary of anaemic women
fivenum(haemoglobin_anaemia)
boxplot(haemoglobin_anaemia, main = "Box Plot of Hb in anaemic women", col.main = "blue",ylab="haemoglobin level" )



# Histogram
ggplot(anc_data_clean, aes(x = haemoglobin)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Hemoglobin Levels ", x = "Hemoglobin Level", y = "Frequency") +
  theme_minimal()


#Anaemic women
anaemia_data <- anc_data_clean[anc_data_clean$anaemia_status == TRUE, ]

ggplot(anaemia_data, aes(x = haemoglobin)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Hemoglobin Levels for Anaemic Pregnant Women", x = "Hemoglobin Level", y = "Frequency") +
  theme_minimal()


# Boxplot
boxplot(anaemia_data$haemoglobin, main = "Box Plot of Hb in anaemic women", col.main = "blue",ylab="haemoglobin level")
ggplot(anaemia_data, aes(x = "", y = haemoglobin, fill = haemoglobin)) +
  geom_boxplot(outlier.colour = "orange", outlier.shape = 8) +
  labs(title = "Boxplot of Hemoglobin Levels in anaemic women ", x = "", y = "Hemoglobin Level") +
  theme_minimal()

anemia_data <- anc_data_clean[anc_data_clean$anaemia_status == TRUE, ]
ggplot(anemia_data, aes(x = anaemia_status, y = haemoglobin, fill = anaemia_status)) +
  geom_boxplot(outlier.colour = "orange", outlier.shape = 8) +
  labs(title = "Boxplot of Hemoglobin Levels for Anemic Pregnant Women", x = "Anemia Status", y = "Hemoglobin Level") +
  theme_minimal()






# Violin Plot

ggplot(anc_data_clean, aes(x = "", y = haemoglobin)) +
  geom_violin(fill = "lightblue") +
  geom_boxplot(width = 0.1, fill = "orange", outlier.colour = "red", outlier.shape = 8) +
  labs(title = "Violin Plot of Haemoglobin Levels", y = "Hemoglobin Level") +
  theme_minimal()


##Violin plot-anaemic data
anaemia_data <- anc_data_clean[anc_data_clean$anaemia_status == TRUE, ]
ggplot(anaemia_data, aes(x = anaemia_status, y = haemoglobin, fill = anaemia_status)) +
  geom_violin(fill = "lightblue") +
  geom_boxplot(width = 0.1,fill = "orange", outlier.colour = "red", outlier.shape = 8) +
  labs(title = "Violin Plot of Haemoglobin Levels for Anaemic Pregnant Women", x = "Anaemia Status", y = "Hemoglobin Level") +
  theme_minimal()





###------------------------------------------------------------------------------------------------------###
#Q37
##Relationship between Haemoglobin and age
correlation <- cor(anc_data_clean$age, anc_data_clean$haemoglobin, method = "pearson")

##correlation NA
cor(anc_data_clean$age, anc_data_clean$haemoglobin)


#linear regression
summary(anc_data_clean$age,anc_data_clean$haemoglobin)
summary(anc_data_clean)
model <- lm(haemoglobin ~ age, data = anc_data_clean)
summary(model)


# Plotting the data and the regression line
ggplot(anc_data_clean, aes(x = age, y = haemoglobin)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") + 
  labs(title = "relationship between haemoglobin and Age",
       x = "Age",
       y = "Haemoglobin")

# Displaying the coefficients
coefficients(model)
# Diagnostic plots to check for homoscedasticity, normality, etc.
par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
plot(model)
##histogram of residual
residuals <- residuals(model)
hist(residuals, breaks = 30, main = "Histogram of Residuals", xlab = "Residuals")


#dot plot
ggplot(anc_data_clean, aes(x = age, y = haemoglobin)) +
  geom_point(color = "black", size = 2) +
  geom_hline(yintercept = 11, linetype = "dashed", color = "blue", size = 1) +  
  geom_vline(xintercept = 30, linetype = "dashed", color = "red", size = 1) +  
  labs(title = "Scatter Plot of Age and Haemoglobin Levels", x = "Age (years)", y = "Haemoglobin (g/dL)") +
  theme_minimal()




## Relationship between anaemic status and age
correlation <- cor(anc_data_clean$age, anc_data_clean$anaemia_status, method = "pearson")
print(paste("binary relationship:", correlation))
#t test
t.test(age ~ anaemia_status, data = anc_data_clean)
#chisquare test
table_anaemicstatus_data <- table(anc_data_clean$age, anc_data_clean$anaemia_status)
chisq.test(table_anaemicstatus_data)
#logical regression
logistic_age_anaemia_model <- glm(anaemia_status ~ age, data = anc_data_clean, family = binomial)
summary(logistic_age_anaemia_model)
coefficients(logistic_age_anaemia_model)

##correlation analysis ->0.1336585
cor(filtered_anaemic_data$age, filtered_anaemic_data$haemoglobin)

#boxplot Comparison of Age by Anemia Status
ggplot(anc_data_clean, aes(x = anaemia_status, y = age, fill = anaemia_status)) +
  geom_boxplot() +
  labs(title = "Comparison of Age by Anemia Status", 
       x = "Anemia Status", y = "Age") +
  scale_fill_manual(values = c("True" = "red", "False" = "blue")) +
  theme_minimal()







####???
# Predict probabilities
predicted_prob_anaemicstatus <- predict(logistic_age_anaemia_model, type = "response")

# Create the plot
ggplot(anc_data_clean, aes(x = age, y = anaemia_status)) +
  geom_point(alpha = 0.5) +  # Scatter plot of the data
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) + # Logistic regression line
  geom_line(color="blue")
labs(title = "Logistic Regression Line",
     x = "Age",
     y = "Probability of Anemia") +
  theme_minimal()





##relationship between different anaemia category and age



# ANOVA test
anova_result <- aov(age ~ anaemia_category, data = anc_data_clean)
summary(anova_result)

TukeyHSD(anova_result)

# Kruskal-Wallis test
kruskal.test(age ~ anaemia_category, data = anc_data_clean)


# plot
ggplot(anc_data_clean, aes(x = anaemia_category, y = age, fill = anaemia_category)) +
  geom_boxplot() +
  labs(title = "age by Anemia Category",
       x = "Anemia Category",
       y = "Age") +
  theme_minimal()

plot <- ggplot(anc_data_clean, aes(x = age, y = anaemia_category)) +
  geom_jitter(width = 0.2, height = 0.1) + 
  labs(title = "the relationship between anaemia category and age",
       x = "age",
       y = "anaemia category")

print(plot)

##-------------------------------------------------------------------------------##
#Q38
##Relationship between Haemoglobin and profession
boxplot_Hb_profession <- ggplot(anc_data_clean, aes(x = profession, y = haemoglobin)) +
  geom_boxplot() +
  labs(title = "profession and haemoglobin",
       x = "profession",
       y = "haemoglobin")

print(boxplot_Hb_profession)
# ANOVA
anova_result <- aov(haemoglobin ~ profession, data = anc_data_clean)
summary(anova_result)







# Relationship between anaemic status and profession
# table
table_anaemiastatus_profession <- table(anc_data_clean$anaemia_status, anc_data_clean$profession)
print(table_anaemiastatus_profession)

# chi square test
chi_square_test <- chisq.test(table_anaemiastatus_profession)
print(chi_square_test)
# logic model
logit_anaemia_profession_model <- glm(anaemia_status ~ profession, data = anc_data_clean, family = "binomial")
summary(logit_anaemia_profession_model)

coefficients(logit_anaemia_profession_model)







##relationship between different anaemia category and profession
# table
table_anaemiacategory_profession <- table(anc_data_clean$anaemia_category, anc_data_clean$profession)
print(table_anaemiacategory_profession)

# chi square
chi_square_test <- chisq.test(table_anaemiacategory_profession)
print(chi_square_test)



##-------------------------------------------------------------------------------##
#Q39
##Relationship between Haemoglobin and education level
boxplot_Hb_educationlevel <- ggplot(anc_data_clean, aes(x = education_level, y = haemoglobin)) +
  geom_boxplot() +
  labs(title = "education level and haemoglobin",
       x = "education level",
       y = "haemoglobin")
print(boxplot_Hb_educationlevel)

# ANOVA
anova_Hb_educationlevel_result <- aov(haemoglobin ~ education_level, data = anc_data_clean)
summary(anova_Hb_educationlevel_result)



# Relationship between anaemia status and education level
# table
table_anaemiastatus_educationlevel <- table(anc_data_clean$anaemia_status, anc_data_clean$education_level)
print(table_anaemiastatus_educationlevel)

# chi square test
chi_square_test <- chisq.test(table_anaemiastatus_educationlevel)
print(chi_square_test)
# logic model
logit_anaemia_education_model <- glm(anaemia_status ~ education_level, data = anc_data_clean, family = "binomial")
summary(logit_anaemia_education_model)

coefficients(logit_anaemia_education_model)



##relationship between anaemia category and education level
# table
table_anaemiacategory_educationlevel <- table(anc_data_clean$anaemia_category, anc_data_clean$education_level)
print(table_anaemiacategory_educationlevel)

# chi square
chi_square_test <- chisq.test(table_anaemiacategory_educationlevel)
print(chi_square_test)



##-------------------------------------------------------------------------------##
#Q40
##Relationship between haemoglobin and marital status
boxplot_Hb_maritalstatus <- ggplot(anc_data_clean, aes(x = marital_status, y = haemoglobin)) +
  geom_boxplot() +
  labs(title = "marital status and haemoglobin",
       x = "marital status",
       y = "haemoglobin")
print(boxplot_Hb_maritalstatus)

# ANOVA
anova_Hb_maritalstatus_result <- aov(haemoglobin ~ marital_status, data = anc_data_clean)
summary(anova_Hb_maritalstatus_result)



# Relationship between anaemia status and marital status
# table
table_anaemiastatus_marital_status <- table(anc_data_clean$anaemia_status, anc_data_clean$marital_status)
print(table_anaemiastatus_marital_status)

# chi square test
chi_square_test <- chisq.test(table_anaemiastatus_marital_status)
print(chi_square_test)
# logic model
logit_anaemia_marital_model <- glm(anaemia_status ~ marital_status, data = anc_data_clean, family = "binomial")
summary(logit_anaemia_marital_model)
coefficients(logit_anaemia_marital_model)


#boxplot Comparison of marital satus by Anemia Status
ggplot(anc_data_clean, aes(x = anaemia_status, y = marital_status, fill = anaemia_status)) +
  geom_boxplot() +
  labs(title = "Comparison of marital status by Anemia Status", 
       x = "Anemia Status", y = "marital status") +
  scale_fill_manual(values = c("Yes" = "orange", "No" = "lightblue")) +
  theme_minimal()



##relationship between anaemia category and marital status
# table
table_anaemiacategory_maritalstatus <- table(anc_data_clean$anaemia_category, anc_data_clean$marital_status)
print(table_anaemiacategory_maritalstatus)

# chi square
chi_square_test <- chisq.test(table_anaemiacategory_maritalstatus)
print(chi_square_test)


###-----------------------------------------------------------------------------------------------------------------###
#Q41
##Relationship between haemoglobin and address
boxplot_Hb_address <- ggplot(anc_data_clean, aes(x = address, y = haemoglobin)) +
  geom_boxplot() +
  labs(title = "address and haemoglobin",
       x = "address",
       y = "haemoglobin")
print(boxplot_Hb_address)

# ANOVA
anova_Hb_address_result <- aov(haemoglobin ~ address, data = anc_data_clean)
summary(anova_Hb_address_result)



# Relationship between anaemia status and address
# table
table_anaemiastatus_address <- table(anc_data_clean$anaemia_status, anc_data_clean$address)
print(table_anaemiastatus_address)

# chi square test
chi_square_test <- chisq.test(table_anaemiastatus_address)
print(chi_square_test)
# logic model
logit_anaemia_address_model <- glm(anaemia_status ~ address, data = anc_data_clean, family = "binomial")
summary(logit_anaemia_address_model)

coefficients(logit_anaemia_address_model)


##relationship between  anaemia category and profession
# table
table_anaemiacategory_address <- table(anc_data_clean$anaemia_category, anc_data_clean$address)
print(table_anaemiacategory_address)

# chi square
chi_square_test <- chisq.test(table_anaemiacategory_address)
print(chi_square_test)


###-----------------------------------------------------------------------------------------------------------------###
#Q42
##Relationship between haemoglobin and sickle cell
# t test
t_test_result <- t.test(haemoglobin ~ sickle_cell, data = anc_data_clean)
print(t_test_result)
# box plot
boxplot <- ggplot(anc_data_clean, aes(x = sickle_cell, y = haemoglobin)) +
  geom_boxplot() +
  labs(title = "relationship between sickle cell and haemoglobin",
       x = "sickle cell test result",
       y = "haemoglobin")

print(boxplot)


# ANOVA
anova_Hb_sicklecell_result <- aov(haemoglobin ~ sickle_cell, data = anc_data_clean)
summary(anova_Hb_sicklecell_result)



# Relationship between anaemia status and sickle cell
# table
table_anaemiastatus_sickle <- table(anc_data_clean$anaemia_status, anc_data_clean$sickle_cell)
print(table_anaemiastatus_sickle)

# chi square test
chi_square_test <- chisq.test(table_anaemiastatus_sickle)
print(chi_square_test)

# logic model
logit_anaemia_sickle_model <- glm(anaemia_status ~ sickle_cell, data = anc_data_clean, family = "binomial")
summary(logit_anaemia_sickle_model)
coefficients(logit_anaemia_sickle_model)





##relationship between anaemia category and sickle cell
# table
table_anaemiacategory_sickle <- table(anc_data_clean$anaemia_category, anc_data_clean$sickle_cell)
print(table_anaemiacategory_sickle)

# chi square
chi_square_test <- chisq.test(table_anaemiacategory_sickle)
print(chi_square_test)
###----------------------------------------------------------------------------------------------------------
#Q43
##Relationship between haemoglobin and malaria

# t test????--->do not know how to do 
t_test_result <- t.test(haemoglobin ~ malaria, data = anc_data_clean)
print(t_test_result)
# box plot
boxplot <- ggplot(anc_data_clean, aes(x = malaria, y = haemoglobin)) +
  geom_boxplot() +
  labs(title = "relationship between malaria and haemoglobin",
       x = "malaria test result",
       y = "haemoglobin")

print(boxplot)






# Relationship between anaemia status and malaria
# table
table_anaemiastatus_malaria <- table(anc_data_clean$anaemia_status, anc_data_clean$malaria)
print(table_anaemiastatus_malaria)


# chi square test
chi_square_test <- chisq.test(table_anaemiastatus_malaria)
print(chi_square_test)



##relationship between  anaemia category and malaria
# table
table_anaemiacategory_malaria <- table(anc_data_clean$anaemia_category, anc_data_clean$malaria)
print(table_anaemiacategory_malaria)

# chi square
chi_square_test <- chisq.test(table_anaemiacategory_malaria)
print(chi_square_test)



### multiple variable
model <- lm(haemoglobin ~ age + address + education_level + profession + marital_status +sickle_cell, data = anc_data_clean)

summary(model)
# 
par(mfrow = c(2, 2))
plot(model)

---------------------------------------------------------------------------------
#other variable 
# Relationship between education and profession
# table
  table_educationlevel_profession<-table(anc_data_clean$education_level, anc_data_clean$profession)
print(table_educationlevel_profession)

# chi square test
chi_square_test <- chisq.test(table_educationlevel_profession)
print(chi_square_test)




##Relationship between age and education level

ggplot(anc_data_clean, aes(x = age, fill = education_level)) +
  geom_histogram(binwidth = 2, position = "dodge") +
  facet_wrap(~education_level, scales = "free_y") +
  theme_minimal() +
  labs(title = "Age Distribution by Education Level", x = "Age", y = "Count")

# ANOVA
anova_age_educationlevel_result <- aov(age ~ education_level, data = anc_data_clean)
summary(anova_age_educationlevel_result)



###-----------------------------------------------------------------------------------------------------------------
##education Grouping
##group profession-employee and self employee
grouped_education_data <- anc_data_clean %>%
  mutate(education_Group = case_when(
    education_level %in% c( "Primary", "Junior High School") ~ "basic education",
    education_level %in% c("Senior High School", "Tertiary") ~ "advance education",
    TRUE ~ "NA"
  ))
summary_data <- grouped_education_data %>%
  group_by(education_Group) %>%
  summarize(mean_age = mean(age), count = n())

##Relationship between Haemoglobin and education group
boxplot_Hb_educationgroup<- ggplot(grouped_education_data, aes(x = education_Group, y = haemoglobin)) +
  geom_boxplot() +
  labs(title = "education group and haemoglobin",
       x = "profession",
       y = "haemoglobin")

print(boxplot_Hb_educationgroup)

# Relationship between anaemia status and group profession
# table
table_groupeducation <- table(grouped_education_data$anaemia_status, grouped_education_data$education_Group)
print(table_groupeducation)

# chi square test
chi_square_test <- chisq.test(table_groupeducation)
print(chi_square_test)

# logic model
logit_groupeducation<- glm(anaemia_status ~ education_Group, data = grouped_education_data, family = "binomial")
summary(logit_groupeducation)
coefficients(logit_groupeducation)
##-------------------------------------------------------------------------------------------------------------------


##profesion grouping
##group profession-employee and self employee
grouped_profession_data <- anc_data_clean %>%
  mutate(profession_Group = case_when(
    profession %in% c( "Teacher", "Company Employee", "Midwife") ~ "Employed",
    profession %in% c("Undertaker", "Trader", "Seamstress", "Hair Dresser", "Fishmonger", "Farmer", "Decorator", "Caterer","Business Owner", "Student") ~ "Self-Employed",
    TRUE ~ "NA"
  ))
summary_data <- grouped_profession_data %>%
  group_by(profession_Group) %>%
  summarize(mean_age = mean(age), count = n())

##Relationship between Haemoglobin and profession group
boxplot_Hb_professiongroup<- ggplot(grouped_profession_data, aes(x = profession_Group, y = haemoglobin)) +
  geom_boxplot() +
  labs(title = "profession group and haemoglobin",
       x = "profession",
       y = "haemoglobin")

print(boxplot_Hb_professiongroup)

# Relationship between anaemia status and group profession
# table
table_groupprofession <- table(grouped_profession_data$anaemia_status, grouped_profession_data$profession_Group)
print(table_groupprofession)

# chi square test
chi_square_test <- chisq.test(table_groupprofession)
print(chi_square_test)

# logic model
logit_groupprofession<- glm(anaemia_status ~ profession_Group, data = grouped_profession_data, family = "binomial")
summary(logit_groupprofession)
coefficients(logit_groupprofession)

##It looks like statistic significant in the profession group


##grouping by advance profession
grouped_ad_profession_data <- anc_data_clean %>%
  mutate(profession_ad_Group = case_when(
    profession %in% c( "Teacher", "Company Employee", "Midwife") ~ "Employed",
    profession %in% c("Trader",   "Fishmonger", "Farmer", "Caterer","Business Owner") ~ "Self-Employed",
    profession %in% c("Undertaker",  "Seamstress", "Hair Dresser", "Decorator") ~ "Self-Employed with vaocational training",
    profession %in% c( "none", "Student") ~ "None Income",
    TRUE ~ "NA"
  ))
summary_data <- grouped_ad_profession_data %>%
  group_by(profession_ad_Group) %>%
  summarize(mean_age = mean(age), count = n())

boxplot_Hb_adprofessiongroup<- ggplot(grouped_ad_profession_data, aes(x = profession_ad_Group, y = haemoglobin)) +
  geom_boxplot() +
  labs(title = "profession group and haemoglobin",
       x = "profession",
       y = "haemoglobin")

print(boxplot_Hb_adprofessiongroup)

# Relationship between anaemia status and group profession
# table
table_groupadprofession <- table(grouped_ad_profession_data$anaemia_status, grouped_ad_profession_data$profession_ad_Group)
print(table_groupprofession)

# chi square test
chi_square_test <- chisq.test(table_groupadprofession)
print(chi_square_test)

# logic model
logit_groupadprofession<- glm(anaemia_status ~ profession_ad_Group, data = grouped_ad_profession_data, family = "binomial")
summary(logit_groupadprofession)
coefficients(logit_groupadprofession)







###-----------------------------------------------------------------------------------------------------------###
#Age group
##Relationship between haemoglobin and age group

boxplot_Hb_adgegroup <- ggplot(anc_data_processed_subset, aes(x = age_group, y = haemoglobin)) +
  geom_boxplot() +
  labs(title = "age group and haemoglobin",
       x = "age group",
       y = "haemoglobin")
print(boxplot_Hb_adgegroup)



# ANOVA
anova_Hb_agegroup_result <- aov(haemoglobin ~ age_group, data = anc_data_processed_subset)
summary(anova_Hb_agegroup_result)



# Relationship between anaemia status and age group
# table
table_anaemiastatus_agegroup <- table(anc_data_processed_subset$anaemia_status, anc_data_processed_subset$age_group)
print(table_anaemiastatus_agegroup )

# chi square test
chi_square_test <- chisq.test(table_anaemiastatus_agegroup )
print(chi_square_test)

