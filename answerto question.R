
#####-------------------------------------------------------------------------------------------########
###Q22+Q34+Q35 Age answer
###data clean
anc_data_clean<-anc_data_processed_in_%>%
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

#boxplot Comparison of haemoglobin by Anemia Status
ggplot(anc_data_clean, aes(x = anaemia_status, y = haemoglobin, fill = anaemia_status)) +
  geom_boxplot() +
  labs(title = "Comparison of Haemoglobin by Anemia Status", 
       x = "Anemia Status", y = "haemoglobin") +
  scale_fill_manual(values = c("True" = "red", "False" = "blue")) +
  theme_minimal()


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

filtered_anaemic_data <- subset(anc_data_processed_in_, age> age_threshold& anaemia_status==TRUE)

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

ggplot(Haemoglobin_data, aes(x = "", y = haemoglobin)) +
  geom_violin(fill = "lightblue") +
  geom_boxplot(width = 0.1, fill = "orange", outlier.colour = "red", outlier.shape = 8) +
  labs(title = "Violin Plot of Haemoglobin Levels", y = "Hemoglobin Level") +
  theme_minimal()


##Violin plot-aneamic data
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
summary(logistic_model)
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
multinom_model <- multinom(anaemia_category ~ age, data = anc_data_clean)

summary(multinom_model)

z <- summary(multinom_model)$coefficients / summary(multinom_model)$standard.errors
p_values <- (1 - pnorm(abs(z), 0, 1)) * 2
print(p_values)


# ANOVA test
anova_result <- aov(age ~ anaemia_category, data = anc_data_clean)
summary(anova_result)

TukeyHSD(anova_result)

# Kruskal-Wallis test
kruskal.test(age ~ anaemia_category, data = anc_data_clean)


# plot
ggplot(anc_data_clean, aes(x = anaemia_category, y = age, fill = anaemia_category)) +
  geom_boxplot() +
  labs(title = "Hemoglobin Levels by Anemia Category",
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
summary(logit_model)

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
summary(logit_model)

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
summary(logit_model)
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
summary(logit_model)

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

correlation <- biserial(anc_data_clean$haemoglobin, anc_data_clean$sickle_cell == "positive")
print(paste("binary test:", correlation))

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
summary(logit_model)
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

correlation <- biserial(anc_data_clean$haemoglobin, anc_data_clean$malaria == "negative")
print(paste("binary test:", correlation))

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



