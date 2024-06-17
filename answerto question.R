
#####-------------------------------------------------------------------------------------------########
###Q22 Age answer
###data clean
anc_data_processed_in_%>%
  dplyr::filter(age > 1)

##Anaemia status yes and no, mild,moderate and severe
detect_anaemia_pregnant(hb = anc_data_processed_in_$haemoglobin * 10)
anc_data_processed_in_$anaemia<-detect_anaemia_pregnant(hb = anc_data_processed_in_$haemoglobin * 10)
detect_anemia1 <- function(hemoglobin_levels) {
  anemia_threshold <- 11
  is_anemic <- hemoglobin_levels < anemia_threshold
  return(is_anemic)
}
anc_data_processed_in_$anaemia_status <- detect_anemia1(anc_data_processed_in_$haemoglobin)

anc_data_processed_in_$anaemia_status<-c(detect_anemia1(anc_data_processed_in_$haemoglobin))

##Age variable ---total 
# definition of sum of age function
sum_of_ages <- function(anc_data_processed_in_) {
  # "age"
  total_age <- sum(anc_data_processed_in_$age, na.rm = TRUE)  
  return(total_age)
}

# total age
total_age <- sum_of_ages(anc_data_processed_in_)

# sum age 5860


##max 47 min 1 mean 26.88073 median 26 first_quartile 25% 21  third_quartile 75%32 


# statistic

age_statistics <- function(anc_data_processed_in_) {
  max_age <- max(anc_data_processed_in_$age, na.rm = TRUE)          
  min_age <- min(anc_data_processed_in_$age, na.rm = TRUE)                
  mean_age <- mean(anc_data_processed_in_$age, na.rm = TRUE)
  median_age <- median(anc_data_processed_in_$age, na.rm = TRUE)    
  quartiles <- quantile(anc_data_processed_in_$age, probs = c(0.25, 0.75), na.rm = TRUE) 
  
  return(list(
    max = max_age,
    min = min_age,
    mean = mean_age,
    median = median_age,
    first_quartile = quartiles[1],
    third_quartile = quartiles[2]
  ))}

##Age Histogram
ggplot(anc_data_processed_in_, aes(x = age)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of age ", x = "Age", y = "number") +
  theme_minimal()


# Age Boxplot
ggplot(anc_data_processed_in_, aes(x = "", y =age, fill = age)) +
  geom_boxplot(outlier.colour = "orange", fill = "orange", outlier.shape = 8) +
  labs(title = "Boxplot of Age ", x = "", y = "Age") +
  theme_minimal()


##calculate anaemic and non anaemic age 
group_stats <- anc_data_processed_in_ %>%
  group_by(anaemia_status) %>%
  summarise(
    max = max(age, na.rm = TRUE),
    min = min(age, na.rm = TRUE),
    mean = mean(age, na.rm = TRUE),
    median = median(age, na.rm = TRUE),
    first_quartile = quantile(age, probs = 0.25, na.rm = TRUE),
    third_quartile = quantile(age, probs = 0.75, na.rm = TRUE)
  )


#######------------------------------------------------------------------------------------------#####


### Q23
##profession
#number of all profession
occupation_stats <- anc_data_processed_in_ %>%
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
  labs(title = "Proportion of Occupations", x = "", y = "Percentage") +
  theme_minimal() +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), position = position_stack(vjust = 0.5))

#####------------------------------------------------------------------------------------#####
##Q24
##education level
#number of all education level
educationlevel <- anc_data_processed_in_ %>%
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

###---------------------------------------------------------------------------------------------##
##Q25
##marriage status
#number of all marriage
marriage <- anc_data_processed_in_ %>%
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
###-----------------------------------------------------------------------------------------------------###
###Q26
##Address
#number of all address
address_stats <- anc_data_processed_in_ %>%
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

##---------------------------------------------------------------------------------------------------###
#Q27
##sickle cell
#number of all sickle cell
sickle_cell_stats <- anc_data_processed_in_ %>%
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


####------------------------------------------------------------------------------------------------##

##Q28
##malaria

malaria_stats <- anc_data_processed_in_ %>%
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

age_threshold <- 1

filtered_anaemic_data <- subset(anc_data_processed_in_, age> age_threshold& anaemia_status==TRUE)

write.csv(filtered_anaemic_data, "filtered_anaemic_data.csv", row.names = FALSE)

# statistic

age_statistics <- function(filtered_anaemic_data) {
  max_age <- max(filtered_anaemic_data$age, na.rm = TRUE)          
  min_age <- min(filtered_anaemic_data$age, na.rm = TRUE)                
  mean_age <- mean(filtered_anaemic_data$age, na.rm = TRUE)
  median_age <- median(filtered_anaemic_data$age, na.rm = TRUE)    
  quartiles <- quantile(filtered_anaemic_data$age, probs = c(0.25, 0.75), na.rm = TRUE) 
  
  return(list(
    max = max_age,
    min = min_age,
    mean = mean_age,
    median = median_age,
    first_quartile = quartiles[1],
    third_quartile = quartiles[2]
  ))}


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
  labs(title = "Proportion of Occupations", x = "", y = "Percentage") +
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
kable(anaemiceducationlevel, format = "markdown", col.names = c("education level", "number", "%"))

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
kable(marriage, format = "markdown", col.names = c("marriage", "number", "%"))

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




###Hb


haemoglobin_statistics <- function(anc_data_processed_in_) {
  max_Hb <- max(anc_data_processed_in_$haemoglobin, na.rm = TRUE)          
  min_Hb <- min(anc_data_processed_in_$haemoglobin, na.rm = TRUE)                
  mean_Hb <- mean(anc_data_processed_in_$haemoglobin, na.rm = TRUE)
  median_Hb <- median(anc_data_processed_in_$haemoglobin, na.rm = TRUE)    
  quartiles <- quantile(anc_data_processed_in_$haemoglobin, probs = c(0.25, 0.75), na.rm = TRUE) 
  
  return(list(
    max = max_Hb,
    min = min_Hb,
    mean = mean_Hb,
    median = median_Hb,
    first_quartile = quartiles[1],
    third_quartile = quartiles[2]
  ))}


# Anaemic

anc_data_processed_in_%>%
  dplyr::filter(age > 1)


anemia_data <- anc_data_processed_in_[anc_data_processed_in_$anaemia_status == TRUE, ]


haemoglobin_levels <- anemia_data$haemoglobin

median_haemoglobin <- median(haemoglobin_levels, na.rm = TRUE)

quartiles <- quantile(haemoglobin_levels, probs = c(0.25, 0.5, 0.75))


# five number summary
summary_stats <- summary(hemoglobin_levels)




# Histogram
ggplot(anc_data_processed_in_, aes(x = haemoglobin)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Hemoglobin Levels ", x = "Hemoglobin Level", y = "Frequency") +
  theme_minimal()


#Anaemic women
anemia_data <- anc_data_processed_in_[anc_data_processed_in_$anaemia_status == TRUE, ]

ggplot(anemia_data, aes(x = haemoglobin)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Hemoglobin Levels for Anaemic Pregnant Women", x = "Hemoglobin Level", y = "Frequency") +
  theme_minimal()


# Boxplot
ggplot(Haemoglobin_data, aes(x = "", y = haemoglobin, fill = haemoglobin)) +
  geom_boxplot(outlier.colour = "orange", outlier.shape = 8) +
  labs(title = "Boxplot of Hemoglobin Levels ", x = "", y = "Hemoglobin Level") +
  theme_minimal()

#Anaemic women
anemia_data <- anc_data_processed_in_[anc_data_processed_in_$anaemia_status == TRUE, ]
ggplot(anemia_data, aes(x = anaemia_status, y = haemoglobin, fill = anaemia_status)) +
  geom_boxplot(outlier.colour = "orange", outlier.shape = 8) +
  labs(title = "Boxplot of Hemoglobin Levels for Anemic Pregnant Women", x = "Anemia Status", y = "Hemoglobin Level") +
  theme_minimal()






# Violin Plot

ggplot(Haemoglobin_data, aes(x = "", y = haemoglobin)) +
  geom_violin(fill = "lightblue") +
  geom_boxplot(width = 0.1, fill = "orange", outlier.colour = "red", outlier.shape = 8) +
  labs(title = "Violin Plot of Hemoglobin Levels", y = "Hemoglobin Level") +
  theme_minimal()


##Violin plot-aneamic data
anaemia_data <- anc_data_processed_in_[anc_data_processed_in_$anaemia_status == TRUE, ]
ggplot(anaemia_data, aes(x = anaemia_status, y = haemoglobin, fill = anaemia_status)) +
  geom_violin() +
  geom_boxplot(width = 0.1, outlier.colour = "red", outlier.shape = 8) +
  labs(title = "Violin Plot of Hemoglobin Levels for Anaemic Pregnant Women", x = "Anaemia Status", y = "Hemoglobin Level") +
  theme_minimal()

###age and Hb
# Descriptive statistics of Hb values by age
print(anc_data_processed_in_$haemoglobin, group = anc_data_processed_in_$age)

# Assuming Hb_status is a categorical variable
table(anc_data_processed_in_$haemoglobin, anc_data_processed_in_$age)

# Proportion table for more insight
prop.table(table(anc_data_processed_in_$haemoglobin, anc_data_processed_in_$age), 2)


system('git remote remove origin')
system('git remote add origin https://github.com/OxfordIHTM/gh-anaemia-pregnancy')

getOption("repos")



###------------------------------------------------------------------------------------------------------###
#Q37
##Relationship between Haemoglobin and age
correlation <- cor(anc_data_processed_in_$age, anc_data_processed_in_$haemoglobin, method = "pearson")

plot <- ggplot(anc_data_processed_in_, aes(x = age, y = haemoglobin)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +  
  labs(title = "relationship between Haemoglobin and age",
       x = "age",
       y = "haemoglobin")

print(plot)

# Relationship between anaemic status and age
correlation <- cor(anc_data_processed_in_$age, anc_data_processed_in_$anaemia_status, method = "pearson")
print(paste("binary relationship:", correlation))

# logical regression
logistic_model <- glm(anaemia_status ~ age, data =  anc_data_processed_in_, family = binomial)
summary(logistic_model)

anc_data_processed_in_$Predicted <- predict(logistic_model, type = "response")

plot <- ggplot(anc_data_processed_in_, aes(x = age, y = anaemia_status)) +
  geom_point() +  
  geom_line(aes(y = Predicted), color = "blue") +  
  labs(title = "anaemic status and age",
       x = "age",
       y = "anaemic status")

print(plot)

##relationship between different anamia and Hb
multinom_model <- multinom(anaemia ~ age, data = anc_data_processed_in_)

summary(multinom_model)

z <- summary(multinom_model)$coefficients / summary(multinom_model)$standard.errors
p_values <- (1 - pnorm(abs(z), 0, 1)) * 2
print(p_values)



# plot
plot <- ggplot(anc_data_processed_in_, aes(x = age, y = anaemia)) +
  geom_jitter(width = 0.2, height = 0.1) + 
  labs(title = "the relationship between differentlevel anaemia and age",
       x = "age",
       y = "anaemia level")

print(plot)

##-------------------------------------------------------------------------------##
#Q38
##Relationship between Haemoglobin and profession
boxplot_Hb_profession <- ggplot(anc_data_processed_in_, aes(x = profession, y = haemoglobin)) +
  geom_boxplot() +
  labs(title = "profession and haemoglobin",
       x = "profession",
       y = "haemoglobin")

print(boxplot_Hb_profession)
# ANOVA
anova_result <- aov(haemoglobin ~ profession, data = anc_data_processed_in_)
summary(anova_result)



# Relationship between anaemic status and profession
# table
table_anaemiastatus_profession <- table(anc_data_processed_in_$anaemia_status, anc_data_processed_in_$profession)
print(table_anaemiastatus_profession)

# chi square test
chi_square_test <- chisq.test(table_anaemiastatus_profession)
print(chi_square_test)



##relationship between different anaemia and profession
# table
table_anaemia_profession <- table(anc_data_processed_in_$anaemia, anc_data_processed_in_$profession)
print(table_anaemia_profession)

# chi square
chi_square_test <- chisq.test(table_anaemia_profession)
print(chi_square_test)



##-------------------------------------------------------------------------------##
#Q39
##Relationship between Haemoglobin and education level
boxplot_Hb_educationlevel <- ggplot(anc_data_processed_in_, aes(x = education_level, y = haemoglobin)) +
  geom_boxplot() +
  labs(title = "education level and haemoglobin",
       x = "education level",
       y = "haemoglobin")
print(boxplot_Hb_educationlevel)

# ANOVA
anova_Hb_educationlevel_result <- aov(haemoglobin ~ education_level, data = anc_data_processed_in_)
summary(anova_Hb_educationlevel_result)



# Relationship between anaemia status and education level
# table
table_anaemiastatus_educationlevel <- table(anc_data_processed_in_$anaemia_status, anc_data_processed_in_$education_level)
print(table_anaemiastatus_educationlevel)

# chi square test
chi_square_test <- chisq.test(table_anaemiastatus_educationlevel)
print(chi_square_test)



##relationship between different anaemia and profession
# table
table_anaemia_educationlevel <- table(anc_data_processed_in_$anaemia, anc_data_processed_in_$education_level)
print(table_anaemia_educationlevel)

# chi square
chi_square_test <- chisq.test(table_anaemia_educationlevel)
print(chi_square_test)

##-------------------------------------------------------------------------------##
#Q40
##Relationship between haemoglobin and marital status
boxplot_Hb_maritalstatus <- ggplot(anc_data_processed_in_, aes(x = marital_status, y = haemoglobin)) +
  geom_boxplot() +
  labs(title = "marital status and haemoglobin",
       x = "marital status",
       y = "haemoglobin")
print(boxplot_Hb_maritalstatus)

# ANOVA
anova_Hb_maritalstatus_result <- aov(haemoglobin ~ marital_status, data = anc_data_processed_in_)
summary(anova_Hb_maritalstatus_result)



# Relationship between anaemia status and marital status
# table
table_anaemiastatus_marital_status <- table(anc_data_processed_in_$anaemia_status, anc_data_processed_in_$marital_status)
print(table_anaemiastatus_marital_status)

# chi square test
chi_square_test <- chisq.test(table_anaemiastatus_marital_status)
print(chi_square_test)



##relationship between different anaemia and marital status
# table
table_anaemia_maritalstatus <- table(anc_data_processed_in_$anaemia, anc_data_processed_in_$marital_status)
print(table_anaemia_maritalstatus)

# chi square
chi_square_test <- chisq.test(table_anaemia_maritalstatus)
print(chi_square_test)


###-----------------------------------------------------------------------------------------------------------------###
#Q41
##Relationship between haemoglobin and address
boxplot_Hb_address <- ggplot(anc_data_processed_in_, aes(x = address, y = haemoglobin)) +
  geom_boxplot() +
  labs(title = "address and haemoglobin",
       x = "address",
       y = "haemoglobin")
print(boxplot_Hb_address)

# ANOVA
anova_Hb_address_result <- aov(haemoglobin ~ address, data = anc_data_processed_in_)
summary(anova_Hb_address_result)



# Relationship between anaemia status and address
# table
table_anaemiastatus_address <- table(anc_data_processed_in_$anaemia_status, anc_data_processed_in_$address)
print(table_anaemiastatus_address)

# chi square test
chi_square_test <- chisq.test(table_anaemiastatus_address)
print(chi_square_test)



##relationship between different anaemia and profession
# table
table_anaemia_address <- table(anc_data_processed_in_$anaemia, anc_data_processed_in_$address)
print(table_anaemia_address)

# chi square
chi_square_test <- chisq.test(table_anaemia_address)
print(chi_square_test)


###-----------------------------------------------------------------------------------------------------------------###
#Q42
##Relationship between haemoglobin and sickle cell

correlation <- biserial(anc_data_processed_in_$haemoglobin, anc_data_processed_in_$sickle_cell == "positive")
print(paste("binary test:", correlation))

# t test
t_test_result <- t.test(haemoglobin ~ sickle_cell, data = anc_data_processed_in_)
print(t_test_result)
# box plot
boxplot <- ggplot(anc_data_processed_in_, aes(x = sickle_cell, y = haemoglobin)) +
  geom_boxplot() +
  labs(title = "relationship between sickle cell and haemoglobin",
       x = "sickle cell test result",
       y = "haemoglobin")

print(boxplot)


# ANOVA
anova_Hb_sicklecell_result <- aov(haemoglobin ~ sickle_cell, data = anc_data_processed_in_)
summary(anova_Hb_sicklecell_result)



# Relationship between anaemia status and sickle cell
# table
table_anaemiastatus_sickle <- table(anc_data_processed_in_$anaemia_status, anc_data_processed_in_$sickle_cell)
print(table_anaemiastatus_sickle)

# chi square test
chi_square_test <- chisq.test(table_anaemiastatus_sickle)
print(chi_square_test)



##relationship between different anaemia and sickle cell
# table
table_anaemia_sickle <- table(anc_data_processed_in_$anaemia, anc_data_processed_in_$sickle_cell)
print(table_anaemia_sickle)

# chi square
chi_square_test <- chisq.test(table_anaemia_sickle)
print(chi_square_test)
###----------------------------------------------------------------------------------------------------------
#Q43
##Relationship between haemoglobin and malaria

correlation <- biserial(anc_data_processed_in_$haemoglobin, anc_data_processed_in_$malaria == "negative")
print(paste("binary test:", correlation))

# t test????--->do not know how to do 
t_test_result <- t.test(haemoglobin ~ malaria, data = anc_data_processed_in_)
print(t_test_result)
# box plot
boxplot <- ggplot(anc_data_processed_in_, aes(x = malaria, y = haemoglobin)) +
  geom_boxplot() +
  labs(title = "relationship between malaria and haemoglobin",
       x = "malaria test result",
       y = "haemoglobin")

print(boxplot)






# Relationship between anaemia status and malaria
# table
table_anaemiastatus_malaria <- table(anc_data_processed_in_$anaemia_status, anc_data_processed_in_$malaria)
print(table_anaemiastatus_malaria)

# chi square test
chi_square_test <- chisq.test(table_anaemiastatus_malaria)
print(chi_square_test)



##relationship between different anaemia and sickle cell
# table
table_anaemia_malaria <- table(anc_data_processed_in_$anaemia, anc_data_processed_in_$malaria)
print(table_anaemia_malaria)

# chi square
chi_square_test <- chisq.test(table_anaemia_malaria)
print(chi_square_test)



