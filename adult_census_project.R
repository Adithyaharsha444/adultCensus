install.packages("ggplot2")
install.packages("ggalluvial")
install.packages("ggrepel")

# Load data
adult_data <- read.csv("D:/Advance Data Analysis/ADA PROJECT/adult.csv", header = TRUE)

library(ggplot2)
library(ggalluvial)
View(adult_data)

install.packages("ggplot2")
library(ggplot2)
colnames(adult_data)
# Assign column names to the data
colnames(adult_data) <- c("age", "workclass", "fnlwgt", "education", "education_num",
                          "marital_status", "occupation", "relationship", "race", 
                          "sex", "capital_gain", "capital_loss", "hours_per_week",
                          "native_country", "income")

head(adult_data)

summary(adult_data)

# Replace "?" with missing values
adult_data[adult_data == "?"] <- NA
adult_data

# Identify missing values
missing_values <- sum(is.na(adult_data))
missing_values

# Drop rows with missing values
adult_data <- na.omit(adult_data)
adult_data

head(adult_data)
summary(adult_data)

# Create a histogram of the age variable
hist(adult_data$age)
library(ggplot2)

# Create plot of age vs count with sex using ggplot2
ggplot(adult_data, aes(x = age, fill = sex)) +
  geom_bar(data = subset(adult_data, sex == "Male"),
           aes(y = ..count..),
           stat = "bin",
           alpha = 0.8,
           position = "identity",
           color = "yellow",
           fill = "darkblue") +
  geom_bar(data = subset(adult_data, sex == "Female"),
           aes(y = ..count..),
           stat = "bin",
           alpha = 0.8,
           position = "identity",
           color = "snow",
           fill = "lightpink1") +
  labs(title = "Male and Female Participants by Age",
       x = "Age",
       y = "Count") +
  scale_fill_manual(values = c("#5e3c99", "#f5a742"),
                    name = "Sex",
                    labels = c("Female", "Male"))

#less female some outliers seen in age after ~80-85 
library(dplyr)


pairs.panels(adult_data)
install.packages("psych")
library(psych)

pairs.panels(adult_data)

install.packages("corrplot")
library(corrplot)
M = cor(appraisal)
corrplot(M, method = "ellipse")

pairs.panels(adult_data)



cor



# Create plot of income vs education with sex using ggplot2
ggplot(adult_data, aes(x = education_num, fill = income)) +
  geom_bar(data = subset(adult_data, income == ">50K"),
           aes(y = ..count..),
           stat = "bin",
           alpha = 0.8,
           position = "identity",
           color = "#FF5733",
           fill = "#3498db") +
  geom_bar(data = subset(adult_data, income == "<=50K"),
           aes(y = ..count..),
           stat = "bin",
           alpha = 0.8,
           position = "identity",
           color = "#2c3e50",
           fill = "#f39c12") +
  labs(title = "Income by Education and Sex",
       x = "Education (Num)",
       y = "Count") +
  scale_fill_manual(values = c("#900C3F", "#1B1464"),
                    name = "Income",
                    labels = c("<=50K", ">50K"))  +
  theme_minimal()


c("#B2DF8A", "#FDBF6F", "#B3CDE3", "#FCCDE5")
Jewel tones: c("#9b1c31", "#0c457d", "#689d70", "#f3bda1")
Earth tones: c("#E2C044", "#C45A3E", "#7D604C", "#4D4F4E")
Bold brights: c("#FF5733", "#900C3F", "#006266", "#1B1464")

adult_data$education

summary(adult_data)
library(ggplot2)
library(ggplot2)
install.packages("ggplot2")
ggplot(adult_data, aes(x = education, y = income)) + 
  geom_bar(stat = "identity", fill = "#69b3a2") +
  xlab("Education") +
  ylab("Income") +
  ggtitle("Mean Income by Education Level")
income
mean(income)
# create a summary table of education and income
education_income <- adult_data %>% group_by(education) %>% summarize(mean_income = mean(income))
education_income

# plot a bar chart of education and income
ggplot(adult_data, aes(x = education, y = income)) + 
  geom_bar(stat = "identity", fill = "#69b3a2") +
  xlab("Education") +
  ylab("Mean Income") +
  ggtitle("Mean Income by Education Level")

# Create a vector of categorical data
my_adult_data <- c("age", "workclass", "fnlwgt", "education", "education_num",
                     "marital_status", "occupation", "relationship", "race", 
                     "sex", "capital_gain", "capital_loss", "hours_per_week",
                     "native_country", "income")




library(corrplot)
cor.adult_dataset = cor(adult_data)
corrplot(adult_data)

colnames(adult_data)

library(caret)
num_adult_data = dummyVars("~.", data = adult_data[-15])
adult_data_1 = data.frame(predict(num_adult_data, newdata = adult_data))
adult_data_1
summary(adult_data_1)
colnames(adult_data_1)


colnames(adult_data_2)

sum(is.na(adult_data$income))
# Find the row with the missing value in the income column
missing_row <- which(is.na(adult_data$income))

# Remove the row from adult_data
adult_data <- adult_data[-missing_row,]


# Update adult_data_2 with the new income column
adult_data_1$income <- adult_data$income
adult_data_1$income
colnames(adult_data_1)

adult_data_1$income <- as.numeric(adult_data_1$income)
Full_model_adultdatset = lm(income~., data = adult_data_1)
summary(Full_model_adultdatset)

cor(Full_model_adultdatset$residuals, adult_data_1[-105])
alias(Full_model_adultdatset)


install.packages("car")
library(car)
library(MASS)

adult_data_2$income = adult_data$income

PCA_adult_datset = prcomp(adult_data_1[-105],scale. = T)
PCA_adult_datset
summary(PCA_adult_datset)
PCA_adult_datset$rotation
plot(PCA_adult_datset)
plot(PCA_adult_datset, type = "l", main = "Scree Plot")

P1 = principal(adult_data_1, nfactors =3, rotate ="varimax")
print(P1$loadings, cutoff = .4)









