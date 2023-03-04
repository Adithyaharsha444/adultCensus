install.packages("ggplot2")
install.packages("ggalluvial")
install.packages("ggrepel")
install.packages("car")
install.packages("psych")
library(psych)
library(caret)
library(ggplot2)
library(dplyr)
library(ggalluvial)
View(adult_data)
library(car)
library(MASS)

# Load data
adult_data <- read.csv("D:/Advance Data Analysis/ADA PROJECT/adult.csv", header = TRUE)


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

summary(adult_data)

ggplot(adult_data, aes(x = education, y = income)) + 
  geom_bar(stat = "identity", fill = "#69b3a2") +
  xlab("Education") +
  ylab("Income") +
  ggtitle("Mean Income by Education Level")


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


colnames(adult_data)
#creating dummy variable for character variable 
num_adult_data = dummyVars("~.", data = adult_data[-15])
adult_data_1 = data.frame(predict(num_adult_data, newdata = adult_data))
adult_data_1
summary(adult_data_1)

colnames(adult_data_1)

#converting income as numeric
adult_data_1$income <- as.numeric(adult_data_1$income)

#Fiting a Full Model
Full_model_adultdatset = lm(income~., data = adult_data_1)
summary(Full_model_adultdatset)

#correlation 
cor(Full_model_adultdatset$residuals, adult_data_1[-105])
alias(Full_model_adultdatset)

#PCA 
PCA_adult_datset = prcomp(adult_data_1[-105],scale. = T)
PCA_adult_datset
summary(PCA_adult_datset)

PCA_adult_datset$rotation

#Screeplot to check elbow point
plot(PCA_adult_datset, type = "l", main = "Scree Plot")

#principle analysis  
P1 = principal(adult_data_1, nfactors =3, rotate ="varimax")
print(P1$loadings, cutoff = .4)









