####Project "Stroke Prediction"

library(dplyr)
library(skimr)
library(DataExplorer)
library(corrplot)
library(caret)
library(tidyverse)
library(tidyquant)
library(tidymodels)
library(ggplot2)
library(gganimate)
library(ggrepel)
library(ggdist)
library(ggthemes)
library(RColorBrewer)
library(knitr)
library(plotly)
library(htmlwidgets)
library(GGally)
library(stats)
library(coefplot)
library(rpart)
library(rpart.plot)
library(useful)
library(xgboost)
library(rstudioapi)
library(datasets) 
library(caTools)
library(party)
library(magrittr)

path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)

stroke <- read.csv("data/healthcare-dataset-stroke-data.csv", 
                   stringsAsFactors = F)

#Gender as numeric
stroke$gender <- case_when(
  stroke$gender == "Female" ~ 1,
  stroke$gender == "Male" ~ 2,
  stroke$gender == "Other" ~ 3)

#Smoking Status as numeric
stroke$smoking_status <- case_when(
  stroke$smoking_status == "formerly smoked" ~ 1,
  stroke$smoking_status == "never smoked" ~ 2,
  stroke$smoking_status == "smokes" ~ 3,
  stroke$smoking_status == "Unknown" ~ 4)

#BMI classification as numeric (references: https://www.ncbi.nlm.nih.gov/books/NBK541070/)
stroke$bmi = case_when(
    stroke$bmi < 16.5 ~ "severly underweight",
    stroke$bmi < 18.5 ~ "underweight",
    stroke$bmi >= 18.5 & stroke$bmi <= 24.9  ~ "normal weight",
    stroke$bmi >= 25 & stroke$bmi <= 29.9 ~ "overweight",
    stroke$bmi >= 30 & stroke$bmi <= 34.9 ~ "obesity class 1",
    stroke$bmi >= 35 & stroke$bmi <= 39.9 ~ "obesity class 2",
    stroke$bmi >= 40 ~ "obesity class 3")

stroke$bmi <- case_when(
  stroke$bmi == "severly underweight" ~ 1,
  stroke$bmi == "underweight" ~ 2,
  stroke$bmi == "normal weight" ~ 3,
  stroke$bmi == "overweight" ~ 4,
  stroke$bmi == "obesity class 1" ~ 5,
  stroke$bmi == "obesity class 2" ~ 6,
  stroke$bmi == "obesity class 3" ~ 3,)

#characters as factors
stroke$ever_married<- factor(stroke$ever_married)
stroke$work_type<- factor(stroke$work_type)
stroke$Residence_type<- factor(stroke$Residence_type)


#exploring data set
dim(stroke) #dimensions
head(stroke) #first 6 rows
str(stroke) #structure
names(stroke) #column names
summary(stroke) #summary for each columns
skim(stroke) #missing values, quantile, etc. for numeric var
create_report(stroke) #full data profile with visualizations


### Plots-Visualizations of Data from Database"Stroke"

#plot: gender
gender <- ggplot(stroke, aes(x=reorder(gender, gender, function(x)-length(x)))) +
  geom_bar(fill='lightblue') +  labs(x='Gender')
gender


#plot: type of residence
residence <- ggplot(stroke, aes(x=reorder(Residence_type, Residence_type, function(x)-length(x)))) +
  geom_bar(fill='lightgreen') +  labs(x='Residence Type')
residence


#plot: smoking status
smoking <- ggplot(stroke, aes(x=reorder(smoking_status, smoking_status, function(x)-length(x)))) +
  geom_bar(fill='lightpink') +  labs(x='Smoking Status')
smoking

#tables: Smoking Status & Stroke; Smoking Status & Hypertension
stroke_smok_stat = table(stroke$smoking_status,stroke$stroke) 
colnames(stroke_smok_stat)[1] <- "No Stroke"
colnames(stroke_smok_stat)[2] <- "Stroke"
print(stroke_smok_stat)


hyperten_smok_stat = table(stroke$smoking_status,stroke$hypertension) 
colnames(hyperten_smok_stat)[1] <- "No Hypertension"
colnames(hyperten_smok_stat)[2] <- "Hypertension"
print(hyperten_smok_stat)


# plot: Average Glucose Level in the Group of Patients with Stroke
glucose_and_stroke <- stroke %>%
  filter (stroke == 1) %>%
ggplot(aes(x = factor(gender), y = avg_glucose_level, fill = factor(gender))) +
  stat_halfeye(
    adjust = 0.5,
    justification = -0.2,
    .width = 0,
    point_colour = NA
  ) +
  geom_boxplot(
    width = 0.11,
    outlier.color = NA,
    alpha = 0.4
  ) + ggdist::stat_dots(
    side = "left",
    justification = 1.1,
    binwidth = 0.25
  ) 
glucose_and_stroke

# plot: Average Glucose Level in the Group of Patients with no Stroke
glucose_no_stroke <- stroke %>%
  filter (stroke == 0) %>%
  ggplot(aes(x = factor(gender), y = avg_glucose_level, fill = factor(gender))) +
  stat_halfeye(
    adjust = 0.5,
    justification = -0.2,
    .width = 0,
    point_colour = NA
  ) +
  geom_boxplot(
    width = 0.11,
    outlier.color = NA,
    alpha = 0.4
  ) + ggdist::stat_dots(
    side = "left",
    justification = 1.1,
    binwidth = 0.25
  ) 
glucose_no_stroke



#plot(stroke$avg_glucose_level, stroke$bmi, 
     #xlab = "Poziom glukozy", ylab = "BMI",
     #main = "Wykres zależności pomiędzy poziomem glukozy a BMI",
     #col = "blue", pch = 16) + theme_minimal()

#ggplot(data = stroke, mapping = aes(x = avg_glucose_level, y = age)) +
  #geom_point(alpha = 0.5, aes(color = gender))


### Correlations
#correlation: age & hypertension
cor(stroke$age, stroke$hypertension, use = "complete.obs")

#correlation: age & heart disease
cor(stroke$age, stroke$heart_disease, use = "complete.obs")

#correlation: age & glucose level
cor(stroke$age, stroke$avg_glucose_level, use = "complete.obs")


####Creating new data frame for chi square analysis
new_dt <- stroke %>%
  select(gender, hypertension, stroke, smoking_status)


#remove Other gender for chi square table
new_dt <- subset(new_dt, gender != "3")

#tables for chi square
stroke_gender = table(new_dt$gender,new_dt$stroke) 
print(stroke_gender)

hyperten_gender = table(new_dt$gender,new_dt$hypertension) 
print(hyperten_gender)

hyperten_stroke = table(new_dt$hypertension,new_dt$stroke) 
print(hyperten_stroke)

#chi square analysis
print(chisq.test(stroke_gender))
print(chisq.test(hyperten_gender))
print(chisq.test(hyperten_stroke)) 
# --- there are no evidences to reject the null hypothesis

#mcnemar test
mcnemar.test(hyperten_stroke)
# --- there is no evidence to reject the null hypothesis


### Prediction Model 

###Generalized Linear Regression 


#simple formula
stroke_regression <- glm(stroke ~ gender + age + hypertension + heart_disease + avg_glucose_level + bmi, 
                         data = stroke, family = binomial)
summary(stroke_regression)

#Coefficient Plot
coefplot(stroke_regression)



###Decision Tree

#formula without splitted data
stroke_tree <- rpart(stroke ~ gender + age + hypertension + heart_disease + avg_glucose_level + bmi, 
                     data = stroke)
print(stroke_tree)
rpart.plot(stroke_tree, extra = "auto")


#formula WITH splitted data
sample_data = sample.split(stroke, SplitRatio = 0.8)
train_data <- subset(stroke, sample_data == TRUE)
test_data <- subset(stroke, sample_data == FALSE)

model<- ctree(stroke ~ ., train_data)
plot(model)


  
  
  
  
  
  
  
  
  
  
  
  #bibliografia
  #https://www.datacamp.com/tutorial/decision-trees-R