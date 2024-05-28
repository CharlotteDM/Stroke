####Project "Stroke Prediction"

library(dplyr)
library(skimr)
library(DataExplorer)
library(corrplot)
library(tidyverse)
library(tidyquant)
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
library(rstudioapi)

path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)

stroke <- read.csv("data/healthcare-dataset-stroke-data.csv", 
                   stringsAsFactors = F)

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

#plot: "Glucose & Average Glucose Level"
means <- aggregate(avg_glucose_level~gender,stroke, mean)


ggplot(stroke, aes(x = gender, y = avg_glucose_level, fill = gender)) +
  geom_boxplot() +
  #geom_jitter(alpha = 0.3, color = "green") +
  stat_summary(fun=mean, colour="darkred", geom="point", 
               shape=18, size=3, show.legend=FALSE) + 
  geom_text(data = means, aes(label = avg_glucose_level, y = avg_glucose_level + 0.08)) +
  labs(x = "Gender", y = "Average Glucose Level") +
  theme_minimal()


# plot: Average Glucose Level in the Group of Patients with Stroke
stroke %>%
  filter (stroke == 1) %>%
ggplot(aes(x = factor(gender), y = avg_glucose_level, fill = factor(gender))) +
  stat_halfeye(
    adjust = 0.5,
    justification = -0.2,
    .width = 0,
    point_colour = NA
  )

# plot: Average Glucose Level in the Group of Patients with no Stroke
stroke %>%
  filter (stroke == 0) %>%
  ggplot(aes(x = factor(gender), y = avg_glucose_level, fill = factor(gender))) +
  stat_halfeye(
    adjust = 0.5,
    justification = -0.2,
    .width = 0,
    point_colour = NA
  )

plot(stroke$avg_glucose_level, stroke$bmi, 
     xlab = "Poziom glukozy", ylab = "BMI",
     main = "Wykres zależności pomiędzy poziomem glukozy a BMI",
     col = "blue", pch = 16) +
  theme_minimal()

ggplot(data = stroke, mapping = aes(x = avg_glucose_level, y = age)) +
  geom_point(alpha = 0.5, aes(color = gender))


### Correlations
#correlation: age & hypertension
cor(stroke$age, stroke$hypertension, use = "complete.obs")

#correlation: age & heart disease
cor(stroke$age, stroke$heart_disease, use = "complete.obs")

#correlation: age & glucose level
cor(stroke$age, stroke$avg_glucose_level, use = "complete.obs")



test1 <- chisq.test(table(stroke$age, stroke$bmi))
test1
summary(table(stroke$age, stroke$bmi))




#plot: age & glucose level








