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



####Creating new data frame for chi square analysis
new_dt <- stroke %>%
  select(gender, hypertension, stroke, smoking_status)

new_dt$gender <- case_when(
  new_dt$gender == "Female" ~ 1,
  new_dt$gender == "Male" ~ 2,
  new_dt$gender == "Other" ~ 3
)

#plot: age & glucose level








