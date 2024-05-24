#Project "Stroke Prediction"
library("dplyr")
library("skimr")
library("DataExplorer")
library("tidyverse")
library("ggplot2")
library("gganimate")
library("ggrepel")
library("RColorBrewer")
library("knitr")
library("plotly")
library("htmlwidgets")
library("GGally")
library("stats")
library("rstudioapi")

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
skim(stroke) #missing values, quantile, etc.
create_report(stroke) #full data profile with visualizations


#plot: gender
gender <- ggplot(stroke, aes(x=reorder(gender, gender, function(x)-length(x)))) +
  geom_bar(fill='lightblue') +  labs(x='Gender')
gender


#plot: type of residence
residence <- ggplot(stroke, aes(x=reorder(Residence_type, Residence_type, function(x)-length(x)))) +
  geom_bar(fill='lightgreen') +  labs(x='Type of Residence')
residence

#plot: smoking status
smoking <- ggplot(stroke, aes(x=reorder(smoking_status, smoking_status, function(x)-length(x)))) +
  geom_bar(fill='lightpink') +  labs(x='Smoking Status')
smoking

#correlation
cor(stroke$age, stroke$hypertension, use = "complete.obs")





