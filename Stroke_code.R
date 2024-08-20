#-------------------------------------------------#
#########---------------Project "Stroke Prediction"
#-------------------------------------------------#


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
library(gmodels)
library(rstudioapi)
library(datasets) 
library(caTools)
library(party)
library(magrittr)
library(Boruta)
library(gridExtra)
library(randomForest)
library(DiagrammeR)
library(glmnet)
library(C50)
library(plotROC)
library(ROCR)
library(OptimalCutpoints)
library(e1071)
library(ROSE)
library(class)

path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)

stroke <- read.csv("data/healthcare-dataset-stroke-data.csv", 
                   stringsAsFactors = F)



#-------------------------------------------------#
###-----------------------------Exploring Data Set
#-------------------------------------------------#

dim(stroke) #dimensions
head(stroke) #first 6 rows
tail(stroke) #last 6 rows
str(stroke) #structure
names(stroke) #column names
summary(stroke) #summary for each columns
skim(stroke) #missing values, quantile, etc. for numeric var
create_report(stroke) #full data profile with visualizations
table(stroke$stroke) #count stroke patients - 249 with stroke


#-------------------------------------------------#
###---------------------------------Data Preparing
#-------------------------------------------------#

#Dealing with N/A in BMI column
stroke$bmi[stroke$bmi=="N/A"]=NA
stroke$bmi<-as.numeric(stroke$bmi)
bmi_means <- mean(stroke$bmi, na.rm = T)
stroke$bmi[is.na(stroke$bmi)] <- mean(stroke$bmi, na.rm = TRUE)
stroke$bmi <- round(stroke$bmi, digits = 2) #rounding values

#-----------------------------------------------------#
### Plots Visualizations of Data from Database"Stroke"
#-----------------------------------------------------#

#preparing new data frame for plots
stroke_plots <- stroke
#characters as factors
stroke_plots$stroke <- factor(stroke_plots$stroke, levels = c(0,1), labels = c("No", "Yes"))
stroke_plots$hypertension <- factor(stroke_plots$hypertension, levels = c(0,1), labels = c("No", "Yes"))
stroke_plots$heart_disease <- factor(stroke_plots$heart_disease, levels = c(0,1), labels = c("No", "Yes"))
#remove Other gender because it is only one case
stroke_plots <- subset(stroke_plots, gender != "Other")

#plot: gender
gender <- ggplot(stroke_plots, aes(x=reorder(gender, gender, function(x)-length(x)))) +
  geom_bar(fill='lightblue') +  
  labs(x='Gender', title = "Distribution of the Variable: Gender") +
  geom_text(aes(label = ..count..), stat = "count", vjust = 2, colour = "black")
gender
table(stroke_plots$gender)

#plot: type of residence
residence <- ggplot(stroke_plots, aes(x=reorder(Residence_type, Residence_type, function(x)-length(x)))) +
  geom_bar(fill='lightgreen') + 
  labs(x='Residence Type', title = "Distribution of the Variable: Residence") +
  geom_text(aes(label = ..count..), stat = "count", vjust = 2, colour = "black")
residence

#plot: type of work
work <- ggplot(stroke_plots, aes(x=work_type)) +
  geom_bar(fill='violet') + 
  labs(x='Type of Work', title = "Distribution of the Variable: Type of Work") +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.3, colour = "black")
work

#plot: smoking status
smoking <- ggplot(stroke_plots, aes(x=smoking_status)) +
  geom_bar(fill='lightpink') +
  labs(x='Smoking Status', title = "Distribution of the Variable: Smoking Status") +
  geom_text(aes(label = ..count..), stat = "count", vjust = 2, colour = "black")
smoking

#plot: hypertension
hypertension <- ggplot(stroke_plots, aes(x=as.factor(hypertension))) +
  geom_bar(fill='darkblue') + 
  labs(x='Hypertension', , title = "Distribution of the Variable: Hypertension") +
  scale_x_discrete(labels = c("0" = "no hypertension", "1" = "hypertension")) +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.3, colour = "black")
hypertension

#plot: heart disease
heart_disease <- ggplot(stroke_plots, aes(x=as.factor(heart_disease))) +
  geom_bar(fill='darkgreen') + 
  labs(x='Heart Disease', , title = "Distribution of the Variable: Heart Disease") +
  scale_x_discrete(labels = c("0" = "no stroke", "1" = "stroke")) +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.3, colour = "black")
heart_disease

#plot:BMI
#BMI classification (references: https://www.ncbi.nlm.nih.gov/books/NBK541070/)
stroke_bmi_class <- stroke_plots

stroke_bmi_class$bmi <- dplyr::case_when(
  stroke_bmi_class$bmi < 16.5 ~ "severly underweight",
  stroke_bmi_class$bmi < 18.5 ~ "underweight",
  stroke_bmi_class$bmi >= 18.5 & stroke_bmi_class$bmi <= 24.9  ~ "normal weight",
  stroke_bmi_class$bmi >= 25 &  stroke_bmi_class$bmi <= 29.9 ~ "overweight",
  stroke_bmi_class$bmi >= 30 & stroke_bmi_class$bmi <= 34.9 ~ "obesity class 1",
  stroke_bmi_class$bmi >= 35 &  stroke_bmi_class$bmi <= 39.9 ~ "obesity class 2",
  stroke_bmi_class$bmi >= 40 ~ "obesity class 3")

#Create a histogram
bmi_plot <- ggplot(stroke_bmi_class, aes(x=bmi)) +
  geom_bar(fill='red', width = 0.8) +
  labs(title="BMI", x="BMI", y = "count") +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.3, colour = "black") +
  theme(
    plot.title = element_text(color = "darkred", size = 15, face = "bold", hjust = 0.5),
    axis.title.x = element_text(color = "darkred", size = 13, face = "bold.italic"),
    axis.title.y = element_text(color = "darkred", size = 13, face = "bold.italic"))
bmi_plot

#overweight and obesity - descriptions of the group 
overweight <- stroke_bmi_class %>% dplyr::select(age, bmi) %>% dplyr::filter(bmi == "overweight")
summary(overweight)
m <- mean(overweight$age) 
m <- round(m, digits = 2)
m #mean age in the group of overweight patients - 49.5
percentage_overweight <- (1610/5109)*100 #group of patients with overweight - 31.5% of total numbers of patients
percentage_obesity <- (1920/5109)*100 #group of patients with obesity (all types) - 37.6%



#plot: Age and BMI and Stroke Occurrence
plot_age_bmi <- ggplot(data = stroke_plots, aes(x = age, y = bmi, color = as.factor(stroke))) +
  geom_point(alpha = 0.5) +
  scale_color_brewer(palette="Set1") +
  stat_smooth(aes(group = 1), method = "lm", formula = y ~ x, se = FALSE) +
  labs(title="Age and BMI and Stroke Occurrence", x="Age", y = "BMI", colour = "Stroke") +
  theme(
    plot.title = element_text(color = "navy", size = 15, face = "bold"),
    axis.title.x = element_text(color = "navy", size = 13, face = "bold"),
    axis.title.y = element_text(color = "navy", size = 13, face = "bold"),
    legend.title = element_text(color = "navy", size = 10, face = "bold"))
ggplotly(plot_age_bmi) 


#plot: Age and Average Glucose Level and Stroke Occurrence
plot_age_gluc <- ggplot(data = stroke_plots, aes(x = age, y = avg_glucose_level, color = as.factor(stroke)))+
  geom_point(alpha = 0.5) +
  scale_color_brewer(palette="Set2") +
  stat_smooth(aes(group = 1), method = "lm", formula = y ~ x, se = FALSE) +
  labs(title="Age & Average Glucose Level and Stroke Occurrence", x="Age", y = "Average Glucose Level (mg/dL)", 
       colour = "Stroke") +
  theme(
    plot.title = element_text(color = "darkgreen", size = 15, face = "bold", hjust = 0.5),
    axis.title.x = element_text(color = "darkgreen", size = 12, face = "bold"),
    axis.title.y = element_text(color = "darkgreen", size = 12, face = "bold"),
    legend.title = element_text(color = "darkgreen", size = 10, face = "bold", hjust = 0.5),
    legend.background = element_rect(fill = "white", colour = "darkgreen")) 
ggplotly(plot_age_gluc) 


#table: Smoking Status & Stroke 
stroke_smok_stat <- table(stroke_plots$smoking_status,stroke_plots$stroke) 
colnames(stroke_smok_stat)[1] <- "No Stroke"
colnames(stroke_smok_stat)[2] <- "Stroke"
print(stroke_smok_stat)


#table: Smoking Status & Hypertension 
hyperten_smok_stat = table(stroke$smoking_status,stroke$hypertension) 
colnames(hyperten_smok_stat)[1] <- "No Hypertension"
colnames(hyperten_smok_stat)[2] <- "Hypertension"
print(hyperten_smok_stat)


#table: Stroke $ Hypertension 
stroke_hypertens <- table(stroke$stroke,stroke$hypertension) 
colnames(stroke_hypertens)[1] <- "No Hypertension"
colnames(stroke_hypertens)[2] <- "Hypertension"
rownames(stroke_hypertens)[1] <- "No Stroke"
rownames(stroke_hypertens)[2] <- "Stroke"
print(stroke_hypertens)


#ggplot - glucose level and bmi
plot_gluc_bmi <- ggplot(data = stroke_plots, aes(x = avg_glucose_level, y = bmi, color = as.factor(smoking_status))) +
  geom_point(alpha = 0.5) +
  scale_color_brewer(palette="Set1") +
  stat_smooth(aes(group = 1), method = "lm", formula = y ~ x, se = FALSE) +
  labs(title="Average Glucose Level & BMI", x="Average Glucose Level", y = "BMI", colour = "Status of smoking") +
  theme(
    plot.title = element_text(color = "navy", size = 15, face = "bold"),
    axis.title.x = element_text(color = "navy", size = 13, face = "bold"),
    axis.title.y = element_text(color = "navy", size = 13, face = "bold"),
    legend.title = element_text(color = "navy", size = 10, face = "bold"))
ggplotly(plot_gluc_bmi) 


#boxplot - glucose level 
box_glucose <- ggplot(stroke_plots, aes(x=as.character(stroke), y=avg_glucose_level), color = stroke) + 
  geom_boxplot(show.legend = T) + 
  geom_jitter(shape=14, position=position_jitter(0.2), color = "lightblue") +
  labs(title="Stroke & Avg Glucose Level", x="Stroke", y = "Average Glucose Level") +
  theme(
    plot.title = element_text(color = "navy", size = 15, face = "bold.italic"),
    axis.title.x = element_text(color = "navy", size = 13, face = "bold"),
    axis.title.y = element_text(color = "navy", size = 13, face = "bold"))
box_glucose


# plot: Average Glucose Level in the Group of Patients with Stroke
glucose_and_stroke <- stroke_plots %>%
  filter (stroke == "Yes") %>%
  ggplot(aes(x = factor(gender), y = avg_glucose_level, fill = gender)) +
  stat_halfeye(adjust = 0.5,justification = -0.2,.width = 0,point_colour = NA) +
  geom_boxplot(width = 0.11,outlier.color = NA, alpha = 0.4) + 
  ggdist::stat_dots(side = "left",justification = 1.1,binwidth = 0.25) +
  labs(title="Gender & Avg Glucose Level in the Group of Patients with Stroke", x="Gender", y = "Average Glucose Level", color = "Gender") +
  theme(
    plot.title = element_text(color = "darkgreen", size = 15, face = "bold"),
    axis.title.x = element_text(color = "darkgreen", size = 13, face = "bold"),
    axis.title.y = element_text(color = "darkgreen", size = 13, face = "bold"),
    legend.title = element_text(color = "darkgreen", size = 10, face = "bold"),
    )
glucose_and_stroke


# plot: Average Glucose Level in the Group of Patients with no Stroke
glucose_no_stroke <- stroke_plots %>%
  filter (stroke == "No") %>%
  ggplot(aes(x = factor(gender), y = avg_glucose_level, fill = gender)) +
  stat_halfeye(adjust = 0.5,justification = -0.2,.width = 0,point_colour = NA) +
  geom_boxplot(width = 0.11,outlier.color = NA,alpha = 0.4) + 
  ggdist::stat_dots(side = "left", justification = 1.1, binwidth = 0.25)+
  labs(title="Gender & Avg Glucose Level with Patient with no Stroke", x="Gender", y = "Average Glucose Level", color = "Gender") +
  theme(
    plot.title = element_text(color = "darkgreen", size = 15, face = "bold"),
    axis.title.x = element_text(color = "darkgreen", size = 13, face = "bold"),
    axis.title.y = element_text(color = "darkgreen", size = 13, face = "bold"),
    legend.title = element_text(color = "darkgreen", size = 10, face = "bold"))
glucose_no_stroke


#-------------------------------------------------#
###-----------------------------------Correlations
#-------------------------------------------------#

#correlation: age & hypertension
cor(stroke$age, stroke$hypertension, use = "complete.obs")

#correlation: age & heart disease
cor(stroke$age, stroke$heart_disease, use = "complete.obs")

#correlation: age & glucose level
cor(stroke$age, stroke$avg_glucose_level, use = "complete.obs")
cor.test(stroke$age, stroke$avg_glucose_leve)
cor.test(stroke$age, stroke$stroke)
cor.test(stroke$avg_glucose_level, stroke$stroke)

###variables as numeric - for future analysis
stroke_new<-stroke
stroke_new$ever_married <- case_when(stroke_new$ever_married == "Yes" ~ 1, stroke_new$ever_married == "No" ~ 0)

stroke_new$work_type <- case_when(stroke_new$work_type == "children" ~ 0,
                                    stroke_new$work_type == "Never_worked" ~ 1,
                                    stroke_new$work_type == "Private" ~ 2,
                                    stroke_new$work_type == "Govt_job" ~ 3,
                                    stroke_new$work_type == "Self-employed" ~ 4)

stroke_new$Residence_type <- case_when(stroke_new$Residence_type == "Urban" ~ 1, stroke_new$Residence_type == "Rural" ~ 0)

stroke_new$smoking_status <- case_when(stroke_new$smoking_status == "never smoked" ~ 0, stroke_new$smoking_status == "formerly smoked" ~ 1,
stroke_new$smoking_status == "smokes" ~ 2,
stroke_new$smoking_status == "Unknown" ~ 3)

stroke_new$gender <- case_when(stroke_new$gender == "Female" ~ 0,stroke$gender == "Male" ~ 1)
stroke_new <- subset(stroke_new, gender != "Other")

stroke_new$stroke <- as.factor(stroke_new$stroke)



#-------------------------------------------------#
###-----------------------------Chi square analysis
#-------------------------------------------------#
#new data frame for chi square
new_dt <- stroke_new %>%
  dplyr::select(gender, hypertension, stroke, smoking_status)

#tables for chi square
stroke_gender <- table(new_dt$gender,new_dt$stroke) 
print(stroke_gender)

hyperten_gender <- table(new_dt$gender,new_dt$hypertension) 
print(hyperten_gender)

hyperten_stroke <- table(new_dt$hypertension,new_dt$stroke) 

#chi square analysis
print(chisq.test(stroke_gender))
print(chisq.test(hyperten_gender))
print(chisq.test(hyperten_stroke)) # there are no evidences to reject the null hypothesis


#-------------------------------------------------#
### -------------------------------Predictions Model 
#-------------------------------------------------#

#-------------------------------------------------#
###-------------------Generalized Linear Regression 
#-------------------------------------------------#

#simple formula
stroke_regression <- glm(stroke ~ ., data = stroke_new, family = binomial)
summary(stroke_regression)

#Coefficient Plot
coefplot(stroke_regression)

#Backward Stepwise Regression
backward_model <- stats::step(stroke_regression, direction = "backward")
#Both Directions Regression
both_model <- stats::step(stroke_regression, direction = "both")

#Full and Null step Procedure
model.null <- glm(stroke ~ 1, data = stroke_new, family = binomial())
model.full <- glm(stroke ~ gender + age + hypertension + heart_disease + avg_glucose_level, #I selected variables that make the most sense in the context of stroke occurrence based on existing research.
                  data = stroke_new, family = binomial())
stats::step(model.null, scope = list(upper = model.full), 
     direction = "both", test = "Chisq", data = stroke_new)
model.final <- glm(stroke ~ age + avg_glucose_level + hypertension + heart_disease, 
                   data = stroke_new, family = binomial())
summary(model.final)

#Feature Ranking and Selection Algorithm
#References:
# https://stats.stackexchange.com/questions/231623/features-selection-why-does-boruta-confirms-all-my-features-as-important
boruta_output <- Boruta(stroke ~ ., data = stroke_new, doTrace = 0)
rough_fix_mod <- TentativeRoughFix(boruta_output)
boruta_signif <- getSelectedAttributes(rough_fix_mod) #the most important factors
importances <- attStats(rough_fix_mod)
importances <- importances[importances$decision != "Rejected", c("meanImp", "decision")]
importances[order(-importances$meanImp), ] #the most important factors from the highest to the lowest importance
boruta_plot <- plot(boruta_output, ces.axis = 0.3, las = 2, xlab = "", main = "Feature importance")
boruta_plot #The plot indicates the importance of the "ever married" variable, but this is an apparent correlation (being married correlates with age). 



#-------------------------------------------------#
###-----------------------------------Splitting Data
#-------------------------------------------------#
set.seed(123)
sample_data <- sample.split(stroke_new, SplitRatio = 0.8)
train_data <- subset(stroke_new, sample_data == TRUE)
test_data <- subset(stroke_new, sample_data == FALSE)
prop.table(table(train_data$stroke)) #95% without stroke, 5% with stroke - reflects the actual distribution of the result in the entire study group, but shows also the huge unbalance


#-------------------------------------------------#
###-----Data Balancing with Package ROSE--------###
#-------------------------------------------------#
table(stroke_new$stroke)
rose_data <- ROSE(stroke ~ ., data = stroke_new, seed = 123)$data
table(rose_data$stroke)

set.seed(123)
sample_data <- sample.split(rose_data, SplitRatio = 0.8)
train_data <- subset(rose_data, sample_data == TRUE)
test_data <- subset(rose_data, sample_data == FALSE)
prop.table(table(train_data$stroke)) #data are more balanced!



#-------------------------------------------------#
###-------------Decision-Tree-------------------###
#-------------------------------------------------#

model_rpart <- rpart(stroke ~ ., data = train_data, method = "class")
model_rpart
rpart.plot(model_rpart)
preds <- predict(model_rpart, newdata = test_data, type = "class")
preds

pred_table <- table(prediction = preds, real_data = test_data$stroke) 
pred_table

#Confusion Matrix
class(preds)
class(test_data$stroke)
levels(preds)
levels(test_data$stroke)
test_data$stroke <- as.factor(test_data$stroke)
preds<- as.factor(preds)
confMatrix <- confusionMatrix(test_data$stroke, preds)
print(confMatrix)

#Params
accuracy_decisiontree <- sum(diag(pred_table)) / sum(pred_table) 
accuracy_decisiontree #Accuracy of the Decision Tree Model is .
print(paste('Accuracy for test is found to be', round(accuracy_decisiontree, 2)))
sensitivity <- confMatrix$byClass["Sensitivity"]
specificity <- confMatrix$byClass["Specificity"]
print(paste('Sensitivity for test is found to be', round(sensitivity, 2), "and specificity is", round(specificity, 2)))

#-------------------------------------------------#
###-------------KNN Model-----------------------###
#-------------------------------------------------#

#standardization of data 
train_data_scaled <- scale(train_data[, -ncol(train_data)])  
test_data_scaled <- scale(test_data[, -ncol(test_data)], center = attr(train_data_scaled, "scaled:center"), scale = attr(train_data_scaled, "scaled:scale"))

train_control <- trainControl(method = "cv", number = 10)
knn_fit <- train(stroke ~ ., data = train_data, method = "knn", trControl = train_control, tuneLength = 10)
print(knn_fit)
#Training of data
k_optimal <- knn_fit$bestTune$k
knn_model <- knn(train = train_data_scaled, test = test_data_scaled, cl = train_data$stroke, k = k_optimal)
#evaluation of model
#confusion matrix
confusion_matrix <- table(test_data$stroke, knn_model)
#accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(confusion_matrix)
print(paste("Accuracy:", round(accuracy, 2)))
#other params
confusion <- confusionMatrix(knn_model, as.factor(test_data$stroke))
print(confusion)


#-------------------------------------------------#
###-------------Random-Forest-------------------###
#-------------------------------------------------#
#model
set.seed(123)
stroke_rf <- randomForest(stroke ~ ., data=train_data, proximity = TRUE)
print(stroke_rf) #out of bag error is 17.17%, the train data model accuracy is 82.83%, ntree = 500, mtry = 3
plot(stroke_rf)

#prediction on the test data
pred_rf <- predict(stroke_rf, test_data)
pred_rf <- as.factor(pred_rf)
test_data$stroke <- as.factor(test_data$stroke)

#confusion matrix
confusionMatrix(pred_rf, test_data$stroke) #Accuracy = 0.83, Sensitivity = 0.81, Specificity = 0.86

#ROC Curve for Random Forest Model
pred_rf_prob <- predict(stroke_rf, newdata = test_data, type = "prob")[, 2]
pref_rf_df <- data.frame(pred = pred_rf_prob, truth = test_data$stroke)
opt_cut <- optimal.cutpoints(X = "pred", status = "truth", methods="Youden", data=pref_rf_df, tag.healthy = "0")
summary(opt_cut)
plot(opt_cut, which = 1)
#AUC = 0.917 - model is really good, TPR = 0.831, FPR = 0.154









#-------------------------------------------------#
#---XGBOOST (method from publication: Jared P. Lander "R dla każdego")
#-------------------------------------------------#
stroke_formula <- stroke ~ gender + age + hypertension + heart_disease + avg_glucose_level + bmi - 1
strokeX <- build.x(stroke_formula, data = stroke_new_st_ch, contrast = F)          
strokeY <- build.y(stroke_formula, data = stroke_new_st_ch)
#strokeY <- as.integer(relevel(strokeY, ref = 1)) - 1
class(strokeY)
strokeBoost <- xgboost(data = strokeX, label = strokeY, max.depth = 3, eta = 3,
                       nrounds = 20, objective = "binary:logistic")
xgb.plot.multi.trees(strokeBoost, feature_names = colnames(strokeX))
xgb.plot.importance(xgb.importance(strokeBoost, feature_names = colnames(strokeX)))
#the most important features - bmi, glucose level and age















  #References:
#Data:
#https://www.kaggle.com/datasets/fedesoriano/stroke-prediction-dataset

  #Materials on websites:
#https://www.geeksforgeeks.org/decision-tree-in-r-programming/
#https://koalatea.io/r-boosted-tree-regression/
#https://www.r-bloggers.com/2023/12/a-complete-guide-to-stepwise-regression-in-r/
#https://www.appsilon.com/post/r-decision-treees
#https://www.utstat.toronto.edu/~brunner/oldclass/appliedf11/handouts/2101f11StepwiseLogisticR.pdf
#https://www.projectpro.io/recipes/apply-gradient-boosting-for-classification-r
#https://machinelearningmastery.com/compare-models-and-select-the-best-using-the-caret-r-package/
#https://machinelearningmastery.com/evaluate-machine-learning-algorithms-with-r/

  #Articles:
# Choudhury, M. J. H., Chowdhury, M. T. I., Nayeem, A., & Jahan, W. A. (2015). Modifiable and non-modifiable risk factors of stroke: A review update. Journal of National Institute of Neurosciences Bangladesh, 1(1), 22-26.
# Hankey, G. J. (2020). Population impact of potentially modifiable risk factors for stroke. Stroke, 51(3), 719-728.
# Wang, Q., Zhang, L., Li, Y., Tang, X., Yao, Y., & Fang, Q. (2022). Development of stroke predictive model in community-dwelling population: A longitudinal cohort study in Southeast China. Frontiers in Aging Neuroscience, 14, 1036215.
  #WHO
#https://www.who.int/news-room/fact-sheets/detail/obesity-and-overweight
