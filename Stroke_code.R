####---------------Project "Stroke Prediction"

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
library(DiagrammerR)
library(glmnet)
library(C50)

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
table(stroke$stroke) #count stroke patients - 249 with stroke




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


#Dealing with N/A in BMI column
stroke$bmi[stroke$bmi=="N/A"]=NA

stroke$bmi<-as.numeric(stroke$bmi)
stroke$bmi[is.na(stroke$bmi)]<-mean(stroke$bmi)
bmi_means <- mean(stroke$bmi, na.rm = T)

#wróc potem niżej do klasyfikacji bmi 

#characters as factors
stroke$ever_married<- factor(stroke$ever_married)
stroke$work_type<- factor(stroke$work_type)
stroke$Residence_type<- factor(stroke$Residence_type)
stroke$stroke <- factor(stroke$stroke)




### Plots-Visualizations of Data from Database"Stroke"

#plot: gender
gender <- ggplot(stroke, aes(x=reorder(gender, gender, function(x)-length(x)))) +
  geom_bar(fill='lightblue') +  labs(x='Gender')
gender
table(stroke$gender)

#plot: type of residence
residence <- ggplot(stroke, aes(x=reorder(Residence_type, Residence_type, function(x)-length(x)))) +
  geom_bar(fill='lightgreen') +  labs(x='Residence Type')
residence


#plot: smoking status
smoking <- ggplot(stroke, aes(x=reorder(smoking_status, smoking_status, function(x)-length(x)))) +
  geom_bar(fill='lightpink') +  labs(x='Smoking Status')
smoking

#plot:BMI
#BMI classification as numeric (references: https://www.ncbi.nlm.nih.gov/books/NBK541070/)
stroke_bmi_class <- stroke

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
stroke$bmi == "obesity class 3" ~ 3)


bmi_df <-stroke %>% select(bmi)
bmi_df$bmi <- as.factor(bmi_df$bmi)
counts <- table(bmi_df$bmi)
print(counts)
bmi_df <- as.data.frame(counts)

#Define custom colors for each bmi
#bin_colors <- c("red", "green", "blue", "yellow", "purple")
#Create a histogram
bmi <- ggplot(bmi_df, aes(x = Var1, y = Freq)) +
  geom_col() +
  theme(legend.position = "bottom")
 bmi


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


### ---------------------Prediction Model 

###----Generalized Linear Regression 


#simple formula
stroke_regression <- glm(stroke ~ gender + age + hypertension + heart_disease + avg_glucose_level + bmi, 
                         data = stroke, family = binomial)
summary(stroke_regression)

#Coefficient Plot
coefplot(stroke_regression)

#Backward Stepwise Regression
backward_model <- stats::step(stroke_regression, direction = "backward")
#Both Directions Regression
both_model <- stats::step(stroke_regression, direction = "both")



#Feature Ranking and Selection Algorithm
boruta_output <- Boruta(stroke ~ ., data = stroke, doTrace = 0)
rough_fix_mod <- TentativeRoughFix(boruta_output)
boruta_signif <- getSelectedAttributes(rough_fix_mod)
importances <- attStats(rough_fix_mod)
importances <- importances[importances$decision != "Rejected", c("meanImp", "decision")]
importances[order(-importances$meanImp), ]

boruta_plot <- plot(boruta_output, ces.axis = 0.7, las = 2, xlab = "", main = "Feature importance")
boruta_plot #The plot indicates the importance of the "ever married" variable, but this is an apparent correlation (being married correlates with age). Moreover, the graph indicates that the glucose level variable is less important than in previous analyses. 
# https://stats.stackexchange.com/questions/231623/features-selection-why-does-boruta-confirms-all-my-features-as-important




###---Decision Tree (The Simplest Method for Me)

#splitting data
sample_data = sample.split(stroke, SplitRatio = 0.8)
train_data <- subset(stroke, sample_data == TRUE)
test_data <- subset(stroke, sample_data == FALSE)

prop.table(table(train_data$stroke)) #95% without stroke, 5% with stroke - 
#reflects the actual distribution of the result in the entire study group

#decision tree with ctree function
model_tree<- ctree(stroke ~ ., train_data)
plot(model_tree)

#prediction for decision tree
predict_model <- predict(model_tree, test_data) 
pred_table <- table(test_data$stroke, predict_model) 
pred_table

#accuracy
accuracy_decisiontree <- sum(diag(pred_table)) / sum(pred_table) #Accuracy of the Decision Tree Model is 0.95.



###---Random Forest (with Package Caret)

#new data frame with variable stroke as numeric for gbm function only
new_df <- stroke 
new_df$stroke <-as.numeric(new_df$stroke) -1
#new data frame with rmd uncsr character variables
new_df_1 <-subset(stroke, select = -c(ever_married, work_type, Residence_type, smoking_status))

#Basic Boosting Tree (gbm function)
model_gbm <- gbm::gbm(formula=stroke~., data = new_df)
model_gbm
head(predict(model_gbm, type = "response"))
tibble::as_tibble(summary(model_gbm))

#Random Forest (caret pckg)
set.seed(1)
model1 <- train(stroke ~ ., data = stroke, method = 'gbm', verbose = FALSE)
model1
plot(model1)

#preprocessing
set.seed(1) 
model2 <- train(stroke ~ ., data = stroke, method = 'gbm', preProcess = c("center", "scale"), verbose = FALSE)
model2
plot(model2)

#splitting data
set.seed(1)
inTraining <- createDataPartition(stroke$stroke, p = .80, list = FALSE)
training <- stroke[inTraining,]
testing  <- stroke[-inTraining,]

#model with training data
set.seed(1)
model3 <- train(stroke ~ ., data = training, method = 'gbm', preProcess = c("center", "scale"), verbose = FALSE)
model3
plot(model3)

#prediction for test data set
test.features = subset(testing, select=-c(stroke))
test.target = subset(testing, select=stroke)[,1]
predictions = predict(model3, newdata = test.features)

#Cross Validation - resampling and splitting data many times
ctrl <- trainControl(method = "cv",number = 10)

#retrain model
model4 <- train(stroke ~ ., data = training, method = 'gbm', preProcess = c("center", "scale"), trControl = ctrl, verbose = FALSE)
model4
plot(model4)

#next prediction for test data set
test.features = subset(testing, select=-c(stroke))
test.target = subset(testing, select=stroke)[,1]
predictions = predict(model4, newdata = test.features)

#tuning parameters
set.seed(1)
tuneGrid <- expand.grid(n.trees = c(50, 100),interaction.depth = c(1, 2), shrinkage = 0.1, n.minobsinnode = 10)

model5 <- train(stroke ~ .,data = stroke, method = 'gbm', preProcess = c("center", "scale"), trControl = ctrl, tuneGrid = tuneGrid, verbose = FALSE)
model5
model5$results %>% arrange(Accuracy)
plot(model5)


#---XGBOOST (method from publication: Jared P. Lander "R dla każdego")
stroke_formula <- stroke ~ gender + age + hypertension + heart_disease + avg_glucose_level + bmi - 1
strokeX <- build.x(stroke_formula, data = stroke, contrast = F)          
strokeY <- build.y(stroke_formula, data = stroke)
strokeY <- as.integer(relevel(strokeY, ref = 1)) - 1
strokeBoost <- xgboost(data = strokeX, label = strokeY, max.depth = 3, eta = 3,
                       nrounds = 20, objective = "binary:logistic")
xgb.plot.multi.trees(strokeBoost, feature_names = colnames(strokeX))
xgb.plot.importance(xgb.importance(strokeBoost, feature_names = colnames(strokeX)))
#the most important features - glucose level and age



### ---------------------Evaluation

###---With Using Caret pck 
#prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
#train the LVQ model
set.seed(1)
modelLvq <- train(stroke~., data=stroke, method="lvq", trControl=control)
#train the GBM model
set.seed(1)
modelGbm <- train(stroke~., data=stroke, method="gbm", trControl=control, verbose=FALSE)
#train the SVM model
set.seed(1)
modelSvm <- train(stroke~., data=stroke, method="svmRadial", trControl=control)
# collect resamples
results <- resamples(list(LVQ=modelLvq, GBM=modelGbm, SVM=modelSvm))
#summary
summary(results)
#boxplots of results
bwplot(results)
#dotplot of results
dotplot(results)

###Evaluation (https://machinelearningmastery.com/evaluate-machine-learning-algorithms-with-r/)
control_ml <- trainControl(method="repeatedcv", number=10, repeats=3)
seed_ml <- 1
metric <- "Accuracy"
preProcess=c("center", "scale")

# Linear Discriminant Analysis
set.seed(seed_ml)
fit.lda <- train(stroke~., data=stroke, method="lda", metric=metric, preProc=c("center", "scale"), trControl=control)
# Logistic Regression
set.seed(seed_ml)
fit.glm <- train(stroke~., data=stroke, method="glm", metric=metric, trControl=control)
# GLMNET
set.seed(seed_ml)
fit.glmnet <- train(stroke~., data=stroke, method="glmnet", metric=metric, preProc=c("center", "scale"), trControl=control)
# SVM Radial
set.seed(seed_ml)
fit.svmRadial <- train(stroke~., data=stroke, method="svmRadial", metric=metric, preProc=c("center", "scale"), trControl=control, fit=FALSE)
# kNN
set.seed(seed_ml)
fit.knn <- train(stroke~., data=stroke, method="knn", metric=metric, preProc=c("center", "scale"), trControl=control)
# CART
set.seed(seed_ml)
fit.cart <- train(stroke~., data=stroke, method="rpart", metric=metric, trControl=control)
# C5.0
set.seed(seed_ml)
fit.c50 <- train(stroke~., data=stroke, method="C5.0", metric=metric, trControl=control)
# Bagged CART
set.seed(seed_ml)
fit.treebag <- train(stroke~., data=stroke, method="treebag", metric=metric, trControl=control)
# Random Forest
set.seed(seed_ml)
fit.rf <- train(stroke~., data=stroke, method="rf", metric=metric, trControl=control)
# Stochastic Gradient Boosting (Generalized Boosted Modeling)
set.seed(seed_ml)
fit.gbm <- train(stroke~., data=stroke, method="gbm", metric=metric, trControl=control, verbose = F)


results <- resamples(list(lda=fit.lda, logistic=fit.glm, glmnet=fit.glmnet,
                          svm=fit.svmRadial, knn=fit.knn, cart=fit.cart, c50=fit.c50,
                          bagging=fit.treebag, rf=fit.rf, gbm=fit.gbm))
summary(results)
bwplot(results)
dotplot(results)
#glmnet seems the best





###Evaluation (https://machinelearningmastery.com/evaluate-machine-learning-algorithms-with-r/)
control_ml <- trainControl(method="repeatedcv", number=10, repeats=3)
seed_ml <- 1
metric <- "Accuracy"
preProcess=c("center", "scale")

# Linear Discriminant Analysis
set.seed(seed_ml)
fit.lda <- train(stroke~., data=new_df_1, method="lda", metric=metric, preProc=c("center", "scale"), trControl=control)
# Logistic Regression
set.seed(seed_ml)
fit.glm <- train(stroke~., data=new_df_1, method="glm", metric=metric, trControl=control)
# GLMNET
set.seed(seed_ml)
fit.glmnet <- train(stroke~., data=new_df_1, method="glmnet", metric=metric, preProc=c("center", "scale"), trControl=control)
# SVM Radial
set.seed(seed_ml)
fit.svmRadial <- train(stroke~., data=new_df_1, method="svmRadial", metric=metric, preProc=c("center", "scale"), trControl=control, fit=FALSE)
# kNN
set.seed(seed_ml)
fit.knn <- train(stroke~., data=new_df_1, method="knn", metric=metric, preProc=c("center", "scale"), trControl=control)
# CART
set.seed(seed_ml)
fit.cart <- train(stroke~., data=new_df_1, method="rpart", metric=metric, trControl=control)
# C5.0
set.seed(seed_ml)
fit.c50 <- train(stroke~., data=new_df_1, method="C5.0", metric=metric, trControl=control)
# Bagged CART
set.seed(seed_ml)
fit.treebag <- train(stroke~., data=new_df_1, method="treebag", metric=metric, trControl=control)
# Random Forest
set.seed(seed_ml)
fit.rf <- train(stroke~., data=new_df_1, method="rf", metric=metric, trControl=control)
# Stochastic Gradient Boosting (Generalized Boosted Modeling)
set.seed(seed_ml)
fit.gbm <- train(stroke~., data=new_df_1, method="gbm", metric=metric, trControl=control, verbose = F)


results_2 <- resamples(list(lda=fit.lda, logistic=fit.glm, glmnet=fit.glmnet,
                          svm=fit.svmRadial, knn=fit.knn, cart=fit.cart, c50=fit.c50,
                          bagging=fit.treebag, rf=fit.rf, gbm=fit.gbm))
summary(results_2)
bwplot(results_2)
dotplot(results_2)
#glmnet seems the best





  #References:
#https://www.geeksforgeeks.org/decision-tree-in-r-programming/
#https://koalatea.io/r-boosted-tree-regression/
#https://www.r-bloggers.com/2023/12/a-complete-guide-to-stepwise-regression-in-r/
#https://www.appsilon.com/post/r-decision-treees
#https://www.utstat.toronto.edu/~brunner/oldclass/appliedf11/handouts/2101f11StepwiseLogisticR.pdf
#https://www.projectpro.io/recipes/apply-gradient-boosting-for-classification-r
#https://machinelearningmastery.com/compare-models-and-select-the-best-using-the-caret-r-package/
#https://machinelearningmastery.com/evaluate-machine-learning-algorithms-with-r/

