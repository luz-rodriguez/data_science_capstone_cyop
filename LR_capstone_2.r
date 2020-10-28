
#####################################################################################
####        "Predicting Divorce with the DS Test: a quantitative approach         ####
####         "Luz Elena Rodriguez - 2020"                                        ####  
#####################################################################################


######## Libraries. Those will be downloaded assuming the reader don't have any of those

if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2",  repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(tibble)) install.packages("tibble", repos = "http://cran.us.r-project.org")
if(!require(corrgram)) install.packages("corrgram", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(mlbench)) install.packages("mlbench", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(ggridges)) install.packages("ggridges", repos = "http://cran.us.r-project.org")
if(!require(factoextra)) install.packages("factoextra", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(Rtools)) install.packages("Rtools", repos = "https://cran.rstudio.com/bin/windows/Rtools/")



## Data Set Description


#The dataset is taken from UCI Machine Learning Repository - Center for Machine Learning And Intelligent Systems
#https://archive.ics.uci.edu/ml/machine-learning-databases/00497/. 

#Reading the dataset from current directory

current_dir <- getwd()
divorce <- utils::read.table(file.path(current_dir,"divorce.csv"), 
                             sep = ";", header = TRUE)

#Exploring dataset
print("Split between married/divorced in dateset")
(table(divorce$Class))

print("Summary scale responses")
with(divorce, summary(Atr1))

print("Dataset glimpse")
tibble::glimpse(divorce)

#The dataset does not include any demographic variables and only have the 54 questions' responses and marital status(output variable).
#The attributes were taken from articles 

# *Attribute Information*
# 
# 1. When one of us apologizes when our discussions go bad, the issue does not extend.
# 2. I know we can ignore our differences, even if things get hard sometimes.
# 3. When we need to, we can take our discussions from the beginning and correct it.
# 4. When I argue with my spouse, it will eventually work for me to contact him.
# 5. The time I spent with my spouse is special for us.
# 6. We don't have time at home as partners.
# 7. We are like two strangers who share the same environment at home rather than family.
# 8. I enjoy our holidays with my spouse.
# 9. I enjoy traveling with my spouse.
# 10. My spouse and most of our goals are common.
# 11. I think that some day, my spouse and I will bee in harmony with each other.
# 12. My spouse and I have similar values regarding personal freedom.
# 13. My spouse and I have similar entertainment.
# 14. Most of our goals in regards to people (children, friends, etc.) are the same.
# 15. My dreams of living are similar and harmonious with those of my spouse.
# 16. I'm compatible with my spouse about what love should be.
# 17. I share the same views with my spouse about being happy.
# 18. My spouse and I have similar ideas about how marriage should be.
# 19. My spouse and I have similar ideas about how roles should be in marriage.
# 20. My spouse and I have similar values regarding trust.
# 21. I know exactly what my spouse likes.
# 22. I know how my spouse wants to be taken care of when she's sick.
# 23. I know my spouse's favorite food.
# 24. I can tell you what kind of stress my spouse is having in life.
# 25. I have knowledge of my spouse's inner world.
# 26. I know my spouse's basic concerns.
# 27. I know what my spouse's current sources of stress are.
# 28. I know my spouse's hopes and wishes.
# 29. I know my spouse very well.
# 30. I know my spouse's friends and their social relationships.
# 31. I feel aggressive when I argue with my spouse.
# 32. When discussing with my spouse, I usually use expressions such as X, Y, Z.
# 33. I can use negative statements about my spouse's personality during our discussions.
# 34. I can use offensive expressions during our discussions.
# 35. I can insult our discussions.
# 36. I can be humiliating when we argue.
# 37. My argument with my spouse is not calm.
# 38. I hate my spouse's way of bringing it up.
# 39. Fights often occur suddenly.
# 40. We're just starting a fight before I know what's going on.
# 41. When I talk to my spouse about something, my calm suddenly breaks.
# 42. When I argue with my spouse, it only snaps in and I don't say a word.
# 43. I'm mostly willing to calm the environment a little bit.
# 44. Sometimes I think it's good for me to leave home for a while.
# 45. I'd rather stay silent than argue with my spouse.
# 46. Even if I'm right in the argument, I'm willing not to upset the other side.
# 47. When I argue with my spouse, I remain silent because I am afraid of not being able to control my anger.
# 48. I feel right in our discussions.
# 49. I have nothing to do with what I've been accused of.
# 50. I'm not actually the one who's guilty of what I'm accused of.
# 51. I'm not the one who's wrong about problems at home.
# 52. I wouldn't hesitate to tell her about my spouse's inadequacy.
# 53. I remind my spouse of her inadequacies during our discussion.
# 54. I'm not afraid to tell her about my spouse's incompetence.
# 


## Data wrangling

#The decision variable *Class* is a binary variable, having married as 0 and divorced 1. 
#I will adjust this vaiable as factor. Other than that, the dataste is very clean.

divorce <- divorce %>% dplyr::mutate(Class = factor(Class))


## Datasets for model creation

#This dataset has only 170 rows. Being a small dataset, I will split the data in a 80-20 proportion for training and testing.
#There will not be a validation dataset.

set.seed(1, sample.kind="Rounding")

test_index <- createDataPartition(y = divorce$Class, times = 1, p = 0.2, list = FALSE)
divorce_training <-divorce[-test_index, ]
divorce_testing <- divorce[test_index, ]

print("Training set proportions:")
prop.table(table(divorce_training$Class))

print("Testing set proportions:")
prop.table(table(divorce_testing$Class))


## Data Exploration

# - Dataset overview

summary <- summary(divorce)
summary

# - Missing values 

table(is.na(divorce))

# - Histograms of predictor variables

## define predictor variables
attributes <- setdiff(colnames(divorce), "Class")

## initialize an empty list to store plots
histograms = list()

## set plot index
i = 1

## create plot and fill plot list

for (attr in attributes) {
  hist = ggplot2::ggplot(divorce_training) + 
    ggplot2::geom_histogram(aes_string(x=attr, fill="Class"),
                   position="stack", alpha=0.6) + 
    ggplot2::theme_classic() + 
    ggplot2::theme(legend.position="top")
    
  histograms[[i]] = hist
  i = i + 1
}  
## display plots
histograms

#- Evaluating variables correlation

#calculating correlation among variables in divorce
corr_divorce <- cor(attributes)
corrplot::corrplot(corr_divorce, type="upper", method="shade",tl.cex = 0.6)

#- Eliminating highly correlated variables

x <- divorce_training[ ,attributes] #Selecting attributes from dataset

divorce_training_pca <- prcomp(x) #Calculating PC from attributes
summary(divorce_training_pca)

data.frame(divorce_training_pca$x[,1:2], Class=divorce_training$Class) %>% 
  ggplot(aes(PC1,PC2, fill = Class))+
  geom_point(cex=3, pch=21) +
  coord_fixed(ratio = 1)+
  ggtitle("Visual representation First Two Principal Components")

data.frame(divorce_training_pca$x[,2:3], Class=divorce_training$Class) %>% 
  ggplot2::ggplot(aes(PC2,PC3, fill = Class))+
  ggplot2::geom_point(cex=3, pch=21) +
  coord_fixed(ratio = 1)+
  ggplot2::ggtitle("Visual representation Principal Components 2 and 3")

# Creating a new training and testing dataset with principal components to use in the modeling session.

divorce_training_pc = as.data.frame(divorce_training_pca$x)
divorce_training_pc$Class = divorce_training$Class
head(divorce_training_pc)

divorce_testing_pc = predict(divorce_training_pca, newdata = divorce_testing)
divorce_testing_pc = as.data.frame(divorce_training_pc)
divorce_testing_pc$class = divorce_testing$class

factoextra::fviz_eig(divorce_training_pca, addlabels=TRUE, 
                     ylim=c(0,80), geom = c("bar", "line"), 
                     barfill="lightblue", barcolor="grey", linecolor="blue", ncp=7) +
 labs(title = "Variance Explained By Each Principal Component",
         x = "Principal Components", y = "% of Variance")

#The following chart also shows how the first component explains 72% of variability.

factoextra::fviz_pca_biplot(divorce_training_pca, 
                            col.ind = as.factor(divorce_training$Class), col="black",
                palette = "lancet", geom = c("point"), repel=TRUE,
                legend.title="Divorced Y: 1, N:0", addEllipses = TRUE)

## Models

### Baseline prediction

#Randomly guessing whether a person will get divorced based on the answers to the questionare

set.seed(3, sample.kind = "Rounding")
# guess with equal probability of divorce
guess_divorce <- sample(c(0,1), nrow(divorce_testing), replace = TRUE)
#how many people would be divorced
print("Proportion of people whow would get divorced")
mean(guess_divorce)
#Calculating accuracy comparing guessing with testing data set
print("Accuracy")
mean(guess_divorce == divorce_testing$Class)


### Accuracy table 
summary_accuracy <- tibble(Method = "Baseline Prediction", Accuracy = 0.6285)
summary_accuracy
 

### Logistic regression

#- glm model with all data

set.seed(3, sample.kind = "Rounding")
logistic_model <- glm(Class ~.,family=binomial(link='logit'),data=divorce_training)

#summary(logistic_model)
 
glm_preds_divorce <- predict(logistic_model, type="response")
print("Divorce rate in logistic regression")
mean((ifelse(glm_preds_divorce > 0.5, 1, 0)) == divorce_testing$Class)

#- Accuracy in testing set Logistic regression

## create prediction probabilities (on test dataset)
glm_test_pred_probs = predict(logistic_model, type="response", newdata=divorce_testing)

## create predictions (on test dataset)
glm_test_preds = as.factor(ifelse(glm_test_pred_probs > 0.5, 1,0))

## evaluate performance (on test dataset)
confusionMatrix(glm_test_preds, divorce_testing$Class)
 
### Accuracy table 
# Update table based on testing data
summary_accuracy <- bind_rows(summary_accuracy, 
                    tibble(Method = "Logistic Regression", 
                           Accuracy = 0.9429))
# Show table 
knitr::kable(summary_accuracy)
 
#- glm model with Principal Components data

set.seed(3, sample.kind = "Rounding")
logistic_model_pc <- glm(Class ~.,family=binomial(link='logit'),data=divorce_training_pc)

#- Accuracy in testing set Principal Components Logistic regression

## create prediction probabilities (on test dataset)
glm_test_pred_probs_pc = predict(logistic_model_pc, type="response", 
                                 newdata=divorce_testing_pc)

## create predictions (on test dataset)
glm_test_preds_pc = as.factor(ifelse(glm_test_pred_probs_pc > 0.5, 1,0))

## evaluate performance (on test dataset)
confusionMatrix(glm_test_preds_pc, divorce_testing_pc$Class)
 
### Accuracy table 
# Update the accuracy table base on testing data
summary_accuracy <- bind_rows(summary_accuracy, 
                    tibble(Method = "Logistic Regression PC", 
                           Accuracy = 1))
# Show the RMSE improvement  
knitr::kable(summary_accuracy)
 
### kNN Model

#- Model with all the data

set.seed(3, sample.kind = "Rounding")    # simulate R 3.5
train_knn_divorce <- train(Class ~ .,
                     method = "knn",
                     data = divorce_training,
                     tuneGrid = data.frame(k = seq(3, 30, 2)))
train_knn_divorce$bestTune
 
#Displaying best tune  
ggplot2::ggplot(train_knn_divorce) + 
  ggplot2::geom_line(colour="#ba1ea8") + 
  ggplot2::geom_point(colour="#ba1ea8", shape=4)+
  ggplot2::scale_x_continuous(limits = c(0,10), breaks=seq(0,12,1)) + 
  theme_minimal()
  
knn_preds_divorce <- predict(train_knn_divorce, divorce_testing)
mean(knn_preds_divorce == divorce_testing$Class)
 
### Accuracy table 
# Update table 
summary_accuracy <- bind_rows(summary_accuracy, 
                    tibble(Method = "kNN original data", 
                           Accuracy = 0.9714))
# Show table 
knitr::kable(summary_accuracy)
 
# - kNN Crossvalidation

set.seed(8, sample.kind = "Rounding")    # simulate R 3.5
train_knn_cv_divorce <- train(Class ~ .,
                         method = "knn",
                         data = divorce_training,
                         tuneGrid = data.frame(k = seq(3, 30, 2)),
                         trControl = trainControl(method = "cv", number = 10, p = 0.9))
train_knn_cv_divorce$bestTune

 
knn_cv_preds_divorce <- predict(train_knn_cv_divorce, divorce_testing)
mean(knn_cv_preds_divorce == divorce_testing$Class)
 
### Accuracy table 
# Update the table  
summary_accuracy <- bind_rows(summary_accuracy, 
                    tibble(Method = "kNN original data Cross Validation", 
                           Accuracy = 0.9714))
# Show table
knitr::kable(summary_accuracy)
 

#- kNN with only PCA

set.seed(3, sample.kind = "Rounding")    # simulate R 3.5
train_knn_divorce_pc <- train(Class ~ .,
                     method = "knn",
                     data = divorce_training_pc,
                     tuneGrid = data.frame(k = seq(3, 30, 2)))
train_knn_divorce_pc$bestTune
 

knn_cv_preds_divorce_pc <- predict(train_knn_divorce_pc, divorce_testing_pc)
mean(knn_cv_preds_divorce_pc == divorce_testing_pc$Class)
  
### Accuracy table 
# Update the error table  
summary_accuracy <- bind_rows(summary_accuracy, 
                    tibble(Method = "kNN PC", 
                           Accuracy = 0.9778))
# Show the RMSE improvement  
knitr::kable(summary_accuracy)
 

### Classification Tree

# - All the data

train_rpart_divorce = rpart::rpart(Class ~ ., 
                     data = divorce_training, 
                     method = 'class',
                     control = rpart.control(minsplit=2),
                     model = TRUE)

## create prediction probabilities (on train dataset)
rpart_train_pred_probs = predict(train_rpart_divorce)

## create predictions (on train dataset)
rpart_train_preds = as.factor(ifelse(rpart_train_pred_probs[ , 2] > 0.5, 1, 0))

rpart.plot::prp(train_rpart_divorce, main="Decision Tree to Predict Divorce")
 
ct_preds_divorce <- predict(train_rpart_divorce, divorce_testing)
mean(ct_preds_divorce == divorce_testing$Class)

 
### Accuracy table 
# Update the error table  
summary_accuracy <- bind_rows(summary_accuracy, 
                    tibble(Method = "Classification tree", 
                           Accuracy = 0.5))
# Show the Accuracy table 
knitr::kable(summary_accuracy)
 

varImp(train_rpart_divorce) 
 
#- Classification tree principal components

train_rpart_divorce_pc = rpart::rpart(Class ~ ., 
                     data = divorce_training_pc, 
                     method = 'class',
                     control = rpart.control(minsplit=2),
                     model = TRUE)

## create prediction probabilities (on train dataset)
rpart_train_pred_probs_pc = predict(train_rpart_divorce_pc)

## create predictions (on train dataset)
rpart_train_preds_pc = as.factor(ifelse(rpart_train_pred_probs_pc[ , 2] > 0.5, 1, 0))

rpart.plot::prp(train_rpart_divorce_pc , main="Decision Tree to Predict Divorce PC")

ct_preds_divorce_pc <- predict(train_rpart_divorce_pc, divorce_testing_pc)
mean(ct_preds_divorce_pc == divorce_testing_pc$Class)
 
### Accuracy table 
# Update the error table  
summary_accuracy <- bind_rows(summary_accuracy, 
                    tibble(Method = "Classification tree PC", 
                           Accuracy = 0.5))
# Show the Accuracy table 
knitr::kable(summary_accuracy)
 

### Random Forest

# - All data

set.seed(14, sample.kind = "Rounding")    # simulate R 3.5
train_rf_divorce <- train(Class ~ .,
                    data = divorce_training,
                    method = "rf",
                    ntree = 100,
                    tuneGrid = data.frame(mtry = seq(1:7)))
train_rf_divorce$bestTune
 
rf_preds_divorce <- predict(train_rf_divorce, divorce_testing)
mean(rf_preds_divorce == divorce_testing$Class)

varImp(train_rf_divorce) 
 
train_rf_divorce$finalModel # inspect final model

# make plot of decision tree
plot(train_rf_divorce$finalModel, margin = 0.1)

train_rf <- randomForest(Class ~ ., data=divorce_training)

confusionMatrix(predict(train_rf, divorce_testing),
                divorce_testing$Class)$overall["Accuracy"]
  
### Accuracy table 
# Update the error table  
summary_accuracy <- bind_rows(summary_accuracy, 
                    tibble(Method = "Random Forest", 
                           Accuracy = 0.9714))
# Show table
knitr::kable(summary_accuracy)
 
#- Random Forest with Principal Components

set.seed(14, sample.kind = "Rounding")    # simulate R 3.5
train_rf_divorce_pc <- train(Class ~ .,
                    data = divorce_training_pc,
                    method = "rf",
                    ntree = 100,
                    tuneGrid = data.frame(mtry = seq(1:7)))
train_rf_divorce$bestTune

rf_preds_divorce_pc <- predict(train_rf_divorce_pc, divorce_testing_pc)
mean(rf_preds_divorce_pc == divorce_testing_pc$Class)

varImp(train_rf_divorce_pc) 
 
train_rf_divorce_pc$finalModel # inspect final model

# make plot of decision tree
plot(train_rf_divorce_pc$finalModel, margin = 0.1)

train_rf_pc <- randomForest(Class ~ ., data=divorce_training_pc)

confusionMatrix(predict(train_rf_pc, divorce_testing_pc),
                divorce_testing_pc$Class)$overall["Accuracy"]

### Accuracy table 
# Update the error table  
summary_accuracy <- bind_rows(summary_accuracy, 
                    tibble(Method = "Random Forest PC", 
                           Accuracy = 1))
# Show the RMSE improvement  
knitr::kable(summary_accuracy)
 
########################################################################################### END