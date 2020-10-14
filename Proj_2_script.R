### INTRODUCION, DATA AND LIBRARY DOWNLOADS
### EXPLORATORY DATA ANALYSIS

## Check for the required libraries and if neccessary install them

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(MASS)) install.packages("MASS", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")

## Call libraries
library(tidyverse)
library(caret)
library(ggthemes)
library(randomForest)
library(MASS)
library(e1071)
library(kableExtra)

##Download the two wine files from the UCI ML repository
url_red_wine <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"

url_white_wine <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"

download.file(url_red_wine, "datasets/red_wine.csv")
download.file(url_white_wine, "datasets/white_wine.csv")

##Clean environment of no longer needed variables
rm(url_red_wine)
rm(url_white_wine)

##Import the CSV files into R - delimited with ;
red_wine <- read_delim("datasets/red_wine.csv", ";")
white_wine <- read_delim("datasets/white_wine.csv", ";")

##Verify columns are identical between white and red wine
identical(colnames(red_wine), colnames(white_wine))

##List predictor variables for description in the report
colnames(red_wine)[1:11]

##Print summary of numeric variables
##Formatting the tables to be even slightly legible was a challenge - stack overflow 
##has been a really useful resource.
red_wine %>%
  select(-quality) %>%
  summary() %>%
  knitr::kable(format = "latex", caption = "Red wine summary data") %>%
  kableExtra::kable_styling(latex_options = "scale_down")

white_wine %>%
  select(-quality) %>%
  summary() %>%
  knitr::kable(format = "latex", caption = "White wine summary data") %>%
  kableExtra::kable_styling(latex_options = "scale_down")

##Merge wine sets into a single set
red_temp <- red_wine %>%
  mutate(type = "Red")

white_temp <- white_wine %>%
  mutate(type = "White")

wine_combined <- bind_rows(white_temp, red_temp)

##Tidy up intermediate step variables
rm(white_temp, red_temp)

##wine type as factor
wine_combined$type <- as.factor(wine_combined$type)

##Plot the distribution of quality as overlaid density function (adjust = 2 to smooth)
wine_combined %>%
  ggplot(aes(quality, fill = type)) +
  geom_density(alpha = 0.4, adjust = 2)

##Boxplots!
wine_combined %>%
  ggplot(aes(type, quality)) +
  geom_boxplot(aes(colour = type)) +
  xlab("Wine type")

##Summary statistics of ratings by wine type
wine_combined %>%
  group_by(type) %>%
  summarise(mean = mean(quality), std_dev = sd(quality), median = median(quality)) %>%
  knitr::kable()

##Alcohol content histograms
wine_combined %>%
  ggplot(aes(alcohol)) + 
  geom_histogram() +
  facet_wrap(~ type, nrow = 1) + 
  theme_minimal() +
  ggtitle("Histogram of alcohol content")

## Residual sugar box plots
wine_combined %>%
  ggplot(aes(type, `residual sugar`, colour = type)) + 
  geom_boxplot() +
  ggtitle("Residual sugar by wine type") +
  xlab("Wine type") +
  theme_minimal()

##Demonstrate lack of variation in "density" feature
wine_combined %>%
  summarise(min_density = min(density), mean_density = mean(density), sd_density = sd(density), max_density = max(density))

##Remove density feature from all data frames
red_wine <- red_wine %>%
  select(-density)

white_wine <- white_wine %>%
  select(-density)

wine_combined <- wine_combined %>%
  select(-density)

### BINARY SELECTOR - RED OR WHITE WINE ###

##Getting to the good stuff - splitting data into train-test
set.seed(1236, sample.kind = "Rounding")

#even with list = FALSE this was returning a matrix
index <- as.vector(createDataPartition(y = wine_combined$type, times = 1, p = 0.2, list = FALSE))

#Subsetting the combined wine data set into test and training data
wine_test <- wine_combined[index,]
wine_train <- wine_combined[-index,]

#Remove index - can be re-generated but will not be used further in this section
rm(index)

##GLM model with 5-fold cross validation
tr_Ctrl <- trainControl(method = "cv",
                        number = 5)

glm_model <- train(type ~ .,
                   data = wine_train,
                   method = "glm",
                   trControl = tr_Ctrl)

##Run test data through model
preds <- predict(glm_model, newdata = wine_test)

(results_glm <- confusionMatrix(wine_test$type, preds))

##Linear SVM - 5 fold cross validation default hyperparameters
linear_SVM_model <- train(type ~ .,
                          data = wine_train,
                          method = "svmLinear2",
                          type = "C-classification",
                          trControl = tr_Ctrl)

## Plot cost vs accuracy
plot(linear_SVM_model)

##Tabulate plot vs accuracy
tibble(Cost = linear_SVM_model$results$cost, Accuracy = linear_SVM_model$results$Accuracy)

##Update results tibble with linear SVM
red_white_results <- bind_rows(red_white_results, tibble(model = "Linear SVM", Accuracy = results_SVM$overall[1]))

##Linear SVM with cartesian grid search over the cost hyperparameter
set.seed(1236, sample.kind = "Rounding")


tuned_SVM <- train(type ~ .,
                   data = wine_test,
                   method = "svmLinear2",
                   type = "C-classification",
                   trControl = tr_Ctrl,
                   tuneGrid = data.frame(cost = 2^(-3:3)))

##Plot the outcome
plot(tuned_SVM)

##Tabulate results
tibble(Cost = tuned_SVM$results$cost, Accuracy = tuned_SVM$results$Accuracy)

##Evaluate tuned SVM on test data
preds <- predict(tuned_SVM, newdata = wine_test)
(results_tuned_SVM <- confusionMatrix(wine_test$type, preds))

##Data spring cleaning before second part of the project
rm(wine_combined, wine_test, wine_train, wine_combined)  #Remove combined red and white data
rm(glm_model, linear_SVM_model, tuned_SVM) #Remove models
rm(results_glm, results_SVM, results_tuned_SVM, red_white_results) #Remove results
rm(tr_Ctrl) #Remove training control
rm(preds) #Remove predictions


#### PART 2 WINE QUALITY PREDICTION

#Convert outcome to factor
red_wine$quality <- as.factor(red_wine$quality)

white_wine$quality <- as.factor(white_wine$quality)

#Partition red wine into training and test data (70:30)

set.seed(1236, sample.kind = "Rounding")

index <- as.vector(createDataPartition(y = red_wine$quality, p = 0.3, list = FALSE))

red_wine_test <- red_wine[index,]
red_wine_train <- red_wine[-index,]

rm(index) #Is reproducible - will use again for white wine split

##Set up training control for red wine
train_red_ctrl <- trainControl(method = "cv",
                               number = 10)


##Split white wine data set
set.seed(1236, sample.kind = "Rounding")

index <- as.vector(createDataPartition(y = white_wine$quality, p = 0.2, list = FALSE))

white_wine_test <- white_wine[index,]
white_wine_train <- white_wine[-index,]

rm(index)

## Set up training control for white wine
train_white_ctrl <- trainControl(method = "cv",
                                 number = 5)

## First LDA on red wine and confusion matrix

red_wine_lda_model <- train(quality ~ .,
                            data = red_wine_train,
                            method = "lda",
                            trControl = train_red_ctrl,
                            preProcess = c("center", "scale"))

preds <- predict(red_wine_lda_model, newdata = red_wine_test)

(results_red_LDA <- confusionMatrix(red_wine_test$quality, preds))

##Start to collect results and clear old models from environment
quality_results <- tibble(Model = "LDA", Wine_type = "Red", Accuracy = results_red_LDA$overall[1])

rm(red_wine_lda_model, results_red_LDA)

##LDA on white and the confusion matrix
white_wine_lda <- train(quality ~ .,
                        data = white_wine_train,
                        method = "lda",
                        trControl = train_white_ctrl,
                        preProcess = c("center", "scale"))

preds <- predict(white_wine_lda, newdata = white_wine_test)

(results_red_lda <- confusionMatrix(white_wine_test$quality, preds))

##Update results table
