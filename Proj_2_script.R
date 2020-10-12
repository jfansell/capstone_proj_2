## Check for the required libraries and if neccessary install them

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

## Call libraries
library(tidyverse)
library(caret)

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
