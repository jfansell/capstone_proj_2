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

