#### Workflow ####
# 1) PULL others changes from remote to local
# 2) start working
# 3) COMMIT frequently, write meaningfull COMMIT messages 
# 4) When finished working, PULL others changes again, make a final COMMIT and PUSH your work to remote.

#### Outline ####
# Set working directory and read in data (Dominik)
# Data inspection (Dominik)
  # types of variables 
  # NA's
  # Outliers
  # Scatterplots
# Data cleaning (Dominik)
# Data visualisation (All)
  # 1 ) What is the distribution of the ecological footprint in the world? (Gary)
  # 1a) Histograms 
  # 1b) Map
  # 2 ) what is the relationship between income and HDI and ecological foorprint in the world? (Louise)
  # 2a) Scatterplot, correlation coefficient
  # 3 ) What countries have an ecological debt and what countries have surplus? (Dominik)
  # 3a) Map



lapply(pack, require, character.only=T)

#### Set working directory ####
path_to_data <- "/home/dominik/Dropbox/Kandidat/Managing_big/datagroup_exam/"
#path_to_data <- "/Users/louisedagmarmadsen/Dropbox/Uni-noter/Kandidat/Sommerskole 2017/Managing and Analysing Cross Sectional and Spatial Data in Social Science/Exam"
# ""
setwd(path_to_data)

#### Read data ####
countries <- read.csv(file = "countries.csv")


#### Data inspection ####
dim(countries) # rows and columns
str(countries) # variable and their mode.


#### Changing type of some variables ####
countries$Country <- as.character(countries$Country)
countries$GDP.per.Capita <- as.character(countries$GDP.per.Capita)
countries$GDP.per.Capita <- gsub(x = countries$GDP.per.Capita, pattern = "[$]", replacement = "")
countries$GDP.per.Capita <- gsub(x = countries$GDP.per.Capita, pattern = "[,]", replacement = "")
countries$GDP.per.Capita <- as.numeric(countries$GDP.per.Capita)


#### 2. Relationship between income and ecological footprint ####
# We are also creating the 

# Simple scatterplots #


plot(countries$GDP.per.Capita, countries$Total.Ecological.Footprint, main = "Relationship between income and total ecological footprint",
     xlab = "Income", ylab = "Total ecological footprint", pch=19)

cor(countries$GDP.per.Capita, countries$HDI, use = "complete.obs", method="kendall")

plot(countries$HDI, countries$Total.Ecological.Footprint, main = "Relationship between income and total ecological footprint",
     xlab = "Income", ylab = "Total ecological footprint", pch=19)

