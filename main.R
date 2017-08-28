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


#### Set working directory ####
path_to_data <- "/home/dominik/Dropbox/Kandidat/Managing_big/datagroup_exam/"
# ""
# ""


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


