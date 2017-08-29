#### Workflow ####
# 1) PULL others changes from remote to local
# 2) start working
# 3) COMMIT frequently, write meaningfull COMMIT messages 
# 4) When finished working, PULL others changes again, make a final COMMIT and PUSH your work to remote.

#### Outline ####
# Set working directory and read in data (Dominik)
# Data inspection and cleaning (Dominik)
  # types of variables 
  # NA's
  # Outliers
  # Scatterplots
# Data visualisation (All)
  # 1 ) What is the distribution of the ecological footprint in the world? (Gary)
  # 1a) Histograms 
  # 1b) Map
  # 2 ) what is the relationship between income and HDI and ecological foorprint in the world? (Louise)
  # 2a) Scatterplot, correlation coefficient
  # 3 ) What countries have an ecological debt and what countries have surplus? (Dominik)
  # 3a) Map


#### Set working directory and read data ####
path_to_data <- "/home/dominik/Dropbox/Kandidat/Managing_big/datagroup_exam/"
# ""
# ""
countries <- read.csv(file = "countries.csv")


#### Data inspection and cleaning ####
dim(countries) # rows and columns
str(countries) # variable and their mode.

# Change modes and remove a $ sign
countries$Country <- as.character(countries$Country) # Change Country from factor to numeric
countries$GDP.per.Capita <- as.character(countries$GDP.per.Capita) 
countries$GDP.per.Capita <- gsub(x = countries$GDP.per.Capita, pattern = "$", replacement = "", fixed = T) # fixed = T makes gsub understand pattern as a string insted of regex
countries$GDP.per.Capita <- gsub(x = countries$GDP.per.Capita, pattern = ",", replacement = "", fixed = T)
countries$GDP.per.Capita <- as.numeric(countries$GDP.per.Capita)

# Look for NA's
colSums(sapply(countries, is.na)) # Number of NA's per variable
countries[rowSums(is.na(countries)) > 0,c(1,3)] # Return countries with NA's and their population















