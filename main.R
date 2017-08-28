
#### Set working directory ####
path_to_data <- "/home/dominik/Dropbox/Kandidat/Managing_big/datagroup_exam/"
# ""
# ""

# Added thius line to test commit

#### Read data ####
countries <- read.csv(file = "countries.csv")


#### First look at the data ####
dim(countries) # rows and columns
str(countries) # variable and their mode.

#### Changing mode of some variables ####
# Let's convert `Country` to a character and `GDP.per.Capita` to numeric. Notice that the GDP variable has a $ sign we need to eliminate. 
countries$Country <- as.character(countries$Country)
library("stringr")
countries$GDP.per.Capita <- as.character(countries$GDP.per.Capita)
gsub(x = countries$GDP.per.Capita[1], pattern = "$", replacement = "")
gsub(x = "mig", pattern = "ig", replacement = "y")
?gsub


#### New plot #####

exciting stuff


#### dgs ####
