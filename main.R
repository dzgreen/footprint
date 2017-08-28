# Analysis plan: 2016 Global Ecological Footprint
# Does your country consume more resources than it produces in a year?
# 
# 
#### Research question: ####
#
# Inspiration:
# Is your country running an ecological deficit, consuming more resources than it can produce per year? 
# Which countries have the greatest ecological deficits or reserves? 
# Do they consume less or produce more than the average country? 
# When will Earth Overshoot Day, the day on the calendar when humanity has used one year of natural resources,
# occur in 2017?
# 
# 
#### Hypotheses: ####
#   Have at least one hypothesis for each major analyses undertaken and 
#   several hypotheses for each multivariate regression analysis 
# 
# -	
# -	
#   
####   Data cleaning process: ####
#   -	Transformation of variables?
# -	
#   
####   Definition and construction of the following: ####
# -	Main exposure variables (if relevant)
# -	Outcome variables 
# -	Other covariates including potential confounders and mediators 
# -	Definition of groups if planning to examine variations between sub-groups of respondents 
# -	Procedures used for dealing with missing data
#     o	Complete case analysis 
#     o	Coding missing values as separate categories 
#     o	Imputation/extrapolation methods 
# 
#### Data exploration: ####
#   -	Outliers (Y – X) – boxplot and Cleavland dotplot 
#   -	Homogeneity(X)–Conditional boxplot
#   -	Normality (Y) – histogram or QQ plot
#   -	Zero over dispersion (Y) – Frequency plot or corrgram
#   -	Collinearity (X) – VIF, scatterplots, correlations and PCA
#   -	Relationships (Y and X) – scatterplots, conditional boxplot
#   -	Interactions – coplots
#   -	Independence (Y) – ACF and variogram, plot Y vs. time/space 
# 
# 
#### Analytical approach: ####
#   Planned analysis:
#     -	Statistical tests for testing different hypotheses
# -	Planned tables and figures
# -	Preliminary/basis analysis model
# 







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
noget <- gsub(x = countries$GDP.per.Capita[1], pattern = "$", replacement = "")
gsub(x = noget, pattern = "$", replacement = "")
?gsub


#### New plot #####

exciting stuff


#### dgs ####
