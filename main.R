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
# Data visualisation (All)
  # 1 ) What is the distribution of the ecological footprint in the world? (Gary)
  # 1a) Histograms
  # 1b) Map
  # 2 ) what is the relationship between income and HDI and ecological foorprint in the world? (Louise)
  # 2a) Scatterplot, correlation coefficient
  # 3 ) What countries have an ecological debt and what countries have surplus? (Dominik)
  # 3a) Map

#### Reading packages ####

# Insert packages that you use - and potentially delete the ones we don't use
pack<-c("car","sandwich","lmtest","RColorBrewer","mgcv","foreign","xtable"
        ,"AER","stargazer", "MASS", "ggplot2")

lapply(pack, require, character.only=T)

#### Set working directory and read data ####
path_to_data <- "/home/dominik/Dropbox/Kandidat/Managing_big/footprint"
#path_to_data <- "/Users/louisedagmarmadsen/Dropbox/Uni-noter/Kandidat/Sommerskole 2017/Managing and Analysing Cross Sectional and Spatial Data in Social Science/Exam"
path_to_data <- "e:/001gerliterati/Letöltések/Summer course/project/footprint"
setwd(path_to_data)

#### Read data ####
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
countries[rowSums(is.na(countries)) > 0,c(1,3)] # Return countries with NA's and their population. Few big countries lack data (Cambodia, CÃ´te d'Ivoire, Finland, Korea, Norway, Somalia, Syrian Arab Republic) )
# When running different function consider using na.rm = T. 

# Look for outliers
#install.packages("GGally")
library(GGally)
ggpairs(countries[,3:11])  # showing scatterplots

############################################################################################
#### 1. Distribution of the ecological footprint in the world ####
############################################################################################
# 1 ) What is the distribution of the ecological footprint in the world? (Gary)

#making a histogram for footprint 
#adding a vertical line showing global per capita biocap would be nice 
ggplot(data=countries, aes(x=Total.Ecological.Footprint))+
  geom_histogram( fill = "#E69F00", color = "dodgerblue2") + #colour of the bins' body and the lining
  labs(x="Total Ecological Footprint per Capita", y = "Number of countries") + #labelling x and y axis
  stat_bin(aes(y=..count.., label=..count..), geom="text", vjust=-0.5)+ # labelling bins wrt frequency on y axis
  ggtitle("Total Ecological Footprint per Capita in the different countries") + # adding title
  theme(plot.title = element_text(lineheight=.8, face="bold")) # setting title format 

# 1.1 Do countries in the same bin have the same the effect on the global level overshooting?

#making a log scale histogram for the distribution of countries wrt their population

ggplot(data=countries, aes(x=Population..millions.))+
  geom_histogram( fill = "#E69F00", color = "dodgerblue2") + 
  scale_x_continuous(trans="log10",breaks = c(1,5,10,50,100,500,1000,1500)) +
  labs(x="Population (Millions) - Log scale ", y = "Number of countries") + 
  stat_bin(aes(y=..count.., label=..count..), geom="text", vjust=-0.5)+
  ggtitle("Countries by population") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))

#countries with highest population weigh more 

#plotting cumulative distribution of global population with respect to TEFP per Capita
#a vertical line  would show the proportion of global population living on a footprint of an agreeable level 

ggplot(data=countries,aes(x =  sort(Total.Ecological.Footprint), y =  cumsum(Population..millions.)))+
  geom_point(aes(colour=Region))+
  labs(x="Total Ecological Footprint per Capita", y = "Cummulative population of countries") +
  ggtitle("Cumulative distribution of global population
  with respect to Total Ecological Footprint per Capita") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))+
  geom_text(aes(label=ifelse(Population..millions.>200,as.character(Country),'')),
            hjust=-0.3,vjust=0, size=2.5)

############################################################################################
#### 2. Relationship between income and ecological footprint ####
############################################################################################

# We are also looking at the relationship between HDI and ecological footprint.
# As income (GDP pr. capita) is part of HDI, they are highly correlated. Still insteresting to see, if there's
# any difference.

cor(countries$GDP.per.Capita, countries$HDI, use = "complete.obs", method="kendall")
# As expected correlation is high: 0.8075072

#### Simple scatterplots ####
plot(countries$GDP.per.Capita, countries$Total.Ecological.Footprint, main = "Relationship between income and total ecological footprint",
     xlab = "Income", ylab = "Total ecological footprint", pch=19)
abline(lm(countries$Total.Ecological.Footprint~countries$GDP.per.Capita), col="red")

plot(countries$HDI, countries$Total.Ecological.Footprint, main = "Relationship between Human Development Index and total ecological footprint",
     xlab = "HDI", ylab = "Total ecological footprint", pch=19)
abline(lm(countries$Total.Ecological.Footprint~countries$HDI), col="red")


#### Changing regions: Eastern/Western Europe ####

# Character version of Region
countries$Region1 <- as.character(countries$Region) 

countries$Region1[countries$Region1 == "Northern/Eastern Europe"] <- "Eastern Europe"
countries$Region1[countries$Region1 == "European Union"] <- "Western Europe"
countries$Region1[countries$Country == "Norway"] <- "Western Europe"
countries$Region1[countries$Country == "Switzerland"] <- "Western Europe"


#### HDI scatterplots  ####

# Total Ecological Footprint ~ HDI, colour coded by regions
ggplot(countries, aes(x = HDI, y = Total.Ecological.Footprint, colour = Region1)) + geom_point() + 
  xlab ("HDI") + ylab ("Total Ecological Footprint (gha)") + ggtitle ("Relationship between Human Development Index and total ecological footprint")

ggplot(countries, aes(x = HDI, y = Total.Ecological.Footprint, colour = Region1)) + geom_point() + 
  labs(title = "Relationship between Human Development Index and Total Ecological Footprint", x ="HDI", y = "Total Ecological Footprint (gha)", colour = "Region")

# Total Ecological Footprint ~ HDI, different plots for each region
ggplot(countries, aes(x = HDI, y = Total.Ecological.Footprint)) + geom_point() + facet_grid(~Region1)

# Total Ecological Footprint ~ HDI, colour coded by regions - line for each region
ggplot(countries, aes(x = HDI, y = Total.Ecological.Footprint, colour = Region1)) + geom_point() +   
  stat_smooth(method = 'nls', formula = 'y~exp(a*x+b)',
              method.args = list(start=c(a=0.1, b=0)), se=FALSE) + 
  labs(title = "Relationship between Human Development Index and Total Ecological Footprint", 
       x ="HDI", y = "Total Ecological Footprint (gha)", colour = "Region")


# Plot which makes one non-linear line for countries  
ggplot(countries, aes(x = HDI, y = Total.Ecological.Footprint)) + 
  geom_point() +   stat_smooth(method = 'nls', formula = 'y~exp(a*x+b)',
                               method.args = list(start=c(a=0.1, b=0)), se=FALSE) +
  labs(title = "Relationship between Human Development Index and Total Ecological Footprint", x ="HDI", y = "Total Ecological Footprint (gha)")


#### Income scatterplots ####

### Creating income intervals ###
# Based on WB distinctions:  
# Low income (L) 1005
# Lower middle income (LM) 1006-3955
# Upper middle income (UM) 3956-12235
# High income (H) > 12235

countries$income.intervals[countries$GDP.per.Capita <= 1005] <- "Low income"
countries$income.intervals[countries$GDP.per.Capita > 1006 & countries$GDP.per.Capita <= 3955] <- "Lower middle income"
countries$income.intervals[countries$GDP.per.Capita > 3956 & countries$GDP.per.Capita <= 12235] <- "Upper middle income"
countries$income.intervals[countries$GDP.per.Capita > 12235] <- "High income"

# Total Ecological Footprint ~ Income, colour coded by regions
ggplot(countries, aes(x = GDP.per.Capita, y = Total.Ecological.Footprint, colour = Region1)) + geom_point() + 
  xlab ("GDP per Capita ($)") + ylab ("Total Ecological Footprint (gha)") + ggtitle ("Relationship between income and total ecological footprint")

# Total Ecological Footprint ~ Income, colour coded by income interval # <- pretty obvious when it's run...
ggplot(countries, aes(x = GDP.per.Capita, y = Total.Ecological.Footprint, colour = income.intervals)) + geom_point() + 
  xlab ("GDP per Capita ($)") + ylab ("Total Ecological Footprint (gha)") + ggtitle ("Relationship between income and total ecological footprint")

#######################################################################################

#######################################################################################
#### 3. What countries have an ecological debt and what countries have surplus? ####

#install.packages("rworldmap")
library(rworldmap)
mapDevice('x11')
dfmap <- countries[,c(1,18)]
def_res_map <- joinCountryData2Map(dF = dfmap, joinCode = "NAME", nameJoinColumn = "Country", nameCountryColumn = "Country")
library(RColorBrewer) # Add better colors
colourPalette <- brewer.pal(7,'RdYlGn')
mapCountryData(def_res_map, nameColumnToPlot = "Biocapacity.Deficit.or.Reserve", colourPalette=colourPalette)

