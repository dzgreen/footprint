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

# Tables
  # Countries with the largest and smallest ecological footprint (eg. two tables, 3 columns: rank, country, gha)
  # Other ideas?

Pie

# Case study
#### Set working directory and read data ####
path_to_data <- "/home/dominik/Dropbox/Kandidat/Managing_big/footprint"
#path_to_data <- "/Users/louisedagmarmadsen/Dropbox/Uni-noter/Kandidat/Sommerskole 2017/Managing and Analysing Cross Sectional and Spatial Data in Social Science/Exam"
path_to_data <- "e:/001gerliterati/Let?lt?sek/Summer course/project/footprint"

setwd(path_to_data)

#### Read data ####
countries <- read.csv(file = "countries.csv")


#### Data inspection and cleaning ####
dim(countries) # rows and columns
str(countries) # variable and their mode.

## Change modes and remove a $ sign ##
countries$Country <- as.character(countries$Country) # Change Country from factor to numeric
countries$GDP.per.Capita <- as.character(countries$GDP.per.Capita)
countries$GDP.per.Capita <- gsub(x = countries$GDP.per.Capita, pattern = "$", replacement = "", fixed = T) # fixed = T makes gsub understand pattern as a string insted of regex
countries$GDP.per.Capita <- gsub(x = countries$GDP.per.Capita, pattern = ",", replacement = "", fixed = T)
countries$GDP.per.Capita <- as.numeric(countries$GDP.per.Capita)

## Changing regions: Eastern/EFTA Europe ##

# Character version of Region
countries$Region1 <- as.character(countries$Region) 

countries$Region1[countries$Region1 == "Northern/Eastern Europe"] <- "Eastern Europe"
countries$Region1[countries$Region1 == "European Union"] <- "European Union/EFTA"
countries$Region1[countries$Country == "Norway"] <- "European Union/EFTA"
countries$Region1[countries$Country == "Switzerland"] <- "European Union/EFTA"

## Look for NA's ##
colSums(sapply(countries, is.na)) # Number of NA's per variable
countries[rowSums(is.na(countries)) > 0,c(1,3)] # Return countries with NA's and their population. 
# Few big countries lack data (Cambodia, Côte d'Ivoire, Finland, Korea, Norway, Somalia, Syrian Arab Republic) )
# When running different functions consider using na.rm = T. 

## Look for outliers ##
#install.packages("GGally")
library(GGally)
ggpairs(countries[,3:11])  # showing scatterplots

############################################################################################
#### 1. Distribution of the ecological footprint in the world ####
############################################################################################
library(ggplot2)

####Initial calculations####

# summing up the population of the 188 countries represented in the data
Total.Global.Population<-sum(countries$Population..millions.) 
Total.Global.Population

# adding a vector to data frame showing country level total footprint 
# as ecological footprint per capita multiplied the population of the country
countries$Country.Level.Total.Footprint<-countries$Population..millions.*countries$Total.Ecological.Footprint

#determining total global footprint as the sum of country level total footprints 
Total.Global.Footprint<-sum(countries$Country.Level.Total.Footprint)
Total.Global.Footprint

#determining global average footprint per capita as total global footprint divided by population
Average.Total.Ecological.Footprint<-Total.Global.Footprint/Total.Global.Population
Average.Total.Ecological.Footprint

# adding a vector to data frame showing country level biocapacity
# as biocapacity per capita multiplied the population of the country
countries$Country.Level.Total.Biocapacity<-countries$Population..millions.*countries$Total.Biocapacity

#determining total global biocapacity as the sum of country level biocapacities 
Total.Global.Biocapacity<-sum(countries$Country.Level.Total.Biocapacity)
Total.Global.Biocapacity
#determining global average biocapacity per capita as total global biocapacity divided by population
Average.Total.Biocapacity<-Total.Global.Biocapacity/Total.Global.Population
Average.Total.Biocapacity

#determining rate of overshoot as total global footprint divided by total global biocapacity
Rate.of.overshoot<-Total.Global.Footprint/Total.Global.Biocapacity
Rate.of.overshoot

Global.overshooting.day<-1/Rate.of.overshoot*365
Global.overshooting.day

#### 1a) What is the distribution of TEFP compared to global average footprint and biocapacity? ####

#making a histogram for footprint 
ggplot(data=countries, aes(x=Total.Ecological.Footprint))+ #determining the data set and variable used in ggplot2 functions below
  geom_histogram( fill = "#56B4E9", color = "dodgerblue2") + #colour of the bins' body and lining
  labs(x="Total Ecological Footprint per Capita", y = "Number of countries") + #labelling x and y axis
  stat_bin(aes(y=..count.., label=ifelse(..count.. > 0, ..count.., "")), geom="text", vjust=-0.5)+ # labelling bins wrt frequency on y axis hiding zeros
  ggtitle("Total Ecological Footprint per Capita in the different countries") + # adding title
  theme(plot.title = element_text(lineheight=.8, face="bold"))+ # setting title format 
  geom_vline(xintercept = Average.Total.Ecological.Footprint, linetype="dotdash")+ #adding a vertical line showing global average per capita footprint 
  geom_text(mapping=aes(x=Average.Total.Ecological.Footprint,y=0, label="Global Average Ecological Footprint per Capita = 2.80"),
            size=3, angle=90, vjust=-0.4, hjust=0, color="#CC0000")+ #labelling vertical line, setting its font size, direction, position
  geom_vline(xintercept = Average.Total.Biocapacity, linetype="dotdash")+ #adding a vertical dotdash line showing global average biocapacity per capita   
  geom_text(mapping=aes(x=Average.Total.Biocapacity,y=0, label="Global Average Biocapacity per Capita = 1.78"),
            size=3, angle=90, vjust=-0.4, hjust=0, color="#009E73") #labelling vertical dotdash line, setting its font size, direction, position

#### 1b) Do countries with similar TEFP per capita have the same effect on the global level overshooting? ####

#making a log scale histogram for the distribution of countries wrt their population

ggplot(data=countries, aes(x=Population..millions.))+ #determining the data set and variable used in ggplot2 functions below
  geom_histogram( fill = "##56B4E9", color = "dodgerblue2") + #colour of the bins' body and lining
  scale_x_continuous(trans="log10",breaks = c(1,5,10,50,100,500,1000,1500)) + # setting x axis to log scale and breaks as a numeric vector
  labs(x="Population (Millions) - Log scale ", y = "Number of countries") + #labelling x and y axis
  stat_bin(aes(y=..count.., label=..count..), geom="text", vjust=-0.5)+ # labelling bins wrt frequency on y axis
  ggtitle("Countries by population") + # adding title
  theme(plot.title = element_text(lineheight=.8, face="bold")) # setting title format 

#countries with high population weigh more in global level overshooting

#plotting cumulative distribution of global population with respect to TEFP per Capita
#the two vertical lines show the proportion of global population having a footprint of a sustainable level and an average level respectively


ggplot(data=countries[order(countries$Total.Ecological.Footprint) , ], # ordering dataframe by TEFP so that we can sum population in order of TEFP
       aes(x =  Total.Ecological.Footprint, y =  cumsum(Population..millions.)))+ #determining variables used in ggplot2 functions below
  geom_point(aes(colour=Region1))+ #adding data points to countries with different colours in different regions
  labs(x="Total Ecological Footprint per Capita", y = "Cummulative population of countries") + #labelling x and y axis
  ggtitle("Cumulative distribution of global population with respect to Total Ecological Footprint per Capita") +  # adding title
  theme(plot.title = element_text(lineheight=.8, face="bold"))+# setting title format 
  geom_text(aes(label=ifelse(Population..millions.>200,as.character(Country),'')), hjust=-0.3,vjust=0, size=2.5)+#tagging countries with highest population
  geom_vline(xintercept = Average.Total.Ecological.Footprint, linetype="dotdash")+ #adding a vertical line showing global average per capita footprint 
  geom_text(mapping=aes(x=Average.Total.Ecological.Footprint,y=0, label="Global Average Ecological Footprint per Capita = 2.80"),
            size=2.5, angle=90, vjust=2, hjust=0.1, color="#CC0000")+ #labelling vertical line, setting its font size, direction, position
  geom_vline(xintercept = Average.Total.Biocapacity, linetype="dotdash")+ #adding a vertical line showing global average biocapacity per capita   
  geom_text(mapping=aes(x=Average.Total.Biocapacity,y=0, label="Global Average Biocapacity per Capita = 1.78"),
            size=2.5, angle=90, vjust=2, hjust=0.11, color="#009E73") #labelling vertical line, setting its font size, direction, position
  

#### 1c) What drives TEFP per capita? ####

#plotting the relationship of TEFP and CFP

ggplot(data=countries,aes(x =  Total.Ecological.Footprint, y =  Carbon.Footprint))+ #determining the data set and variable used in ggplot2 functions below
  geom_point(aes(colour=Region))+ #adding data points to countries with different colours in different regions
  labs(x="Total Ecological Footprint per Capita", y = "Carbon Footprint per Capita") + #labelling x and y axis
  ggtitle("The relationship of Total Ecological Footprint per Capita and Carbon Footprint per Capita") + # adding title
  theme(plot.title = element_text(lineheight=.8, face="bold"))+# setting title format 
  geom_text(aes(label=ifelse(Carbon.Footprint>5 | (Total.Ecological.Footprint>5 & (Carbon.Footprint/Total.Ecological.Footprint<0.6)),as.character(Country),'')),
            hjust=-0.3,vjust=0, size=2.5)+#tagging countries with highest carbon footprint or high TEFP with relatively low carbon FP
  geom_smooth(method='lm',formula=y~x)+ #adding a linear regression line
  ggsave(filename="carbonplot.pdf",width=8,height=4) # saving plot to a pdf file

#plotting the relationship of TEFP and fish FP
ggplot(data=countries,aes(x =  Total.Ecological.Footprint, y =  Fish.Footprint))+
  geom_point(aes(colour=Region))+ ylim(0, 15)


#### 1c) Maps  ####
#(legends missing!!!!)

#install.packages("rworldmap")
library(rworldmap)

#making a worldmap showing TEFP

globalmap<-joinCountryData2Map(countries,
                            joinCode="NAME", 
                            nameJoinColumn="Country",
                            verbose=T)

par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i") 

global_tefp<-mapCountryData(mapToPlot=globalmap, nameColumnToPlot="Total.Ecological.Footprint",
                            catMethod=c(0,2,4,6,10),
                            colourPalette="heat",
                            mapRegion="world",
                            addLegend=F,
                            mapTitle="Total Ecological Footprint in the world",
                            aspect=1,lwd=0.5)

#making a worldmap showing CFP

global_cfp<-mapCountryData(mapToPlot=globalmap, nameColumnToPlot="Carbon.Footprint",
                           catMethod=c(0,1,2,3,4,10),
                           colourPalette="heat",
                           mapRegion="world",
                           addLegend=F,
                           mapTitle="Carbon Footprint in the world",
                           aspect=1,lwd=0.5)


############################################################################################
#### 2. Relationship between income and ecological footprint ####
############################################################################################

library(ggplot2)

#### Income scatterplots ####

# Total Ecological Footprint ~ Income, colour coded by regions
ggplot(countries, aes(x = GDP.per.Capita, y = Total.Ecological.Footprint, colour = Region1)) + geom_point() + 
  labs(title = "Relationship between GDP per Capita and Total Ecological Footprint", 
       x ="GDP per Capita ($)", y = "Total Ecological Footprint (gha)", colour = "Region")

# The plot shows a clustering of the majority of countries within the first 'square' of an Total Ecological Footprint
# below 5 gha and GDP pr. Capita below $1500. Outside of this is most European Union/Efta countries and North America
# as well as some Middle East/Central Asian countries (most likely oil producing) and Asia-Pacific.

ggplot(countries, aes(x = GDP.per.Capita, y = Biocapacity.Deficit.or.Reserve, colour = Region1)) + geom_point() + 
  labs(title = "Relationship between GDP per Capita and the Biocapacity balance", 
       x ="GDP per Capita ($)", y = "Biocapacity - Deficit or Reserve (gha)", colour = "Region")

# This plot looks into the relationship between income and biocapacity balance. Is it the case that income rich
# countries have a biocapacity deficit whilst income poor countries have surplusses? This plot does not suggest 
# such a relationship.
### -> DOMINIK MAYBE YOUR TEST/TABLE COULD GO HERE? ###


#### HDI scatterplots  ####

# We are also looking at the relationship between HDI and ecological footprint.
# As income (GDP pr. capita) is part of HDI, they are highly correlated. Still insteresting to see, if there's
# any difference.

cor(countries$GDP.per.Capita, countries$HDI, use = "complete.obs", method="kendall")
# As expected correlation is high: 0.8075072

# Total Ecological Footprint ~ HDI, colour coded by regions
ggplot(countries, aes(x = HDI, y = Total.Ecological.Footprint, colour = Region1)) + geom_point() + 
  labs(title = "Relationship between Human Development Index and Total Ecological Footprint", 
       x ="HDI", y = "Total Ecological Footprint (gha)", colour = "Region")

# The plot shows a clear regional clustering in the relationship between HDI and Total Ecological Footprint.
# In the African region there is more variation in the HDI-level, but without increases in the total footprint
# Correspondingly, countries with higher HDI seem to have higher variation in total footprint

# Total Ecological Footprint ~ HDI, different plots for each region
ggplot(countries, aes(x = HDI, y = Total.Ecological.Footprint)) + geom_point() + facet_grid(~Region1)

# A different plot showing the regional differences

#######################################################################################
#### 3. What countries have an ecological debt and what countries have surplus? ####
#######################################################################################

#install.packages("rworldmap")
library(rworldmap)
mapDevice('x11')
dfmap <- countries[,c(1,18)]
def_res_map <- joinCountryData2Map(dF = dfmap, joinCode = "NAME", nameJoinColumn = "Country", nameCountryColumn = "Country")
library(RColorBrewer) # Add better colors
colourPalette <- brewer.pal(7,'RdYlGn')
mapCountryData(def_res_map, nameColumnToPlot = "Biocapacity.Deficit.or.Reserve", colourPalette=colourPalette)

