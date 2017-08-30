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

#Initial calculations

Total.Global.Population<-sum(countries$Population..millions.)
Total.Global.Population

countries$Country.Level.Total.Footprint<-countries$Population..millions.*countries$Total.Ecological.Footprint
Total.Global.Footprint<-sum(countries$Country.Level.Total.Footprint)
Total.Global.Footprint
Average.Total.Ecological.Footprint<-Total.Global.Footprint/Total.Global.Population
Average.Total.Ecological.Footprint

countries$Country.Level.Total.Biocapacity<-countries$Population..millions.*countries$Total.Biocapacity
Total.Global.Biocapacity<-sum(countries$Country.Level.Total.Biocapacity)
Total.Global.Biocapacity
Average.Total.Biocapacity<-Total.Global.Biocapacity/Total.Global.Population
Average.Total.Biocapacity

Rate.of.overuse<-Total.Global.Footprint/Total.Global.Biocapacity
Rate.of.overuse
Global.overshooting.day<-1/Rate.of.overuse*365
Global.overshooting.day


#making a histogram for footprint 
#adding a vertical line showing global per capita biocap and/or average of footprint would be nice 
ggplot(data=countries, aes(x=Total.Ecological.Footprint))+
  geom_histogram( fill = "#E69F00", color = "dodgerblue2") + #colour of the bins' body and the lining
  labs(x="Total Ecological Footprint per Capita", y = "Number of countries") + #labelling x and y axis
  stat_bin(aes(y=..count.., label=..count..), geom="text", vjust=-0.5)+ # labelling bins wrt frequency on y axis
  ggtitle("Total Ecological Footprint per Capita in the different countries") + # adding title
  theme(plot.title = element_text(lineheight=.8, face="bold"))+ # setting title format 
  geom_vline(xintercept = Average.Total.Ecological.Footprint)+
  geom_text(mapping=aes(x=Average.Total.Ecological.Footprint,y=0, label="Global Average Ecological Footprint per Capita = 2.80"), size=3, angle=90, vjust=-0.4, hjust=0)+
  geom_vline(xintercept = Average.Total.Biocapacity)+
  geom_text(mapping=aes(x=Average.Total.Biocapacity,y=0, label="Global Average Biocapacity per Capita = 1.78"), size=3, angle=90, vjust=-0.4, hjust=0)

#### 1a) Do countries in the same bin have the same the effect on the global level overshooting? ####

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
  geom_point(aes(colour=Region1))+
  labs(x="Total Ecological Footprint per Capita", y = "Cummulative population of countries") +
  ggtitle("Cumulative distribution of global population
  with respect to Total Ecological Footprint per Capita") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))+
  geom_text(aes(label=ifelse(Population..millions.>200,as.character(Country),'')),
            hjust=-0.3,vjust=0, size=2.5)

#### 1.b) What drives TEFP per capita? ####

#plotting the relationship of TEFP and CFP

ggplot(data=countries,aes(x =  Total.Ecological.Footprint, y =  Carbon.Footprint))+
  geom_point(aes(colour=Region))+
  labs(x="Total Ecological Footprint per Capita", y = "Carbon Footprint per Capita") +
  ggtitle("The relationship of Total Ecological Footprint per Capita and Carbon Footprint per Capita") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))+
  geom_text(aes(label=ifelse(Carbon.Footprint>5 | (Total.Ecological.Footprint>5 & (Carbon.Footprint/Total.Ecological.Footprint<0.6)),as.character(Country),'')),
            hjust=-0.3,vjust=0, size=2.5)+
  geom_smooth(method='lm',formula=y~x)+
  ggsave(filename="carbonplot.pdf",width=8,height=4)

#plotting the relationship of TEFP and FFP
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

# We are also looking at the relationship between HDI and ecological footprint.
# As income (GDP pr. capita) is part of HDI, they are highly correlated. Still insteresting to see, if there's
# any difference.

cor(countries$GDP.per.Capita, countries$HDI, use = "complete.obs", method="kendall")
# As expected correlation is high: 0.8075072

#### HDI scatterplots  ####
# Total Ecological Footprint ~ HDI, colour coded by regions
ggplot(countries, aes(x = HDI, y = Total.Ecological.Footprint, colour = Region1)) + geom_point() + 
  labs(title = "Relationship between Human Development Index and Total Ecological Footprint", 
       x ="HDI", y = "Total Ecological Footprint (gha)", colour = "Region")
# Interesting that many (African) countries have HDI increases with now footprint increase
# Countries with higher HDI seem to have higher variation in total footprint

# Total Ecological Footprint ~ HDI, different plots for each region
ggplot(countries, aes(x = HDI, y = Total.Ecological.Footprint)) + geom_point() + facet_grid(~Region1)


#### Income scatterplots ####

# Total Ecological Footprint ~ Income, colour coded by regions
ggplot(countries, aes(x = GDP.per.Capita, y = Total.Ecological.Footprint, colour = Region1)) + geom_point() + 
  xlab ("GDP per Capita ($)") + ylab ("Total Ecological Footprint (gha)") + ggtitle ("Relationship between income and total ecological footprint")


ggplot(countries, aes(x = GDP.per.Capita, y = Biocapacity.Deficit.or.Reserve, colour = Region1)) + geom_point() + 
  xlab ("GDP per Capita ($)") + ylab ("Biocapacity.Deficit.or.Reserve") + ggtitle ("Relationship between income and total ecological footprint") + ylim(-10,20)

ggplot(countries, aes(x = GDP.per.Capita, y = Biocapacity.Deficit.or.Reserve)) + geom_point() + facet_grid(~Region1)

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

