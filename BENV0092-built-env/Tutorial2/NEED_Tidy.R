setwd("S:/ENERGINST_ESDA/ESDA DATASETS/NEED data")
need<-read.table("S:/ENERGINST_ESDA/ESDA DATASETS/NEED data/reduced records/need_public_use_file_2014.csv",header = TRUE,sep=",")
#delete the columns i dont need
summary(need)
need <- subset( need, select = -c(6, 8, 10,12,14,16,18,20,22,24,26,28,30,32,34,36 ) )
#install.packages("reshape")

library(tidyverse)
library(reshape)
library(data.table)
library(plyr) 
library(psych)
library(Hmisc)


need_tidy <- melt(setDT(need), measure.vars = patterns("Gcons", "Econs"), 
                  value.name = c("Gas_Cons", "Electricity_Cons"))
need_tidy <-need_tidy [, Year := forcats::lvls_revalue(variable, c("2005", "2006", "2007", "2008","2009", "2010", "2011", "2012" ))][]
#https://stackoverflow.com/questions/23945350/reshaping-wide-to-long-with-multiple-values-columns
#https://stackoverflow.com/questions/44895273/converting-data-from-wide-to-long-for-two-variables


#check for outliers
#Adan and Fuerst (2016): valid records defined as properties with electricity consumption value between 100 and 25,000 kWh 
#and gas consumption value between 3000 and 50,000 kWh

need_tidy<- subset(need_tidy, Electricity_Cons >= 100 & Electricity_Cons <= 25000&Gas_Cons>=3000&Gas_Cons<=50000)
summary(need_tidy)
str(need_tidy)
need_tidy <- within(need_tidy, {
  HH_ID<- factor(HH_ID)
  PROP_AGE <- factor(PROP_AGE)
  REGION<- factor(REGION)
  PROP_TYPE<- factor(PROP_TYPE)
  EE_BAND<- factor(EE_BAND)
  FLOOR_AREA_BAND<-factor(FLOOR_AREA_BAND)
  CWI_YEAR<-factor(CWI_YEAR)
  LI_YEAR<-factor(LI_YEAR)
  BOILER_YEAR<-factor(BOILER_YEAR)
  MAIN_HEAT_FUEL<-factor(MAIN_HEAT_FUEL)
  Year<- factor(Year)
  
})
summary(need_tidy)
#there are 4454 dwellings that are not connected to Gas. These should be omitted from analysis on Gas. Lets set their Gas values to NA
need_tidy$Gas_Cons<-ifelse(need_tidy$MAIN_HEAT_FUEL==2,NA,need_tidy$Gas_Cons)  

#we should change the values to meaningful names: PLYR PACKAGE

need_tidy$REGION<-revalue(need_tidy$REGION, c("E12000001"="North East", "E12000002"="North West","E12000003"="Yorksh. and Humber","E12000004"="Midlands", "E12000005"="Midlands","E12000006"="East of England", "E12000007"="London","E12000008"="South East","E12000009"="South West","W99999999"="Wales"))
need_tidy$PROP_AGE<-revalue(need_tidy$PROP_AGE, c("101"="before 1930", "102"="1930-1949 ","103"="1950-1966", "104"="1967-1982","105"="1983-1995","106"="1996 onwards"))
need_tidy$PROP_TYPE<-revalue(need_tidy$PROP_TYPE, c("101"="Detached", "102"="Semi-detached","103"="End terrace", "104"="Mid terrace","105"="Bungalow","106"="Flat"))
need_tidy$FLOOR_AREA_BAND<-revalue(need_tidy$FLOOR_AREA_BAND, c("1"="1 to 50", "2"="51-100 ","3"="101-150", "4"="Over 151"))
need_tidy$EE_BAND<-revalue(need_tidy$EE_BAND, c("1"="A/B", "2"="C ","3"="D", "4"="E","5"="F","6"="G"))

#some descriptives
str(need_tidy)#shows the class of each var

describe(need_tidy)
head(need_tidy) # First 6 rows of dataset
rowSums(is.na(need_tidy)) # Number of missing per row, shows blank cells
colSums(is.na(need_tidy))# Number of missing per column, shows blank cells


#summary stats per group
#1. using 'describeby'
#Households in the 20 per cent most deprived LSOAs are in the bottom quintile (1)
#households in the 20 per cent least deprived LSOAs are in the top quintile (5)
describeBy(need_tidy$Gas_Cons, need_tidy$IMD_ENG)#gas consumption by deprivation index
describeBy(need_tidy$Electricity_Cons, need_tidy$IMD_ENG)#electricity consumption by deprivation index
describeBy(need_tidy$Electricity_Cons, need_tidy$PROP_TYPE)
#hhs in most deprived areas have lower mean gas and electricity consumption
describeBy(need_tidy$Gas_Cons, need_tidy$REGION) 
describeBy(need_tidy$Gas_Cons, need_tidy$Year) 

#gas consumption by region. we would expect houses in the North to consume more gas on average



#2. using graphs
ggplot(need_tidy, aes(PROP_AGE)) + 
  geom_bar(aes(y = (..count..)),fill="orange", alpha=0.6, na.rm = TRUE) + 
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="darkgreen") + 
  theme_bw() +
  xlab("PROPERTY AGE") +
  ylab("Frequency") + 
  ggtitle("Distribution of PROPERTY AGE in the sample")
# MORE OLDER BUILDINGS IN SAMPLE (BEFORE 1930)
ggplot(need_tidy, aes(PROP_TYPE)) + 
  geom_bar(aes(y = (..count..)),fill="gray", alpha=0.6) + 
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="darkgreen") + 
  theme_bw() +
  xlab("PROPERTY TYPE") +
  ylab("Frequency") + 
  ggtitle("Distribution of PROPERTY TYPE in the sample")
#many semi-detached houses in sample

ggplot(need_tidy, aes(REGION)) + 
  geom_bar(aes(y = (..count..)),fill="gray", alpha=0.6) + 
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="darkgreen") + 
  theme_bw() +
  xlab("REGION") +
  ylab("Frequency") + 
  ggtitle("Distribution of REGIONS in the sample")


#how gas consumption differs by property type

p1<-ggplot(need_tidy, aes(x = PROP_TYPE, y = Gas_Cons)) +
  geom_violin() +
  geom_boxplot()+
  labs(subtitle="Gas Consumption by Property Type", 
       y="Gas Consumption", 
       x="Property type" )
p1

#as expected, detached houses have the highest gas consumption while flats the lowest

#how gas consumption differs by area
p2<-ggplot(need_tidy, aes(x = REGION, y = Gas_Cons)) +
  geom_violin() +
  geom_boxplot()+
  labs(subtitle="Gas Consumption by Region", 
       y="Gas Consumption", 
       x="Region" )
p2
#Northern areas have slightly higher mean gas consumption

#how gas consumption differs by property age
p3<-ggplot(need_tidy, aes(x = PROP_AGE, y = Gas_Cons)) +
  geom_violin() +
  geom_boxplot()+
  labs(subtitle="Gas Consumption by Property Age", 
       y="Gas Consumption", 
       x="Property Age" )
p3
#OLDER homes seem to use more gas

#how gas consumption differs by floor area
p4<-ggplot(need_tidy, aes(x = FLOOR_AREA_BAND, y = Gas_Cons)) +
  geom_violin() +
  geom_boxplot()+
  labs(subtitle="Gas Consumption by Floor Area", 
       y="Gas Consumption", 
       x="Area band" )
p4

#how gas consumption differs by EPC Band
p5<-ggplot(subset(need_tidy,EE_BAND!="G"), aes(x = EE_BAND, y = Gas_Cons)) +
  geom_violin() +
  geom_boxplot()+
  labs(subtitle="Gas Consumption by EPC Band", 
       y="Gas Consumption", 
       x="EPC Band" )
p5
ggplot(aes(y = Gas_Cons, x = FLOOR_AREA_BAND, fill = EE_BAND), data = need_tidy) + geom_boxplot()

write.csv(need_tidy, file = "NEED_tidy.csv",row.names=FALSE)