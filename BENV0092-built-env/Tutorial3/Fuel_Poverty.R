#Tutorial 3 - Fuel Poverty Data and Logistic Regression

#Aim of this Tutorial
#1. Explore the Fuel Poverty Dataset, part of the English Housing Survey.
#2. Fit a logistic regression model that predicts whether a household is in fuel poverty or not
#using the data that we have. Assess the fit of the model.

#Dataset constructed from fieldwork carried out between April 2014 and March 2016
#For full description of the variables, visit: http://doc.ukdataservice.ac.uk/doc/8228/mrdoc/pdf/8228_2015_fuel_poverty_dataset_documentation.pdf
fp<-read.csv("S:/ENERGINST_ESDA/ESDA DATASETS/English Housing Survey_Fuel Poverty/fuel_poverty_2015_eul.csv",header = TRUE,sep=",")
#Logistic regression is a method for fitting a regression curve, y = f(x), when y is a categorical variable. 
#The typical use of this model is predicting y given a set of predictors x. 
#The predictors can be continuous, categorical or a mix of both.
View(fp)
#our variable names are terrible, lets change them to meaningful names
library(plyr) #the rename function produces an error if I load both dplyr and plyr, so avoid loading tidyverse here
library(ggplot2)
fp<-rename(fp, c("fpLIHCflg"="In_fuel_Poverty", "fpfullinc"="hh_income","fuelexpn"="fuel_costs", 
             "Unoc"="Under_occupied","gorEHS"="Region", "tenure4x"="Tenure","emphrp3x"="Head_Working_Status",
             "ethhrp2x"="Head_Ethnic_Origin","Ageyng"="Age_of_youngest","Ageold"="Age_of_oldest"))

#I will delete variables I will not be using:
fp <- subset(fp , select = -c(3:5, 7:11,14:17,20:22,25:28,44:45,48:53) )
summary(fp)
#I will omit the under occupied homes, as the costs will not reflect full occupancy
fp<-subset(fp,fp$Under_occupied=="Not under occupying")

#here our binary response variable (In Fuel Poverty) should be converted to a dummy 0/1 variable
fp$In_fuel_Poverty<-ifelse((fp$In_fuel_Poverty=="Not in fuel poverty - low income high costs measure"),0,1)

str(fp)#In_fuel_Poverty should be a factor and not 'numeric'
fp$In_fuel_Poverty<-as.factor(fp$In_fuel_Poverty)

#Lets explore our sample using graphs
p1<-ggplot(fp, aes(x=Region))+
  geom_bar(stat="count", width=0.7, fill="steelblue")
p1

p2<-ggplot(fp, aes(x=Tenure))+
  geom_bar(stat="count", width=0.7, fill="steelblue")
p2
#housing association and local authority tenures are usually Social housing
#I will exlude these from my sample, I want to explore private rented and owner occupied dwellings this time
fp<-subset(fp,fp$Tenure=="owner occupied"|fp$Tenure=="private rented")

p3<-ggplot(fp, aes(x=DWtype))+
  geom_bar(stat="count", width=0.7, fill="steelblue")
p3#there is a category for converted and non residential. I dont want these in my sample, as we are only
#interested in residential properties

#Delete these from your dataframe!
fp<-subset(fp,!fp$DWtype=="converted and non-residential")


p4<-ggplot(fp, aes(x=FloorArea))+
  geom_bar(stat="count", width=0.7, fill="steelblue")
p4 


p5<-ggplot(fp, aes(x=Mainfueltype))+
  geom_bar(stat="count", width=0.7, fill="steelblue")
p5

p6<-ggplot(fp, aes(x=hhcompx))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_minimal()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
p6

p7<-ggplot(fp, aes(x=Head_Working_Status))+
  geom_bar(stat="count", width=0.7, fill="steelblue")
p7

p8<-ggplot(fp, aes(x=Head_Ethnic_Origin))+
  geom_bar(stat="count", width=0.7, fill="steelblue")
p8

p9<-ggplot(fp, aes(x=hhsize))+
  geom_bar(stat="count", width=0.7, fill="steelblue")
p9

p10<-ggplot(fp, aes(x=EPC))+
  geom_bar(stat="count", width=0.7, fill="steelblue")
p10

p11<-ggplot(fp, aes(x=DWage))+
  geom_bar(stat="count", width=0.7, fill="steelblue")
p11 # too many bins-we should probably aggregate the groups
#lets fix Dwelling Age groupings
str(fp$DWage)
fp$DWage[fp$DWage == "Pre 1850"] <- "Pre 1919"
fp$DWage[fp$DWage == "1850 to 1899"] <- "Pre 1919"
fp$DWage[fp$DWage == "1900 to 1918"] <- "Pre 1919"
fp$DWage[fp$DWage == "1965 to 1974"] <- "1965 to 1980"
fp$DWage[fp$DWage == "1975 to 1980"] <- "1965 to 1980"

p11<-ggplot(fp, aes(x=factor(DWage)))+
  geom_bar(stat="count", width=0.7, fill="steelblue")
p11 #Age is not displayed in the order that I would prefer. Run the following code and re-run the plot:
fp$DWage <- factor(fp$DWage,levels = c("Pre 1919", "1919 to 1944", "1945 to 1964","1965 to 1980", "1981 to 1990", "Post 1990"))

summary(fp$hh_income) 

p12<-ggplot(fp, aes(x=Tenure,y=hh_income))+
  geom_boxplot( fill="steelblue")
p12

p13<-ggplot(fp, aes(x=hh_income))+
  geom_histogram()
p13
#I am getting a minimum income of -1194 and a Median of around 29000. What should I use as minimum acceptable 
#Income value? I will use the £8000 minimum pension/year.
fp<-subset(fp,fp$hh_income>8000)

summary(fp$fuel_costs)
p14<-ggplot(fp, aes(x=fuel_costs))+
  geom_histogram()
p14

#there are a couple of observations located quite far away from the rest - will be disregarded as outliers
fp<-subset(fp,fp$fuel_costs<5000)


###        End of Part 1 Video
###        Start of Part 2 Video

#I want to check whether my numerical variables are correlated (Collinearity problem). 
#install.packages("corrplot")
library(corrplot)
correlations <- cor(fp[,4:5],use="pairwise.complete.obs")
corrplot(correlations, method="number") #for more, see: https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html)
#we see that hh_income and fuel_costs have a low correlation (<0.3). We can still safely include both variables
#in the same model!

str(fp)
fp$EPC<-as.factor(fp$EPC)
fp$DWtype<-as.factor(fp$DWtype)

#lets fit our fist model

mylog<- glm(In_fuel_Poverty ~  hh_income , data = fp, family = "binomial") #for more, see: https://www.statmethods.net/advstats/glm.html
summary(mylog)
#How do we interprete this result?
#logistic regression coefficients give the change in the log odds of the outcome for a one unit increase 
#in the predictor variable
#For every one unit increase in income, the log odds of being in fuel poverty (versus not being) 
#decreases by 8.7 * 10^(-5)=0.000087

#Alternatively, the ODDS of being in fuel poverty (versus not being) 
#change by e^(-0.000087)=0.999, which means they actually DECREASE by 1-0.999=0.001=0.1%
#Odds can range from 0 to infinity. 
#In other words, as income of the household increases, the odds of it experiencing fuel poverty decrease.

#Lets look at Deviance:a measure of goodness of fit of a model. Higher numbers always indicate bad fit.
#Null Deviance
#shows how well the response variable is predicted by a model that includes only the intercept (grand mean)
#Residual Deviance: shows how well the response variable is predicted by a model that includes predictors

#The z-value is the regression coefficient divided by its standard error.
#If the z-value is big in magnitude (either positive or negative), 
#it indicates that the corresponding regression coefficient is not 0 and the corresponding X-variable matters.

#AIC: Akaike Information Criterion: does not tell me anything if I am just looking at one model
#I care about how it changes as we add one variable at a time.
#If it decreases, it means that my fit is improving

mylogit2<- glm(In_fuel_Poverty ~ hh_income + fuel_costs, data =fp, family = "binomial")
summary(mylogit2)
mylogit3<- glm(In_fuel_Poverty ~  hh_income + fuel_costs + EPC, data = fp, family = "binomial")
summary(mylogit3)
mylogit4<- glm(In_fuel_Poverty ~  hh_income + fuel_costs + EPC + DWtype, data = fp, family = "binomial")
summary(mylogit4)
coef(mylogit4)
#Create another model, including Dwelling Age as a feature. What did you discover?

#Lets interprete the results of my final model, mylogit4
#For every one unit increase in income, the log odds of being in fuel poverty (versus not being) 
#decreases by 0.0002. 
#For every one unit increase in fuel costs, the log odds of being in fuel poverty (versus not being) 
#increases by 0.005. 
#Interpreting the estimates for EPC, which is a categorical variable is a bit different:
summary(fp$EPC)#R has chosen EPC A/B/C as the reference case - it compares all categories to that

#Belonging to EPC Band D versus belonging to A/B/C increases the log odds of being in fuel poverty (versus not being) 
#by 0.77


summary(fp$DWtype)#R has chosen Detached as the reference case (DWtype Detached) - it compares all categories to that
#Living at a mid terrace versus living in a Detached house, increases the log odds of being in fuel poverty 
#by 0.64. So the ODDS increase by (e^0.64)

#******Note: If I am just looking for a relationship between dependent and independent variables, I do not need to do the next steps.
#******These are mentioned here for exploratory purposes.

#lets calculate probabilities analytically, for a more meaningful interpretation.
#Probability can range from 0 to 1. Probability greater than 0.5 indicates success is more likely than failure. 
#Probability less than 0.5 indicates failure is more likely than success.
#Lets look at our 11th observation

#lets look at household 11: income:15795, fuel costs: 961.53, EPC:A/B/C, Dwelling type: detached
y=-5.412-0.0002*15795+0.0052*961.53

y
#What is the probability that House 11 is in fuel poverty?
#probability=ODDS(y)/(1+ODDS(y)), ODDS(y)=exp(y)
probability<-exp(y)/(1+exp(y))
probability# I get a 0.027 probability that the house is in fuel poverty

#lets see using another (quite faster) method to find probabilities:
fp$EPC<-as.factor(fp$EPC)
p<-predict(mylogit4, fp, type="response")
p # here I see that house 11 has a 0.027 probability, like in my calculations above


#no exact equivalent to the R2 of linear regression exists, 
#the McFadden R2 index can be used to assess the model fit
install.packages("blorr")
library(blorr)

blr_rsq_mcfadden(mylogit4)
#You could also try Maximum likelihood pseudo r-squared (Cox & Snell) and 
#Cragg and Uhler's or Nagelkerke's pseudo r-squared. 

#******IMPORTANT*******
# My analysis was conducted only to see how logistic regression works.
# It is not meaningful. Why? How were my data collected here?
# The fuel poverty index here, was modelled using the income and the total fuel cost for every household!
# So we already knew that there was a relationship in our case!
# Always be aware of how the data you are using are produced!
# You can find the detailed methodology for calculating in the LIHC index, used for this dataset, in the same folder
# where you have the csv file for this tutorial. Instructions on how to download the data are in our Moodle page


