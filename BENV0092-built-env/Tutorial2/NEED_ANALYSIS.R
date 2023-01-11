##Tutorial 2: Working with the National Energy Efficiency Dataset (NEED)
need_tidy<-read.csv("S:/ENERGINST_ESDA/Energy Analytics in the Built Environment/ESDA 2020-21/Tutorial 2/NEED_tidy.csv")
 
summary(need_tidy)
#Observe the many different variables available
#In this dataframe, every row represents a different household, in a different year. We have yearly gas and electricity metered 
#data for each house from 2005 to 2012,info about the house and its location, 
#retrofit details (cavity wall insulations, loft insulations and boiler replacements), 
#and dates of when retrofits took place.

#Goal of the analysis in this Tutorial: Explore the NEED Dataset and discover whether retrofits were successful
# in reducing gas consumption of households that participated in the retrofits.

library(tidyverse)
library(psych)
View(need_tidy)
#I have 298828 rows, but how many households? Remember, there are entries for the same household for the years 2005-2012
length(unique(need_tidy$HH_ID)) # I have 40469 households!
#Observe columns CWI, BOILER and LI: These tell us whether a house had a retrofit or not.
##lets modify the column that will indicate whether a h/h received a cavity wall insulation
need_tidy$CWI<-ifelse((!is.na(need_tidy$CWI)),1,0)  
##lets modify the column that will indicate whether a h/h received a LOFT insulation
need_tidy$LI<-ifelse((!is.na(need_tidy$LI)),1,0)  
##lets modify the column that will indicate whether a h/h received a BOILER replacement
need_tidy$BOILER<-ifelse((!is.na(need_tidy$BOILER)),1,0) 

#this way I have created dummy variables: 1 for when a house received a retrofit and 0 for those that did not (control group - previously NA)

#######End of VIDEO PART 1

#######Start of VIDEO PART 2

#lets see when the improvements took place:
need_tidy$CWI_YEAR<-as.factor(need_tidy$CWI_YEAR)

ggplot(data=subset(need_tidy, !is.na(CWI_YEAR)), aes(x=CWI_YEAR)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  xlab("Year when Cavity wall insulation took place") +
  ylab("Frequency") 

#Most cavity walls (30%) were installed before 2005
#We highly doubt we can detect effect of cavity wall insulation (no data before 2004)
need_tidy$LI_YEAR<-as.factor(need_tidy$LI_YEAR)

ggplot(data=subset(need_tidy, !is.na(LI_YEAR)), aes(LI_YEAR)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  xlab("Year when Loft insulation took place") +
  ylab("Frequency") 

#Most lofts (25%) were insulated in 2012
need_tidy$BOILER_YEAR<-as.factor(need_tidy$BOILER_YEAR)

ggplot(data=subset(need_tidy, !is.na(BOILER_YEAR)), aes(BOILER_YEAR)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  xlab("Year when Boiler replacement took place") +
  ylab("Frequency") 

#Most boilers were replaced after 2008
#lets see if consumption of the boiler retrofit participants was different than the controls over the years
need_tidy$BOILER<-as.factor(need_tidy$BOILER)
need_tidy$Year<-as.factor(need_tidy$Year)#if I do not do this, I will get a completely different plot!!!!!
ggplot(aes(y = Gas_Cons, x = BOILER, fill = Year), data = need_tidy) + geom_boxplot()#some evident reduction for group 1

## We will explore the effect of boiler replacement on gas consumption:
## Did households that replaced their old boilers with new efficient ones consume less gas?

#we can compare gas consumption values of 2005 when most had not replaced their boilers to that of 2012
need_tidy0512 <-filter(need_tidy,Year==2005|Year==2012 )#this is a dataframe in a 'long' format
length(unique(need_tidy0512$HH_ID))

need_tidy0512<-select(need_tidy0512, HH_ID, REGION, Gas_Cons, Year, BOILER, BOILER_YEAR, FLOOR_AREA_BAND, PROP_TYPE)
#we only leave in what we need, to make things tidier

ggplot(aes(y = Gas_Cons, x = Year, fill = BOILER), data = need_tidy0512) + 
  geom_boxplot()+ ylab("Gas Consumption (KWh)")
#this is the difference in total consumption, from various different kinds of hhs

#1. Perform a statistical test that can show if change in consumption between 2005 and 2012 was significant
#Lets look at the households that had their boilers replaced: group 1 (participants). 
need_tidy0512_1<-filter(need_tidy0512, BOILER==1)#created a new dataframe that only contains group 1 (participants)
#I reshape back to the wide format (Otherwise the t-test might produce an error)
need_tidy0512_1<- spread(need_tidy0512_1,Year,Gas_Cons)
#now I have one row per household and 2 gas consumption columns, one for 2005, one for 2012
need_tidy0512_1<-na.omit(need_tidy0512_1)

#Lets see how their consumption changed from 2005 to 2012.

#to choose an appropriate test that compares before-after gas consumption of the participants, 
#we need to know if our consumption data are normally distributed.
#Because we are looking at the same sample of homes in 2 different points in time, we want 
#the distribution of differences in consumption 2005-2012 to be approximately normal
#lets check this:
#we need to create a new 'difference' variable. 

need_tidy0512_1<-mutate(need_tidy0512_1, Diff=need_tidy0512_1$'2012'-need_tidy0512_1$'2005')
qqnorm(need_tidy0512_1$Diff)
qqline (need_tidy0512_1$Diff)#how does this look? not too bad! 
#Q-Q plots take your sample data, sort it in ascending order, and then plot them 
#versus quantiles calculated from a theoretical distribution.
#Here I have asked for a normal distribution (qqnorm).
hist(need_tidy0512_1$Diff, main="Histogram of consumption data difference 2005-2012",xlab="Looks quite normal!")

#The assumption of normality is satisfied, We will choose to perform a parametric test. Specifically a Related/Paired Samples t-test, 
#to see whether the change in consumption between 2005 and 2012
#was significantly different for the group of participants.

result <- t.test(need_tidy0512_1$'2005', need_tidy0512_1$'2012', paired = TRUE)
result

#The p-value of the test is< 2.2e-16, which is less than the significance level alpha = 0.05. We can then 
#reject the null hypothesis and conclude that 
#the average consumption of the participants before new boiler installation is significantly different from 
#the average consumption after new boiler installation.
#the result also provides confidence intervals of the before-after difference: here, 6028.914-6337.389 kwh 
#lower consumption in the After period (2012)


#2.
#BUT, WAIT A MINUTE.. IS THIS ENOUGH? HAVE I CORRECTLY TESTED IF THE INTERVENTION WORKED?
#WHAT WAS MY MISTAKE?

########END OF VIDEO TUTORIAL PART 2

########START OF VIDEO TUTORIAL PART 3

#in our previous t-test we compared consumption of participants only, between 2 time periods to assess 
#whether the retrofit had an impact. But there are so many factors that could have led to the gas demand reduction that 
#we observed, factors other than the retrofit program, factors that we have not taken into account.
#We would get much more reliable results if we took the control group's gas cons into account, as a benchmark, to account for the 
#unobserved factors that we did not take into account in the previous analysis, such as the weather.

#to see if there is a significant difference in the change in gas cons between the two groups we need 
#to perform an Unpaired samples t test to the differences in mean consumption (2005-2012) for the 2 groups.
#But before we do that, we need to know that we are comparing similar households.

#I will again create a difference variable, but this time for both groups of households
#to create a difference column we need to change the data structure to wide format. Remember that we are using the dataframe
#need_tidy0512 here which contains data for both participants and control households.
need_tidy0512WIDE<- spread(need_tidy0512,Year,Gas_Cons)
#now I have two seperate columns for Gas consumption for 2005 and 2012.lets calculate the percentage change (since we are looking
#at 2 different groups now, better to compare them in terms of %difference in consumption)
need_tidy0512WIDE$cons.Diff<-(need_tidy0512WIDE$'2012'-need_tidy0512WIDE$'2005')*100/(need_tidy0512WIDE$'2005')

summary(need_tidy0512WIDE$cons.Diff)#we have many NA's, lets remove the rows:
need_tidy0512WIDE<-need_tidy0512WIDE[!is.na(need_tidy0512WIDE$cons.Diff), ]
#we see that there are values with a consumption decrease of -93% or 1185%! These can be either:
#data input errors or the house changed occupants between 2005 and 2012
#I will exclude everything that showed a difference more than +-50%
need_tidy0512WIDE<-filter(need_tidy0512WIDE,cons.Diff<50&cons.Diff>-50)

#now that we created our Difference column, we will group the households based on common dwelling characteristics.
#lets look at houses by geographical area and type/size
#lets keep only households from London and South East as they experience similar weather conditions. This way we will
#take into account the effect of weather, more or less.

#lets keep only Detached households and of size 101-150 sqm. This way we will take into account the effect that 
#dwelling type and dwelling size might have on gas consumption, more or less.

need_tidy0512WIDE<- filter(need_tidy0512WIDE, (REGION == 'London'|REGION == 'South East')& PROP_TYPE=='Detached'&FLOOR_AREA_BAND=='101-150')

summary(need_tidy0512WIDE)
str(need_tidy0512WIDE$BOILER)#we want this to be a factor. If it is not then:
#need_tidy0512WIDE$BOILER<-as.factor(need_tidy0512WIDE$BOILER)

#lets visualise the consumption difference between the 2 groups

av_values <- aggregate(cons.Diff ~ BOILER, need_tidy0512WIDE, FUN = "mean")
p<-ggplot(need_tidy0512WIDE, aes(x=BOILER,y=cons.Diff,fill=BOILER)) + 
  geom_boxplot()+ 
  stat_summary(fun='mean', geom="point", shape=20, size=10, color="red", fill="red") +
  geom_text(data = av_values, aes(label = cons.Diff))+ #here I added text in the y coordinate (cons.)
  ylab("Difference in Gas Consumption from 2005 to 2012 (%)")+xlab("group 0/1")
p
#we see that there was a larger reduction for group 1 (the intervention group) - there is a larger negative difference (-21.26%)
#but we need to perform a statistical test to decide if there was a significant difference between the 2 or not
#lets see the distribution of our data. is it normal?

#comparing two independent samples: http://www.sthda.com/english/wiki/unpaired-two-samples-t-test-in-r
#we can use an Unpaired samples t test to check if there was a significant difference in the gas consumption 
#change between the 2 groups.
#T Tests have certain assumptions (see link above). If these assumptions are not satisfied, SOME WOULD SAY we CANNOT use them.
#one of them is the assumption of Normality
#Since we will be comparing consumption DIFFERENCE of the 2 groups, we need to know if this Difference has a normal distribution or not
#we need to check for normality in BOTH groups (0 and 1)
#lets see:
qqnorm(need_tidy0512WIDE$cons.Diff[need_tidy0512WIDE$BOILER == 0])
qqline (need_tidy0512WIDE$cons.Diff[need_tidy0512WIDE$BOILER == 0])#how does this look? not too bad

hist(need_tidy0512WIDE$cons.Diff[need_tidy0512WIDE$BOILER == 0], main="Histogram of control group consumption difference",xlab="skewed data for the control group")
#this does not look good! it is quite skewed.

#will also perform the Shapiro Wilk test to test for normality
shapiro.test(need_tidy0512WIDE$cons.Diff[need_tidy0512WIDE$BOILER == 0])
#The Shapiro test tells me the data are not normally distributed (I had a significant test).

qqnorm(need_tidy0512WIDE$cons.Diff[need_tidy0512WIDE$BOILER == 1])
qqline (need_tidy0512WIDE$cons.Diff[need_tidy0512WIDE$BOILER == 1])#how does this look? not great

#Lets do the same for our participants
hist(need_tidy0512WIDE$cons.Diff[need_tidy0512WIDE$BOILER == 1], main="Histogram of control group consumption difference",xlab="skewed data for the control group")
#this does not look good!

shapiro.test(need_tidy0512WIDE$cons.Diff[need_tidy0512WIDE$BOILER == 1])#not normal

#****IMPORTANT*** 
#Some would say that since we do not have normality, we cannot use a T-test to compare gas consumption 
#of treatment group and the control group
#And that we must use a Non-parametric test. Here we are comparing 2 independent groups of households. So:
#They would suggest using an Independent group Wilcoxon rank sum Test for the difference in consumption for the 2 groups 
#However, although my data are skewed, I still have a large sample of data FOR EACH GROUP (n>30) 
#so I will choose to use an Independent Samples T-Test, which is a Parametric test and much more robust than
#any non-Parametric test.

#The null hypothesis is that the gas consumption DIFFERENCES of group 1 and group 2 are identical 
#My sample needs to satisfy a further assumption here: The 2 groups should have approximately equal variances, for us to use a t-test:
#We'll use F-test to test for homogeneity in variances. This can be performed with the function var.test():
res.ftest <- var.test(cons.Diff ~ BOILER, data = need_tidy0512WIDE)
res.ftest #P value greater than the significance level alpha = 0.05. In conclusion, there is no significant difference between the variances of the two sets of data. 
#Therefore, we can use the classic t-test:

res <- t.test(cons.Diff ~ BOILER, data = need_tidy0512WIDE, var.equal = TRUE)
res

#As the p-value turns out to be 0.017, and is less than the 0.05 significance level, we reject the null hypothesis.
#We can conclude that the control group's consumption difference is significantly different than the treatment's consumption difference. 
#This may be due to the effect of boiler replacement.
#From the sample of similar households that we chose (same area, same dwelling type, same sqm area), 
#it seems that those that had a new condensing boiler installed, 
#showed around 4% reduction in their gas consumption, compared to the control sample.
#Given that we compared 2 quite similar groups of households (same household type and size), 
#located in the same area so they experience the same weather conditions,
#we can say that there is a good chance that Boiler Replacement worked and significantly decreased the consumption of
#our intervention group (participants).

#Controlling for more socio demographic factors, would give a clearer picture: e.g. controlling for Property Age as well!
#But.. would we still be sure? No - for a number of reasons!
#1. Some households received additional measures apart from boiler replacement 
#2. We don't take into account what happened to consumption between 2005 and 2012 - exploring changes during these years could
#reveal more insights (e.g. such as changes of tenants) and would make the analysis more robust, as many households
#got a boiler replacement during these years

