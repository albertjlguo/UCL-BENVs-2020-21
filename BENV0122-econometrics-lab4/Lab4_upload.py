# -*- coding: utf-8 -*-
"""
Created on Tue Apr 13 12:49:10 2021

@author: kmayr
"""
### Welcome to lab 4: Data analysis with Python ###
### For this lab we will be using the data from tutorial 8, which contains key financial information for USA ###

### While the basic version of Python has a lot of great functions, oftentimes you will find yourself needing more sophisticated tools ###
### There exist many packages that can help you with data analysis and today we will be looking at and use some of the basics ###

### Pandas is your go-to for anything related to data; it is a great package for structuring data as a matrix and helps with basic data manipulation
### numpy is a library that contains many very useful functions for matrix and vectorised data ###
import pandas as pd 

### Matplotlib is Python's attempt at ggplot2 and allows you to produce graphs ###
### seaborn is a similar library made specifically for visualising statistical data ###
import matplotlib.pyplot as plt

### statsmodels is a library for some more "traditional" statistical analysis ###
### There are many more like scipy (mathematics, science, engineering) and scikit-learn (machine learning) ###
import statsmodels.api as sm
from statsmodels.tsa.statespace.tools import diff
from sklearn import linear_model

### Some packages have rather long names, which is cumbersome when you want to call functions within the package, so you can give them an alias via the "as" statement ### 


### Set up your working directory and data ###
directory = ### PUT YOUR DATA HERE ###
### When inputting a path, you'll have to use \\ instead of \ as a single \ is treated as an escape character in Python###
data = 'lesson8_data.xls'

### let's load the data into a data frame using pandas ###

df = pd.read_excel(directory + data)

### pandas can handle most data types available, such as csvs (any delineation), xlsx (excel), dta (stata), and so on ###
### now let's have a look at the data inside the data frame ###
### the .head() method returns the first five entries in your data frame ###

df_head = df.head() 

### for small data such as this isn't super useful, but once the data gets large it might be helpful to inspect a small subset first ###

### if you want to get the columns of the data frame, you can call on the columns property ###

df.columns

### the .describe() method gives you the basic summary statistics for numeric columns
df.describe()

### since we have quite a few columns it is a bit hard to read ###
### let's just look at individual columns first ###
### you can subset the data to a single column first ###

df['FFR'].describe()

### if you are only looking at a single column you can just put it in brackets ###
### if you want more than one column, you need to input them as a list ###

df[['FFR', 'Tbill']].describe()


### naturally you can subset using the columns command we used earlier ###
df[df.columns[:4]].describe()

### if you want to get a specific cell in the matrix you can use the .loc[] or .iloc[] methods ###

df.loc[1,'FFR'] 

### you can also retrieve the entirety of multiple columns if you want and save them as a new df###
df_new = df.loc[:,['FFR', 'Tbill']] 

### next, let's try to do some more complicated subsetting of the data ###
### for example, what if we want the rows that are either the minimum or the maximum value for Unemp column? ###

max_unemp = df['Unemp'].max()
min_unemp = df['Unemp'].min()
mean_unemp = df['Unemp'].mean()
median_unemp = df['Unemp'].median()
sd_unemp = df['Unemp'].std()
var_unemp = df['Unemp'].var()
var_unemp2 = sd_unemp**2

df_max = df[df['Unemp'] == max_unemp]
df_min = df[df['Unemp'] == min_unemp]

df_min_max = df[(df['Unemp'] == max_unemp) | (df['Unemp'] == min_unemp)]

### You can create quite complex conditions to subset your data ###
### A lot of the methods discussed during the first lab, such as for and while loops, also work on DataFrames! ###
### However, pandas offers some inherent tools that make this much easier ###

for index, row in df.iterrows():
    print(df.loc[index,'FFR'] + 2)

### The iterrows function allows you to iterate over the rows of the DataFrame ###
### However, iterating over rows is usually not very efficient and the .apply() function can achieve the same much faster ###

df['FFR'].apply(lambda x: x+2)

### You can use the the apply function to make easy booleans for subsetting ###

df['unemployment_max'] = df['Unemp'].apply(lambda x: x == max_unemp)

### Now that we have discussed the fundamentals of working with data, let's try visualise the data ###

fig = plt.figure(0,  figsize = [15,10]) ### Figuratively, plt.fig starts a "canvas" for you to plot on
ax = fig.add_subplot() ### The actually plotting happens on a subplot of the figure
ax.set_xlabel('Date')
ax.set_ylabel('Unemployment Rate')
ax.set_title('Unemployment over time')
ax.plot(df['DATE'], df['Unemp']) ### Here we select the data for our x and y lables
ax.xaxis.set_tick_params(rotation=45)
new_x_ticks = [x for x in list(df['DATE']) if "Q1" in x] ### If we use all the date lables for the x ticks, it would be too packed! 
ax.set(xticks = new_x_ticks)
ax.grid(True)
fig.show()

### Next, let's try to run a plain OLS regression on the data ###
### Since the purpose of this lab is to familiarise you with Python, I will not be covering how to analyse the data proper ###
### Instead, I will be showing you how to implement some methods that you have seen in class ###
### For a proper analysis using this data have a look at tutorial 8 ###
### First, let's see if we can see some correlation between Unemployment and the Tbill ###

lim_val = max(df['Tbill'].max(),df['Unemp'].max())+1
fig1 = plt.figure(1,  figsize = [15,15]) 
ax1 = fig1.add_subplot()
ax1.set_xlabel('Tbill')
ax1.set_ylabel('Unemployment')
ax1.set_xlim([-1,lim_val])
ax1.set_ylim([-1,lim_val])
ax1.set_title('Tbill - Unemployment Scatter')
ax1.scatter(df['Tbill'], df['Unemp']) 
ax1.grid(True)

### It looks like higher values of Tbill are not really associated with higher values of Unemployment! We get some clusters of low Tbill and high unemployment as well (probably monetary easing) ###
### Mean unemployment seemsto be around 6 ###
### Let's see what OLS will return to us ###

Y = df['Unemp']
X = df['Tbill']
X = sm.add_constant(X)
reg = sm.regression.linear_model.OLS(Y, X, hasconst = True)
results = reg.fit()
results.summary()


### As you can see form the summary results, the fit of the model is pretty egregious ###
### You can do the same regression using scikit learn ###

model = linear_model.LinearRegression()
reg_sk = model.fit(X, Y)
reg_sk.coef_
reg_sk.intercept_

### Let's try to demean and first difference the data###
### There are many ways to do this, but two ways are ###
df['Unemp_mean_sub'] = df['Unemp'].apply(lambda x: x - df['Unemp'].mean())
df['Tbill_mean_sub'] = df['Tbill'] - df['Tbill'].mean()


### It would be possible to first difference by iterating over the rows of the DataFrame, but luckily the statsmodel has a fucntion that does that for us ###
df['Unemp_fd'] = diff(df['Unemp'], k_diff = 1)
df['Tbill_fd'] = diff(df['Tbill'], k_diff = 1)


fig2 = plt.figure(2,  figsize = [15,10])
ax2 = fig2.add_subplot(211) ### The three-digit number here indicates the number of rows, the number of columns, and the index number of the subplot ###
ax2.set_ylabel('Unemployment (De-Meaned)')
ax2.set_title('Unemployment (De-Meaned) over time')
ax2.plot(df['DATE'], df['Unemp_mean_sub'])
ax2.xaxis.set_tick_params(rotation=45)
ax2.set(xticks = new_x_ticks)
ax2.grid(True)
ax2.tick_params(labelbottom=False)

ax3 = fig2.add_subplot(212)
ax3.set_xlabel('Date')
ax3.set_ylabel('Unemployment (First Differenced)')
ax3.set_title('Unemployment (First Differenced) over time')
ax3.plot(df['DATE'], df['Unemp_fd'], c = 'r')
ax3.xaxis.set_tick_params(rotation=45)
ax3.set(xticks = new_x_ticks)
ax3.grid(True)


### The de-meaned values don't look very stationary, but the fd values look more promising! Let's do a simple regression on each ###
Y2 = df['Unemp_mean_sub']
X2 = df['Tbill_mean_sub']
X2 = sm.add_constant(X2)
reg2 = sm.regression.linear_model.OLS(Y2, X2, hasconst = True)
results2 = reg2.fit()
results2.summary()

Y3 = df['Unemp_fd']
X3 = df['Tbill_fd']
X3 = sm.add_constant(X3)
reg3 = sm.regression.linear_model.OLS(Y3, X3, missing = 'drop', hasconst = True)
results3 = reg3.fit()
results3.summary()

fig3 = plt.figure(3,  figsize = [15,10])
ax3 = fig3.add_subplot()
ax3.set_xlabel('Date')
ax3.grid(True)
ax3.set_xlabel('Date')
ax3.set_ylabel('Residuals')
ax3.set_title('Regression 3 residuals over time')
ax3.plot(df['DATE'][1:], results3.resid)
ax3.xaxis.set_tick_params(rotation=45)
ax3.set(xticks = new_x_ticks)

### This looks much better and we might have a chance to use this data! ###
### The next step would be to use the Augmented Dickey Fuller test to check for stationarity, which you can find in the code for tutorial 8 ###
### This concludes lab 4 and I wish you best of luck on the exams! ###