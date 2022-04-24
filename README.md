# Overview
Analyzing Credit Card Spend of 5000 customer data

# Aim of Project
1. The objective of this case study is to understand what's driving the total spend of credit card(Primary Card + Secondary card).
2. Priotize the drivers based on the importance.

# Life Cycle of Project

1.Data Analysis

Exploratory data analysis through understanding of data. 
Looking at the distribution of dependent variable and also segregating categorical with numerical variable. 
Finding missing value and outliers through descriptive analysis. Variables with maximum missing values are detected 

2.Feature Engineering

Omitting the variables which has highest no. of missing values. 
Imputing missing value with mean value treatment and also outliers is capped with 5 and 95 percentile. 
Removing features with high multicollinearity through correlation matrix.

3.Feature Selection

Selecting the categorical features with 95% confidence level i.e. p<.05 for variable reduction.
Significant categorical variables are selected through Anova test and stepwise regression. 
Further VIF detects multicollinearity in selected variables. 

4.Model Building

Building the final model through splitting the dataset into development and validation datasets.
Predicting the total spend in both the datasets and creating top 10 decile to test model’s ability to predict the intended outcome.

# Output

<img width="600" alt="Lin Charts" src="https://user-images.githubusercontent.com/104310260/164983304-74754d80-30d3-4c7c-b4c2-1062685db55e.png">

Driving factors that influence the card spent amount are

Card
Card2
Incinc
Gender
OtherDebt



