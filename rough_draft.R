options(scipen=999)  # turn off scientific notation like 1e+06
library(ggplot2) #run ggplot2 
##Import census data (update path), can be obtained on https://factfinder.census.gov/faces/nav/jsf/pages/searchresults.xhtml?refresh=t
#Select geography as county, and all counties within Maryland, and for each variable, choose people->employment->employment(labor force) status, 

county_income_md<-read.csv("desktop/johns hopkins class of 2020/sophomore year/public health secondary data/final project/individual income/ACS_16_5YR_S1901.csv")
county_education_md<-read.csv("desktop/johns hopkins class of 2020/sophomore year/public health secondary data/final project/educational attainment /ACS_16_5YR_S1501.csv")
county_employment_md<-read.csv("desktop/johns hopkins class of 2020/sophomore year/public health secondary data/final project/employment /employment.csv")
county_insurance_md<-read.csv("desktop/johns hopkins class of 2020/sophomore year/public health secondary data/final project/health insurance /ACS_16_5YR_B27011.csv")

#Keep only variables you need, rename columns
colnames(county_education_md)[colnames(county_education_md)=="HC02_EST_VC04"] <- "County_Education"
county_education_md<-county_education_md[c(1,3,30)]

colnames(county_income_md)[colnames(county_income_md)=="HC01_EST_VC13"] <- "Median_income"
county_income_md<-county_income_md[c(1,3,92)]

colnames(county_employment_md)[colnames(county_employment_md)=="HC02_EST_VC01"] <- "Labor_force_participation"
county_employment_md<-county_employment_md[c(1,3,6)]

colnames(county_insurance_md)[colnames(county_insurance_md)=="HD01_VD05"] <- "Employed_insured"
colnames(county_insurance_md)[colnames(county_insurance_md)=="HD01_VD01"] <- "Total estimate"
county_insurance_md<-county_insurance_md[c(1,3,4,12)]

#Merge datasets by county keeping all observations, geo id match for both data sets, same names 
county_merged<-Reduce(function(x, y) merge(x, y, all=TRUE), list(county_education_md, county_employment_md, county_income_md, county_insurance_md))

#Examine the range of median household incomes
range(county_merged$Median_income)

#Break median income into quartiles and find interquartile range 
data<-c(county_merged$Median_income)
IQR(data)
Q3 <- quantile(data, 0.75, type = 2)
Q1 <- quantile(data, 0.25, type = 2)
Q2 <- quantile(data, 0.50, type = 2)
Q4 <- quantile(data, 1.00, type = 2)
Q5 <- quantile(data, 0.00, type = 2)

#Using median household income, create breaks every according to quartiles 
county_merged$Median_income_cat1<-cut(county_merged$Median_income, c(0,35886,52169,68464,86872,113800))

#Create new variable, percent with insurance provided by employer within a county 
county_insurance_md$percent_employer_insurance<-(100*county_insurance_md$Employed_insured/county_insurance_md$`Total estimate`)
x<-c(county_insurance_md$percent_employer_insurance)
county_insurance_md$percent_employer_insurance<-round(county_insurance_md$percent_employer_insurance, digits = 1)

#Create histogram of median income among all counties, adjust bin width 
ggplot(county_merged, aes(x=Median_income))+ geom_histogram(bins=10)
basehist<-ggplot(county_merged, aes(x=Median_income)) + geom_histogram(binwidth = 100000)

ggplot(county_merged, aes(y = Labor_force_participation, x = Median_income)) + geom_point(aes(color="red"))+ geom_smooth(method="lm")+labs(title="Labor Force Participation Vs Median Income", subtitle="From Maryland County dataset", y="labor force participation (%)", x="Median Income")
colorplot<-ggplot(county_merged, aes(y = Labor_force_participation, x = Median_income)) + geom_point(aes(color=County))+ geom_smooth(method="lm", color="red") + labs(title="Labor Force Participation Vs Median Income", subtitle="From Maryland County dataset", y="labor force participation (%)", x="Median Income")

boxplot(county_merged$Median_income)

#Perform Chi Square Test of Significance Between Variables 
chisq.test(county_merged[5:8])

t.test(county_merged$percent_employer_insurance[county_merged$Median_income_cat1=="(3.59e+04,5.22e+04]"], county_merged$percent_employer_insurance[county_merged$Median_income_cat1=="(8.69e+04,1.14e+05]"])



