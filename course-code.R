2+7
17/3
2^3

sum<-2+5
sum

sum^2

library(tidyr)

#Read data and show data statistics
setwd("F:/ICPSRworkshop/Introduction to the R Statistical Computing Environment/ICPSR-R-Workshop")
getwd()

#read csv
mydata<-read.csv("oda_crs_oecd2012_2.csv")

names(mydata)
summary(mydata$AgencyCode)
dim(mydata)
head(mydata)
tail(mydata)

#check datatype
class(mydata$AgencyCode)
class(mydata$RecipientName)
table(mydata$RecipientName)

table(mydata$FlowCode)
summary(mydata$FlowCode)

install.packages("foreign")
library(foreign)

#read stata data
stata.data<-read.dta("nes2004subset2.dta")
names(stata.data)
summary(stata.data)
dim(stata.data)

install.packages("readstata13")
library(readstata13)
qog<-read.dta13("ch3_qog-1.dta")
names(qog)
View(qog)

#data rendering
qog<-read.csv("qog_subset_data.csv")
dim(qog)
# Display the column names of the dataset
colnames(qog)


#Subsetting by select certain variables
qog.subset.ciri<-subset(qog, select = c(ccode, cname, year,
                                     ciri_assn,ciri_dommov,ciri_formov,ciri_injud,
                                     ciri_physint,ciri_worker,ciri_speech))
summary(qog.subset.ciri)

#drop columns
qog.subset.ciri2<-subset(qog.subset.ciri,select=-ciri_injud)
summary(qog.subset.ciri2)

qog.ciri.2005<-subset(qog.subset.ciri2,year>2004)
summary(qog.ciri.2005)

#remove missing values
dim(qog.subset.ciri)
qog.ciri.all<-subset(qog.subset.ciri,is.na(ciri_physint)!=TRUE)
dim(qog.ciri.all)

#keep all data of Afghanistan
qog.ciri.afg<-subset(qog.subset.ciri,ccode==4)
head(qog.ciri.afg)
dim(qog.ciri.afg)

polity<-subset(qog, select=c(ccode, year, p_polity2))
head(polity)

#merge data
merged_data<-merge(x=qog.subset.ciri,y=polity,by.x=c("ccode","year"),by.y=c("ccode","year"))
head(merged_data)

#conditional re-coding
merged_data$democracy<-ifelse(merged_data$p_polity2>=6,1,0)
head(merged_data)
table(merged_data$democracy)


install.packages("car")
library(car)
merged_data$new_formov<-car::recode(merged_data$ciri_formov,"
                                    0='servely_restricted';
                                    1='somewhat_restricted';
                                    2='unrestricted'
                                    ")
table(merged_data$new_formov)
merged_data[merged_data==-66]<-NA
merged_data[merged_data==-77]<-NA
table(merged_data$new_formov)

#Tidyverse
