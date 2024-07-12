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

#install.packages("foreign")
library(foreign)

#read stata data
stata.data<-read.dta("nes2004subset2.dta")
names(stata.data)
summary(stata.data)
dim(stata.data)

#install.packages("readstata13")
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
#library(car)
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
library(tidyr)
#install.packages("tidyverse")
library(tidyverse)

dim(qog)

qog_ciri<-qog %>% select(ccode, cname, year,
                         ciri_assn,ciri_dommov,ciri_formov,ciri_injud,
                         ciri_physint,ciri_worker,ciri_speech)
head(qog_ciri)
qog2005<-qog %>% filter(year>2004)
summary(qog2005$year)

#merging in tidyverse
merged_data<-qog_ciri %>% left_join(polity,by=c("ccode"="ccode","year"="year"))
summary(merged_data)

#replace dummy variables
merged_data<-merged_data %>% mutate(democracy=ifelse(p_polity2>6,1,0))
table(merged_data$democracy)

merged_data2<-merged_data %>% mutate(ciri_worker2=recode(ciri_worker,
                                                        "0" = "severly_restricted",
                                                        "1" = "somewhat_restricted",
                                                        "2" = "no_restricted",
                                                        "-66"=NULL,
                                                        "-77"=NULL
                                                        ))
table(merged_data2$ciri_worker2)
merged_data2<-merged_data %>% mutate(new_ciri_assn=ciri_assn*100)

#arrange data
merged_data_arrange<-merged_data %>% arrange(cname,year)
head(merged_data_arrange)

#descriptive stats
nes20<-read.csv("nes2020_subset.csv")
head(nes20)
summary(nes20$ft_science)

mean(nes20$ft_congress,na.rm = TRUE)
sd(nes20$ft_congress,na.rm = TRUE)
var(nes20$ft_congress,na.rm = TRUE)
IQR(nes20$ft_congress,na.rm = TRUE)

#install.packages("moments")
library(moments)

table(nes20$vote)
prop.table(table(nes20$vote))

library(tidyverse)

#the summarize for mean
nes20 %>% summarise(mean(ft_congress,na.rm=TRUE))

nes20 %>% summarise(median(ft_congress,na.rm=TRUE))

#specific summarize on target variable with stats
nes20 %>% summarise_at(vars(ft_congress,ft_science),
                       funs(mean(.,na.rm=TRUE),
                            sd(.,na.rm=TRUE),
                            var(.,na.rm=TRUE),
                            length))

#groupby summarize
nes20 %>% group_by(vaccines) %>% summarise_at(vars(ft_congress,ft_science),
                                              funs(mean(.,na.rm=TRUE),
                                                   sd(.,na.rm=TRUE),
                                                   var(.,na.rm=TRUE),
                                                   length))

#data visualization
#density plot
plot(density(nes20$ft_congress,na.rm=TRUE),
     main='density of a continuous variable',
     xlim=c(0,100))

#histogram
hist(nes20$ft_congress,
     main='history of ft_congress',
     xlab='feeling thermometer score')

hist(nes20$ft_science,
     main='history of ft_science',
     xlab='feeling thermometer score',
     col='lightblue',
     breaks=seq(0,100,by=10),
     xaxt='n',
     axis(side=1,at=c(5,15,35,55,65,75,85,95)))

#barplot
barplot(table(nes20$freetrade),
        main='barplot of freetrade',
        xlab='Frequency',
        col='aquamarine4',
        xlim=c(0,5000),
        horiz=TRUE,las=1)

barplot(table(nes20$party),
        main="barplot of party",
        ylab='frequency',
        names.arg=c('democrat','republician','indepedent','other'),
        cex.names = 0.85)

#scatterplot
plot(x=nes20$ft_science,y=nes20$ft_harris,pch=16,col='skyblue3')

#boxplot
boxplot(ft_congress~vote,data=nes20,
        xlab='',
        names=c('Non-voter','voter'))

#ggplot2
ggplot(data=nes20,aes(x=ft_science,y=ft_biden))

#install.packages('poliscidata')
library(poliscidata)
library(ggplot2)

worlddata<-poliscidata::world
head(worlddata)

#ggplot histogram
ggplot(data=worlddata,aes(x=women13))+
  geom_histogram(bins=21,color='black',fill='grey')+
  theme_bw()+
  ggtitle('women in legislature')+
  xlab('woman in legislature(2013)')

library(dplyr)
#ggplot barchart
ggplot(data=worlddata %>% filter(!is.na(regime_type3)),
       aes(x=regime_type3,fill=hi_gdp))+
  theme_minimal()+
  geom_bar()

#boxplot
ggplot(data=worlddata %>% filter(!is.na(regime_type3)),
       aes(x=regime_type3,y=gdppcap08))+
  geom_boxplot(color='blue')+
  coord_flip()

#geompoint
ggplot(data=worlddata %>% filter(!is.na(regime_type3)),
       aes(x=gdppcap08,y=hdi,color=regime_type3))+
  geom_point(size=3)+
  theme(legend.title=element_blank())+
  theme_bw()+
  labs(color='regime_type')+
  geom_text(aes(label=country),size=1.2,color='black')

#geosmooth
ggplot(data=worlddata %>% filter(!is.na(regime_type3)),
       aes(x=gdppcap08,y=hdi,color=regime_type3))+
  geom_smooth()+
  geom_point(color='black')
