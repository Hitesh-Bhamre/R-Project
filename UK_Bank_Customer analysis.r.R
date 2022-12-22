## Import Data ##

getwd()
setwd("D:\\R Project\\UK_Bank_Customer")
my_bank = read.csv("UK_Bank_Customer.csv")

bank = as.data.frame(my_bank)
view(bank)

#packages 
library(psych)
library(tidyr)
library(lubridate)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotrix)
library(plotly)

## EDA ##

dim(bank)
str(bank)
describe(bank)
head(bank)

## Dataset Cleaning ##

#Change column names

bank <- rename(bank, Customer_ID = Customer.ID,
               Job_Classification = Job.Classification, 
               Date = Date.Joined,
               Deposit = Balance,
               First_Name = Name,
               Last_Name = Surname)
view(bank)


## Change datatype of Attribytes

bank$Deposit = as.integer(bank$Deposit)
class(bank$Deposit)

typeof(bank$Age)
bank$Age = as.integer(bank$Age)


#Checking NA Values

is.na(bank$Age)

sum(is.na(bank$Age))
boxplot(bank$Age)
hist(bank$Age)

boxplot(bank$Age)

describe(bank$Age)

#replace NA with Age mean base on gender

bank$Age[is.na(bank$Age)]=
  mean((bank$Age[bank$Gender == "Male"]),na.rm=TRUE)
bank$Age[is.na(bank$Age)]=
  mean((bank$Age[bank$Gender == "Female"]),na.rm=TRUE)

sum(is.na(bank$Age))

bank$Age <- format(round(bank$Age, 0))

## Change class of Date column

typeof(bank$Date)
class(bank$Date)

bank$Date <- strptime(bank$Date, "%B %d, %Y")

view(bank)

#bank <- mutate(bank, Month = as.numeric(format(bank$Date, format = "%m")))


## Covert numeric month to string 

bank$Month <- lubridate::month(bank$Date,
                                label = TRUE,
                                abbr = TRUE)
view(bank)


## Visualization Analysis

## Total Deposit by Month

month_group <- aggregate(Deposit ~ Month, bank, sum)
month_group

bar_chart_month <- ggplot(month_group,
                          aes(Month, Deposit,
                           fill = Month))+
                   geom_bar(stat="identity")+
                   geom_text(aes(label = Deposit),
                   position=position_dodge(width=0.5),
                   vjust=-0.50,
                   size = 3)+
                   theme_grey()+
                   ggtitle("Deposits by Month")
bar_chart_month

## Deposit by Gender

gender_group <- aggregate(Deposit ~ Gender+Month, bank, sum)
gender_group
  
line_chart_month <- ggplot(gender_group, aes(x = Month, 
                                             y = Deposit, 
                                             group = Gender)) + 
                    geom_line(aes(color = Gender, 
                                  linetype = Gender),
                                  size = 1) + 
                    scale_color_manual(values = c("darkred", 
                                                  "darkblue"))+
                    ggtitle("Deposits by Month")+
                    geom_point(size = 2, color = "darkgreen")
  
line_chart_month  

##Deposits by Age

hist_age <- ggplot(data = bank,
                   aes(x = Age, color = Gender))+
              geom_histogram(stat = "count", 
                             bins = 10,
                             fill = "black")+
              ggtitle("Age Distribution")+
              labs(x = "Age", y = "Frequency")
hist_age


## Total Deposits by Region

Region_group <- aggregate(Deposit ~ Region, bank, sum)
Region_group

bar_region <- ggplot(Region_group,
                     aes(Region, Deposit,
                     fill = Region))+
              geom_bar(stat="identity")+
              geom_text(aes(label = Deposit),
                             position=position_dodge(width=0.5),
                            vjust=-0.50)+
              ggtitle("Region by Deposits")+
              ylim(0,600000)
bar_region

## Job_classification Pie Chart
table(bank$Job_Classification)
job <- as.data.frame(table(bank$Job_Classification))
job
job<- rename(job, Job_Category = Var1)
job
pie_chart_job <- plot_ly(data = job, title="Job Classification",
                         type='pie', labels= ~Job_Category, 
                         values= ~Freq,
                         textinfo='Freq',
                         insidetextorientation='radial')
    
pie_chart_job
  
## Deposits by Job_Classification
jc_group <- as.data.frame(aggregate(Deposit ~ Job_Classification, 
                                    bank, sum))
jc_group
bar_job <- ggplot(jc_group,
                  aes(x=Job_Classification, 
                      y=Deposit,
                      fill = Deposit))+
           geom_bar(stat="identity")+
           geom_text(aes(label = Deposit),
                      position=position_dodge(width=0.5),
                      vjust=-0.50)+
           ggtitle("Job Category by Deposits")+
           ylim(0,550000)
bar_job


## End Project ## 


bank %>% 
  arrange(desc(Deposit)) %>% 
  group_by(Name) %>% slice(1:2)
