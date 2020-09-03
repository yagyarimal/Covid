days185=0:185
caseupto=round(100*(1.24^days185),digit=0)
rbind(days185,caseupto)
# today 15,380,140
#upto179= 1.006345e+19
library(covid19.analytics)
library(dplyr)
library(prophet)
library(lubridate)
library(ggplot2)
tsc=covid19.data(case='ts-confirmed')# one idea for 2020-01-22to 2020-07-24 
#https://data.humdata.org/hxlproxy/api/data-preview.csv?url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv&filename=time_series_covid19_confirmed_global.csv
line=read.csv("C:/Users/Rimal/Desktop/covidmap/line.csv", header=T,stringsAsFactors = FALSE)

data=read.csv("C:/Users/Rimal/Desktop/R Programming is Best for Covid-19 Data Analysis and Prediction/time_series_covid19_confirmed_global(5).csv", header=T,stringsAsFactors = TRUE)
line=as.data.frame(line)
data
head(line)
l1=line %>% 
  mutate(Date=mdy(Date))
str(tsc)
data=as.data.frame(data)
str(data)
us=tsc%>%filter(Country.Region=='US') 
 us
us = data.frame(t(us))
str(us)
us=cbind(rownames(us),data.frame(us,row.names=NULL))
View(us)
colnames(us)=c('Date','Conformed')
usa=us[-c(1:4),]
usaa=data.frame(usa)
View(usaa)
usaa$Conformed=as.numeric(as.character(usaa$Conformed))
usaa$Date=ymd(usaa$Date)
str(usaa)
ggplot(usaa) +
  geom_point(aes(Date, Conformed)) +
  geom_point(data = usaa, aes(Date, Conformed), colour = 'red', size = 1)

ggplot(data=usaa,aes(x=Date,y=Conformed))+
  geom_line()+
    theme_bw()+ 
  ggtitle('Covid-19 in Conformed US')

qplot(Date,Conformed, data = usaa,main ='Covid-19 in Conformed US')

ds=usaa$Date
y=usaa$Conformed
df=data.frame(ds,y)
m=prophet(df)
future=make_future_dataframe(m,periods = 122)
View(future)
forecast=predict(m,future)
plot(m,forecast)
dyplot.prophet(m,forecast)
prophet_plot_components(m,forecast)
# data is  rality  vs conformed it wonly tires to approcmimate reality
pred=forecast$yhat[1:224]
pred
actual=m$history$y
actual
library(ggplot2)
plot(actual,pred)
abline(lm(pred~actual),col='red')#neither under or over estimination
summary(lm(pred~actual))
# R value high indicates more reliableity p is very low with very sighnificant

usaa=tsc%>%filter(Country.Region=='Brazil'|Country.Region=='Chile'|Country.Region=='India'|Country.Region=='Mexico'|Country.Region=='Peru'|Country.Region=='Russia'|Country.Region=='South Africa'|Country.Region=='US') 
usaa
usaa = data.frame(t(usaa))
str(usaa)
usaa=cbind(rownames(usaa),data.frame(usaa,row.names=NULL))
View(usaa)
colnames(usaa)=c('Date','Brazil','Chile','India','Mexico','Peru','Russia','South Africa','US')
usaa=usaa[-c(1:9),]
usaa=data.frame(usaa)

View(usaa)
usaa$Brazil=as.numeric(as.character(usaa$Brazil))
usaa$Chile=as.numeric(as.character(usaa$Chile))
usaa$India=as.numeric(as.character(usaa$India))
usaa$Mexico=as.numeric(as.character(usaa$Mexico))
usaa$Peru=as.numeric(as.character(usaa$Peru))
usaa$Russia=as.numeric(as.character(usaa$Russia))
usaa$South.Africa=as.numeric(as.character(usaa$South.Africa))
usaa$US=as.numeric(as.character(usaa$US))
usaa$Date=ymd(usaa$Date)
str(usaa)
View(usaa)
str(usaa)
#Daily Line Plot
library(lubridate)
library(tidyverse)
library(tidyverse)
library(dplyr)
library(lubridate)
library(magrittr)
library(gganimate)
library(tidyr)
library(ggplot2)
library(gifski)
library(lubridate)
data=read.csv("C:/Users/Rimal/Desktop/R Programming is Best for Covid-19 Data Analysis and Prediction/owid-covid-data2.csv", header=T,stringsAsFactors = FALSE,strip.white=TRUE)
library(lubridate)
library(tidyverse)
head(data)
str(data)
data=as.data.frame(data)
datanew=data %>% 
  mutate(date=mdy(date),format='%m%d%y')
head(data)
new2=data %>% mutate(date=lubridate::mdy(date),
                     year = lubridate::year(date), 
                     month = lubridate::month(date), 
                     day = lubridate::day(date))
new2$day=as.numeric(new2$day)
data=as.data.frame(data)
head(data$date)
datanew = data %>% 
  mutate(date=mdy(date))
head(datanew$date)
str(datanew)
datanew$date=as.Date(datanew$date,"%m/%d/%Y")
datanew$date
datanew$d=day(datanew$date)
datanew$d
datanew$d=as.numeric(datanew$d)
class(datanew$d)
datanew$m=month(datanew$date)
datanew$m
datanew$y=year(datanew$date)
str(datanew)
tail(datanew,40)
datanew$m=as.numeric(datanew$m)
View(datanew)
df=datanew%>% 
  filter(m==8) %>%  
  group_by(d,location,total_cases) %>% 
  summarise(count=n()) %>% 
  mutate(Cuml=cumsum(total_cases))
df=as.data.frame(df)
head(df)
new=data.frame(complete(df,d,location,
                        fill =list(count=0) ))
new$location
new2=new %>% filter(location =='United States'|
                      location =='Russia'|
                      location =='India'|
                      location =='Peru'|
                      location =='Chile'|
                      location =='Mexico'|
                      location =='Brazil')
head(new2)
ggplot(data=new2,aes(x=d,y=Cuml,
                     group=location,
                     color=location))+
  geom_line()+
  geom_point()+
  scale_y_log10()+
  theme_bw()+ 
  ggtitle('Animinated line plot')+
  transition_reveal(d)

 
