#start at 100
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(gganimate)
theme_set(theme_bw())
library(gifski)
library(av)
library(gapminder)
head(gapminder)
library(utils)
data=read.csv("C:/Users/Rimal/Desktop/Dynamic Animated Plots for Better COVID Cases Analysis/Copy of COVID-19-geographic-disbtribution-worldwide-2020-08-31.csv", header=T,stringsAsFactors = FALSE)
data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = " ", fileEncoding = "UTF-8-BOM")

setwd("C:/Users/Rimal/Desktop/covidmap")
write.csv(gapminder,'gapminder.csv')

gapminder=read.csv("C:/Users/Rimal/Desktop/covidmap/gapminder.csv", header=T,stringsAsFactors = FALSE)
covid=read.csv("C:/Users/Rimal/Desktop/covidmap/COVID-19 Cases (1).csv", header=T,stringsAsFactors = FALSE)
gapminder$country=tolower(gapminder$country)
covid$Country_Region=tolower(covid$Country_Region)
data1=merge(gapminder,covid,
            by.x='country',
            by.y='Country_Region')
head(data1)
str(data1)
data1=data.frame(data1)
keeps <- c("country","continent","lifeExp","pop","ï..Case_Type","Date","Admin2","Cases")
data2 = data1[keeps]
head(data2)
str(data2)
us=data2 %>% 
  filter(ï..Case_Type =='Confirmed') 
cc=us %>% 
  group_by(country,continent,lifeExp,pop,ï..Case_Type,Date,Admin2) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count))
head(cc)
attatch(gapminder)
head(gapminder)

p=ggplot(data=gapminder,aes(gdpPercap,lifeExp,size=pop,color=country))+
  geom_point(alpha=0.7)+
 scale_colour_manual(values = country_colors)+
  scale_size(range=c(2,15))+
scale_x_log10()+
  facet_wrap(~continent)+
  theme(legend.position = 'none')+
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=22,face="bold"),
        strip.text = element_text(size=20))
p
p2=p+labs(title = 'year:{frame_time}',x='GDP percap0',y='life exceptancy')+
  transition_time(year)+
  shadow_wake(0.5)
p2
#Bubbble plot     
p2+transition_time(year)+
  labs(title='GDP VS Life',subtitle = 'Year:{frame_time}')+
  shadow_wake(0.5)+
  facet_wrap(~continent)


library(tidyverse)
library(dplyr)
library(lubridate)
library(magrittr)
library(gganimate)
library(tidyr)
library(ggplot2)
library(gifski)
library(lubridate)
#Animinated visualization
setwd("C:/Users/Rimal/Desktop/covidmap")
data=read.csv("C:/Users/Rimal/Desktop/covidmap/covid.csv", header=T,stringsAsFactors = FALSE)
data=read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = " ", fileEncoding = "UTF-8-BOM")
str(data)
data=as.data.frame(data)
head(data$date_confirmation)
datanew=data %>% 
  mutate(date_confirmation=dmy(date_confirmation))
head(datanew$date_confirmation)
yy=datanew %>% 
  group_by(date_confirmation) %>% 
  summarise(count=n()) %>% 
  mutate(Cuml=cumsum(count))
head(yy)
yy %>% 
  ggplot(aes(x=date_confirmation,y=Cuml))+
  geom_line(color='red')+
  geom_point(size=1.5)+ #may break
  geom_area(fill='red')+
  theme_bw()+
  ggtitle('Daily Cummulative Cases')+
  transition_reveal(Cuml)
  anim_save('C:/Users/Rimal/Desktop/covidmap/Cumlplot22') 
  
  
 
  #start here C:\Users\Rimal\Desktop\Dynamic Animated Plots for Better COVID Cases Analysis
  
   #https://data.world/liz-friedman/covid-19-testing-data-from-our-world-in-data
  data=read.csv("C:/Users/Rimal/Desktop/Dynamic Animated Plots for Better COVID Cases Analysis/owid-covid-data2.csv", header=T,stringsAsFactors = FALSE)
  library(lubridate)
  library(tidyverse)
  head(data)
  str(data)
  data=as.data.frame(data)
  datanew=data %>% 
    mutate(date=ymd(date))
  head(data)
  new2=data %>% mutate(date=lubridate::ymd(date),
         year = lubridate::year(date), 
         month = lubridate::month(date), 
         day = lubridate::day(date))
  head(new2)
  new2=new2 %>% 
  group_by(date) %>% 
    summarise(count=n()) %>% 
   mutate(Cuml=cumsum(count))
  
  str(new2)
  tail(new2)
  new2=as.data.frame(new2)
  keeps <- c("date","Cuml")
  yy = new2[keeps]
  head(yy)
  tail(yy)
  yy=as.data.frame(yy)
    yy %>% 
    ggplot(aes(x=date,y=Cuml))+
    geom_line(color='red')+
    geom_point(size=1.5)+ 
    geom_area(fill='red')+
    theme_bw()+
    ggtitle('Daily Cummulative Cases')+
    transition_reveal(Cuml)
  anim_save('C:/Users/Rimal/Desktop/covidmap/Cumlplot22')
  
yy %>% 
ggplot(aes(x=date,y=Cuml))+
geom_line(color='red')+
geom_point(size=1.5)+ 
geom_area(fill='black')+
theme_bw()+
ggtitle('Daily line plot')+
transition_reveal(Cuml)
anim_save('C:/Users/Rimal/Desktop/covidmap/Cumlplot2') 


#daily line plot
setwd("C:/Users/Rimal/Desktop/covidmap")
data=read.csv("C:/Users/Rimal/Desktop/covidmap/covid.csv", header=T,stringsAsFactors = FALSE)
str(data)
data=as.data.frame(data)
head(data$date_confirmation)
datanew = data %>% 
  mutate(date_confirmation=dmy(date_confirmation))
head(datanew$date_confirmation)
str(datanew)
datanew$date_confirmation=as.Date(datanew$date_confirmation,"%m/%d/%Y")
datanew$date_confirmation
datanew$d=day(datanew$date_confirmation)
datanew$d
datanew$d=as.numeric(datanew$d)
class(datanew$d)
datanew$m=month(datanew$date_confirmation)
datanew$m
datanew$y=year(datanew$date_confirmation)
str(datanew)
tail(datanew,40)

new=datanew %>% 
  filter(m==3) %>% 
  group_by(d,country) %>% 
  summarise(count=n())
new
  new=data.frame(complete(new,d,country,
                          fill =list(count=0) ))
 new$country
new2=new %>% filter(country =='United States'|
                 country =='France'|
                 country =='United Kingdom'|
                 country =='Germany')
new2
ggplot(data=new2,aes(x=d,y=count,
                     group=country,
                     color=country))+
  geom_line()+
  geom_point()+
  scale_y_log10()+
    theme_bw()+ 
    ggtitle('Animinated line plot')+
    transition_reveal(d)


#daily line plot2
data=read.csv("C:/Users/Rimal/Desktop/Dynamic Animated Plots for Better COVID Cases Analysis/owid-covid-data2.csv", header=T,stringsAsFactors = FALSE,strip.white=TRUE)
library(lubridate)
library(tidyverse)
head(data)
str(data)
data=as.data.frame(data)
datanew=data %>% 
  mutate(date=ymd(date),format='%m%d%y')
head(data)
new2=data %>% mutate(date=lubridate::ymd(date),
                     year = lubridate::year(date), 
                     month = lubridate::month(date), 
                     day = lubridate::day(date))
new2$day=as.numeric(new2$day)
data=as.data.frame(data)
head(data$date)
datanew = data %>% 
  mutate(date=ymd(date))
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

new=data.frame(complete(df,d,location,
                        fill =list(count=0) ))
new$location
new2=new %>% filter(location =='United States'|
                      location =='France'|
                      location =='United Kingdom'|
                      location =='Germany'|
                      location =='India'|
                      location =='Russia'|
                      location =='Brazil'|
                      location=="Nepal")
new2
ggplot(data=new2,aes(x=d,y=Cuml,
                     group=location,
                     color=location))+
  geom_line()+
  geom_point()+
  scale_y_log10()+
  theme_bw()+ 
  ggtitle('Animinated line plot')+
  transition_reveal(d)


 #Barplot animination
data=read.csv("C:/Users/Rimal/Desktop/Dynamic Animated Plots for Better COVID Cases Analysis/owid-covid-data2.csv", header=T,stringsAsFactors = FALSE,strip.white=TRUE)
library(lubridate)
library(tidyverse)
head(data)
str(data)
data=as.data.frame(data)
datanew=data %>% 
  mutate(date=ymd(date),format='%y%m%d')
head(data)
new2=data %>% mutate(date=lubridate::ymd(date),
                     year = lubridate::year(date), 
                     month = lubridate::month(date), 
                     day = lubridate::day(date))
new2$day=as.numeric(new2$day)
data=as.data.frame(data)
head(data$date)
datanew = data %>% 
  mutate(date=ymd(date))
head(datanew$date)
str(datanew)
datanew$date=as.Date(datanew$date,"%y/%m/%d")
datanew$date
datanew$d=day(datanew$date)
datanew$d
datanew$d=as.numeric(datanew$d)
class(datanew$d)
datanew$m=month(datanew$date)
datanew$m
datanew$m=as.integer(datanew$m)
datanew$y=year(datanew$date)
str(datanew)
str(datanew)
new=datanew %>% 
  filter(location =='United States'|
           location =='Brazil'|
           location =='United Kingdom'|
           location =='India'|
           location=="Nepal") %>% 
  filter(m==1|m==2|m==3|m==4|m==5|m==6|m==7|m==8) %>% 
  group_by(location,m,total_cases) %>% 
  summarise(count=n()) %>% 
  mutate(Cuml=cumsum(total_cases))
p=  new %>% 
  ggplot(aes(x=location,
             y=Cuml,
             fill=location))+
  geom_bar(stat = 'identity')+
  geom_point(size=1.5)+
  scale_y_log10()+
  theme_bw()+
  guides(fill=F)
#Animation    
p+transition_time(as.integer(m))+
  labs(title='Animatiated Bar plot by Month',
       subtitle='Month:{frame_time}')
anim_save('C:/Users/Rimal/Desktop/Dynamic Animated Plots for Better COVID Cases Analysis/Cumlplot222') 
p+ transition_states(Cuml)+
  labs(title='Annimatiom bar plot for top countries')+
  shadow_mark()+
  enter_grow() 






#Barplot animination
setwd("C:/Users/Rimal/Desktop/covidmap")
data=read.csv("C:/Users/Rimal/Desktop/covidmap/covid.csv", header=T,stringsAsFactors = FALSE)
str(data)
data=as.data.frame(data)
head(data$date_confirmation)
datanew = data %>% 
  mutate(date_confirmation=dmy(date_confirmation))
head(datanew$date_confirmation)
str(datanew)
datanew$date_confirmation=as.Date(datanew$date_confirmation,"%m/%d/%Y")
datanew$date_confirmation
datanew$d=day(datanew$date_confirmation)
datanew$d
datanew$d=as.numeric(datanew$d)
class(datanew$d)
datanew$m=month(datanew$date_confirmation)
datanew$m
datanew$y=year(datanew$date_confirmation)
str(datanew)
tail(datanew,40)
str(datanew)
   new=datanew %>% 
     filter(country =='United States'|
     country =='France'|
     country =='United Kingdom'|
     country =='Germany') %>% 
     filter(m==2|m==3) %>% 
     group_by(country,m) %>% 
     summarise(count=n())
 p=  new %>% 
   ggplot(aes(x=country,
              y=count,
              fill=country))+
     geom_bar(stat = 'identity')+
     geom_point(size=1.5)+
   scale_y_log10()+
     theme_bw()+
     guides(fill=F)
#Animation    
p+transition_time(as.integer(m))+
  labs(title='Animatiated Bar plot by Month',
       subtitle='Month:{frame_time}')
p+ transition_states(count)+
labs(title='Annimatiom bar plot for top countries')+
shadow_mark()+
enter_grow() 


#Bobble plot1
data=read.csv("C:/Users/Rimal/Desktop/Dynamic Animated Plots for Better COVID Cases Analysis/owid-covid-data2.csv", header=T,stringsAsFactors = FALSE,strip.white=TRUE)
library(lubridate)
library(tidyverse)
head(data)
str(data)
data=as.data.frame(data)
head(data)
str(data)
data=as.data.frame(data)
datanew=data %>% 
  mutate(date=ymd(date),format='%y%m%d')
head(data)
new2=data %>% mutate(date=lubridate::ymd(date),
                     year = lubridate::year(date), 
                     month = lubridate::month(date), 
                     day = lubridate::day(date))
new2$day=as.numeric(new2$day)
data=as.data.frame(data)
head(data$date)
datanew = data %>% 
  mutate(date=ymd(date))
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
data2=datanew %>% 
  group_by(location,continent,date,gdp_per_capita,life_expectancy,total_cases,m,d) %>% 
  summarise(count=n()) %>% 
  mutate(total=cumsum(count))

p=ggplot(data2,
         aes(x=gdp_per_capita,y=life_expectancy,
             size=total,color=location ))+
  geom_point(show.legend=F,alpha=0.7)+
  scale_x_log10()+
  labs(x='GDP Per Capita',
       y='Life Expentancy')+
  scale_size(range=c(2,15))
p
#Animinated Bubble plot
p+transition_time(m)+
  labs(title = 'Year per Capital Vs Life Expectancy',
       subtitle='year:{frame_time}')+
  shadow_wake(0.5)
 anim_save('C:/Users/Rimal/Desktop/covidmap/monthplot') 

#Bubbble plot   time may be date  
p+transition_time(date)+
  labs(title='GDP VS Life',subtitle = 'Year:{frame_time}')+
  shadow_wake(0.5)+
  facet_wrap(~continent)
anim_save('C:/Users/Rimal/Desktop/covidmap/continent') 






#Bobble plot
attatch(gapminder)
write.csv(gapminder,'gapminder.csv')
gapminder=read.csv("C:/Users/Rimal/Desktop/covidmap/gapminder.csv", header=T,stringsAsFactors = FALSE)
head(gapminder)
p=ggplot(gapminder,
         aes(x=gdpPercap,y=lifeExp,
             size=pop,color=country))+
  geom_point(show.legend=F,alpha=0.7)+
  scale_x_log10()+
  labs(x='GDP Per Capita',
       y='Life Expentancy')+
  scale_size(range=c(2,15))
p
#Animinated Bubble plot
p+transition_time(year)+
  labs(title = 'Year per Capital Vs Life Expectancy',
       subtitle='year:{frame_time}')+
  shadow_wake(0.5)

#Bubbble plot     
p+transition_time(year)+
  labs(title='GDP VS Life',subtitle = 'Year:{frame_time}')+
  shadow_wake(0.5)+
  facet_wrap(~continent)







#Bobble plot
attatch(gapminder)
write.csv(gapminder,'gapminder.csv')
gapminder=read.csv("C:/Users/Rimal/Desktop/covidmap/gapminder.csv", header=T,stringsAsFactors = FALSE)
head(gapminder)
p=ggplot(gapminder,
         aes(x=gdpPercap,y=lifeExp,
                       size=pop,color=country))+
  geom_point(show.legend=F,alpha=0.7)+
  scale_x_log10()+
  labs(x='GDP Per Capita',
       y='Life Expentancy')+
  scale_size(range=c(2,15))
p
  #Animinated Bubble plot
p+transition_time(year)+
    labs(title = 'Year per Capital Vs Life Expectancy',
    subtitle='year:{frame_time}')+
  shadow_wake(0.5)

#Bubbble plot     
p+transition_time(year)+
  labs(title='GDP VS Life',subtitle = 'Year:{frame_time}')+
  shadow_wake(0.5)+
  facet_wrap(~continent)

    
