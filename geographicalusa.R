library(leaflet)
library(tidyverse)
library(ggmap)
library(htmltools)
library(leaflet.extras)
library(maps)
library(ggplot2)
library(mapproj)
library(mapdata)
library(spData)
setwd("C:/Users/Rimal/Desktop/covidmap")
data <- read.csv("C:/Users/Rimal/Desktop/covidmap/worldcities.csv",stringsAsFactors = F,encoding = 'UTF-*')
tbl_df(data)
head(data)
str(data)
uss=data %>% filter(country=='United States') 
str(uss)
w=map_data('world')# world map data longit,lati group, region country
head(w)
inc=map_data('world',region=c('Nepal','India','China'))
inc
ggplot (inc,aes(x=long,y=lat, group=group,fill=region))+
  geom_polygon(color='black')+
  coord_map('polyconic')# not stretch
usa=map_data('state')
str(usa)
ggplot (usa,aes(x=long,y=lat, group=group,fill=region))+
geom_polygon(color='black')+
guides(fill=F)
library(ggfortify)
library(mapdata)
library(maps)
library(ggplot2)
jp <- ggplot2::map_data('world2', 'japan')
class(jp)
str(jp)
ggplot(jp, aes(x = long, y = lat, group = group)) +
  geom_polygon()
c=read.csv("C:/Users/Rimal/Desktop/covidmap/data/us_confirmed_csv.csv", header=T,sep=",")
str(c)
head(c)
ct=tbl_df(c)
head(ct)
ct$Long=formatC(ct$Long, digits = 3,format = "f")
ct$Lat=formatC(ct$Lat, digits = 3,format = "f")
ct$Long=as.numeric(ct$Long)
ct$Lat=as.numeric(ct$Lat)
tail(ct)
library(tidyverse)
#https://www.ecdc.europa.en/stes/default/files/documents/COVID-19-geographic-distribution-worldwide-2020-06-02.xls
us=ct %>% 
  filter(Country.Region =='US') 
cc=us %>% 
  group_by(Province.State) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count))
head(cc)
cc$Province.State=tolower(cc$Province.State)
cc=na.omit(cc)
tail(cc)
#merging two data sets
head(data) 
s=map_data('state')
ss=s
ss$region=tolower(ss$region)
ss$subregion=tolower(ss$subregion)
ss=data.frame(ss)
tail(ss)
str(ss)
cc=as.data.frame(cc)
head(cc)
data1= merge(ss,cc,
            by.x='region',
            by.y='Province.State')

str(data1)
keeps <- c("region","long","lat","group","count")
data1 = data1[keeps]
head(data1)
str(data1)
ggplot(data1,aes(x=long,y=lat,
                 group=group,
                 fill=count))+
  geom_polygon(color='gray')+
  coord_map('polyconic')+
  scale_fill_gradient2(low='white',high='red')+
  theme_void()+
  ggtitle('Covid Case in US')

str(c)
head(c)
library(tidyverse)
ct=tbl_df(c)
head(ct)
s=map_data('state')
ss=s
ss$region=tolower(ss$region)
ss$subregion=tolower(ss$subregion)
ss=data.frame(ss)
tail(ss)
str(ss)
keeps <- c("region","long","lat","group")
ss = ss[keeps]
head(ss)
cc=as.data.frame(ct)
head(cc)
cc=as.data.frame(cc)
cy=cc %>% 
  group_by(Combined_Key,Province.State) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count))
cy$Province.State=tolower(cy$Province.State)
head(cy)
data11= merge(cy,ss,
             by.x='Province.State',
             by.y='region')
head(data11)
data11$long=as.numeric(round(data11$long,digits=2))
data11$lat=as.numeric(round(data11$lat,digits=2))
names(data11)
names(data11)[names(data11) == "long"] <- "longitude"
names(data11)[names(data11) == "lat"] <- "latitude"
data11$Province.State=tolower(data11$Province.State)
head(data11)
str(data11)
#Third New York
library(tmap)
library(magrittr)
library(spData)
library(leaflet)
leaflet() %>% addTiles() %>% 
addProviderTiles('CartoDB') %>%   # to specify the providers
  setView(lng=-73.935242,lat=40.730610,zoom=5) %>%  # setting view on  ysing long and latitute
  addCircleMarkers(lng=-73.935242,lat=40.730610,radius=5,color='red')
# UMas-Dartmouth
library(tmap)
library(magrittr)
library(spData)
library(leaflet)
leaflet() %>% addTiles() %>% 
  addProviderTiles('CartoDB') %>% 
  setView(lng=-71.0060,lat=41.6287, zoom=5) %>% 
  addCircleMarkers(lng=-71.0060,lat=41.6287,radius=5,color='red')

# pokhara 
library(tmap)
library(magrittr)
library(spData)
library(leaflet)
leaflet() %>% addTiles() %>% 
  addProviderTiles('CartoDB') %>% 
  setView(lng=83.959518,lat= 28.209499, zoom=5) %>% 
  addCircleMarkers(lng=83.959518,lat= 28.209499, 
                   radius=2,color='#6eeb34')

mycolor=colorNumeric(palette = 'RdBu',
                     domain = c(1:1000),reverse = T)
library(tidyverse)
cyy=cc %>% 
  group_by(Province.State) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count))
cyy$Province.State=tolower(cyy$Province.State)
head(cyy)
sss=data
str(sss)
library(tidyverse)
st=sss %>% 
  filter(country =="United States")
str(st)
st$admin_name=tolower(st$admin_name)

dataf= merge(cyy,st,
              by.x='Province.State',
              by.y='admin_name')

head(dataf)
str(dataf)
mycolor=colorNumeric(palette = 'RdBu',
                     domain = c(1:1500),reverse = T)
dataf$Province.State=toupper(dataf$Province.State)
dataf %>%   
  leaflet() %>%  
  addProviderTiles("OpenStreetMap") %>% 
  addCircleMarkers(radius=~0.0001*count,
                   color=~mycolor(count),
                   popup = ~paste0(Province.State,"<br/>",city_ascii,"<br/>",population,
                                   "<br/>", 
                                   count) ) %>%   
  
addLegend(pal=mycolor,
          values = c(1:1500),
          opacity = 0.75,
          title = 'Count',
          position='topright') %>% 
  setView(lng=-71.0060,lat=41.6287, zoom=4)

#map of two state
str(dataf)
names(dataf)[names(dataf) == "lng"] <- "longitude"
names(dataf)[names(dataf) == "lat"] <- "latitude"
usa2=dataf %>%  
  filter(Province.State =='MASSACHUSETTS'| Province.State=="NEW YORK"|Province.State=="TEXAS")
str(usa2)
usa3=usa2 %>% 
  group_by(city,Province.State,longitude,latitude,count) %>% 
  summarise(countt=n()) %>% 
  arrange(desc(countt))
str(usa3)
head(usa3)
library(tmap)
library(magrittr)
library(spData)
library(leaflet)
library(RCurl)

usa3=na.omit(usa3)
map=usa3%>% 
  leaflet() %>% 
  addProviderTiles('OpenStreetMap') %>% 
  addCircleMarkers(radius =2,
                   color='red',
                   popup = ~city) 
map %>% addMarkers()
map %>% clearMarkers()

mycolor1=colorFactor(palette  =c('red','blue','yellow'),
                    levels=c('MASSACHUSETTS','NEW YORK','TEXAS'))
map %>% 
  addCircleMarkers(data=usa3,
                   radius=2,
                   color= ~mycolor1(Province.State),
                   label = ~paste0(city," (",Province.State,")"))
#search
leaflet() %>% 
  addTiles() %>% 
  addSearchOSM() %>% 
  addReverseSearchOSM()
str(usa3)
head(usa3)
library(tmap)
library(magrittr)
library(spData)
library(leaflet)
library(RCurl)
library(RJSONIO)
library(rgeos)
MA=filter(usa3,Province.State=='MASSACHUSETTS')
m=leaflet() %>% 
  addProviderTiles('CartoDB') %>% 
  addCircleMarkers(data=MA,
                   radius = 5,
                   label = ~htmlEscape(city),
                   color = 'blue',
                   group = 'MASSACHUSETTS')
m
NY=filter(usa3,Province.State=='NEW YORK')
m=m %>% 
    addCircleMarkers(data=NY,
                   radius = 5,
                   label = ~htmlEscape(city),
                   color = 'red',
                   group = 'NEW YORK') %>%  
addLayersControl(overlayGroups = 
                   c('MASSACHUSETTS','NEW YORK'))
m
T=filter(usa3,Province.State=='TEXAS')
m=m %>% 
  addCircleMarkers(data=T,
                   radius = 5,
                   label = ~htmlEscape(city),
                   color = 'yellow',
                   group = 'TEXAS') %>%  
  addLayersControl(overlayGroups = 
                     c('MASSACHUSETTS','NEW YORK','TEXAS'))
m
#toggleling with railway
library(leaflet)
library(tmap)
library(magrittr)
library(spData)
library(leaflet)
m1=leaflet() %>% 
  addTiles(group='OpenRailwayMap') %>% 
  addProviderTiles("HERE",group = 'HERE') %>% 
  addCircleMarkers(data=MA,radius = 5,
                   label=~htmlEscape(city),
                   color = 'blue',
                   group = 'MASSACHUSETTS') %>% 
  addCircleMarkers(data=NY,
                   radius = 5,
                   label = ~htmlEscape(city),
                   color='red',
                   group = 'NEW YORK') %>% 
  addCircleMarkers(data=T,
                   radius = 5,
                   label = ~htmlEscape(city),
                   color = 'yellow',
                   group = 'TEXAS') %>% 
  addLayersControl(baseGroups = c('OpenRailwayMap','HERE'),
                     overlayGroups =   c('MASSACHUSETTS','NEW YORK','TEXAS'))

m1
#cluster
dataf %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(radius = 2,
                   label = ~htmlEscape(city),
                   color = 'red',
                   clusterOptions = markerClusterOptions())
  
      