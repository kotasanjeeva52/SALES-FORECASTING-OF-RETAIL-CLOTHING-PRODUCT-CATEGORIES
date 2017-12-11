
library(readxl)
library(forecast)
library(lubridate)
library(DataCombine)
library(imputeTS)
library(plyr)
library(dplyr)
library(TTR)
library(graphics)
library(data.table)
library(Quandl)
library(DMwR)
library(gtools)
library(car)
getwd()
setwd("G:/PHD Hackthon")
event <- read_xlsx("Events_HolidaysData.xlsx")
sum(is.na(event))
macro <- read_excel("MacroEconomicData.xlsx")
sum(is.na(macro))



weather1 <- read_excel("WeatherData.xlsx", sheet = ("2009"))
weather2 <- read_excel("WeatherData.xlsx", sheet = ("2010"))
weather3 <- read_excel("WeatherData.xlsx", sheet = ("2011"))
weather4 <- read_excel("WeatherData.xlsx", sheet = ("2012"))
weather5 <- read_excel("WeatherData.xlsx", sheet = ("2013"))
weather6 <- read_excel("WeatherData.xlsx", sheet = ("2014"))
weather7 <- read_excel("WeatherData.xlsx", sheet = ("2015"))
weather8 <- read_excel("WeatherData.xlsx", sheet = ("2016"))
data = read.csv("Train.csv")
test = read.csv("template.csv")
weather2$Year<-recode(weather2$Year,"c('2009')='2010'")
weather3$Year<-recode(weather3$Year,"c('2009')='2011'")
weather4$Year<-recode(weather4$Year,"c('2009')='2012'")
weather5$Year<-recode(weather5$Year,"c('2009')='2013'")
weather6$Year<-recode(weather6$Year,"c('2009')='2014'")
weather7$Year<-recode(weather7$Year,"c('2009')='2015'")
weather8$Year<-recode(weather8$Year,"c('2009')='2016'")
sum(is.na(data))

train1 <- na.ma(data, k = 4, weighting = "simple")
train = subset(train1, ProductCategory=="WomenClothing")
Train_data<-transform(train, Date = as.Date(paste(Year, Month, 1, sep = "-")))

#View(Train_data)



weather = rbind(weather1,weather2,weather3,weather4,weather5,weather6,weather7,weather8)
weather<-as.data.frame(weather)
dim(weather)

library(dplyr)
col_numeric<-select(weather,4:21)
col_fac<-select(weather,1:3,22,23)


col_charec<-lapply(col_numeric,as.character)
col_numer<-lapply(col_charec,as.numeric)


weather_o<-cbind(col_numer,col_fac)
names(weather)


levels(weather$WeatherEvent)
#View(weather)
colnames(weather)
sum(is.na(weather))






attach(weather)
#plot(weather_o$VisibilityÂ..km..avg~weather_o$WeatherEvent)


weat_f<-filter(weather_o,WeatherEvent %in% c("Fog"))
str(weat_f)
#View(weat_f)
#mean(weat_f$VisibilityÂ..km..avg)
#mean(weat_f$Temp.avg..Â.C.)


weat_fr<-filter(weather_o,WeatherEvent %in% c("Fog , Rain"))
str(weat_fr)
#View(weat_fr)
#mean(weat_fr$VisibilityÂ..km..avg)
#mean(weat_fr$Temp.avg..Â.C.)


weat_frs<-filter(weather_o,WeatherEvent %in% c("Fog , Rain , Snow"))
str(weat_frs)
#View(weat_fr)
#mean(weat_frs$VisibilityÂ..km..avg)
#mean(weat_fr$Temp.avg..Â.C.)




weat_T<-filter(weather_o,WeatherEvent %in% c("Thunderstorm"))
str(weat_T)
#View(weat_T)
#mean(weat_T$VisibilityÂ..km..avg)
#mean(weat_T$Temp.avg..Â.C.)

weat_na<-filter(weather_o,is.na(WeatherEvent))
#View(weat_na)
str(weat_na$WeatherEvent)
dim(weat_na)
#mean(weat_na$Temp.avg..Â.C.,na.rm=TRUE)
#mean(weat_na$VisibilityÂ..km..avg,na.rm=TRUE)


#sum(is.na(weat_na$Temp.avg..Â.C.))


weat_notna<-filter(weather_o,!is.na(WeatherEvent))
dim(weat_notna)
#mean(weat_notna$VisibilityÂ..km..avg,na.rm=TRUE)
#mean(weat_notna$Temp.avg..Â.C.,na.rm=TRUE)
sum(is.na(weat_notna))

##Imputation OF NA Values in Weather EVENT 

weather_o$WeatherEvent<-factor(weather_o$WeatherEvent,levels = c(levels(weather_o$WeatherEvent),"Normal"))
levels(weather_o$WeatherEvent)
weather_o$WeatherEvent[is.na(weather_o$WeatherEvent)]<-"Normal"
sum(is.na(weather_o$WeatherEvent))


sum(is.na(weather_o))

sapply(weather_o,function(x) sum(is.na(x)))
weather_o11<-weather_o[which(is.na(weather_o$Month)),]

str(weather_o)


weather_o<-weather_o[-which(is.na(weather_o$Month)),]

##IMpute NUll VALUES for remaining columns

library(zoo)
weather_o<-na.locf(weather_o)


############# NULL VALUES WERE IMPUTED ###############


#############  GROUPING  ########################

col_numeric<-select(weather_o,1:18)
col_fac<-select(weather_o,19:23)

str(col_numer)


col_numer<-as.data.frame(lapply(col_numeric,as.numeric))
col_facc<-as.data.frame(lapply(col_fac,as.factor))

weather_o<-cbind(col_numer,col_facc)
#View(weather_o)
install.packages("tidyr")
library(tidyr)
group_by()


wea_tem<-weather_o %>% 
  group_by(Month,Year) %>% 
  summarise("mean_Temp"=mean(weather$Temp.avg..Â.C.)) %>%
  arrange(Year)
#View(wea_tem)
colnames(weather_o)

wea_dew<-weather_o %>% 
  group_by(Month,Year) %>% 
  summarise("mean_dew"=mean(weather$Dew.Point.avg..Â.C.)) %>%
  arrange(Year)

#View(wea_dew)

wea_hum<-weather_o %>% 
  group_by(Month,Year) %>% 
  summarise("mean_hum"=mean(weather$HumidityÂ.....avg)) %>%
  arrange(Year)

str(wea_hum)
colnames(weather_o)



#weather_data<-transform(weather, Date = as.Date(paste(Year, Month, Day, sep = "-")))
#View(weather_data)


#View(weather)

data<-rbind.fill(list(Train_data, weather))


#View(data)

names(weather)

class(weather)
dim(weather)
summary(weather)
sum(is.na(weather))
str(weather)

sum(is.na(weather$Year))


weather_Final <- centralImputation(data = weather) #Cenral Imputation
sum(is.na(weather_Final))

#df3_imputed1 <- knnImputation(data = weather,k=5) #KNN Imputation
#sum(is.na(df3_imputed1))

merge=rbind.fill(train,weather_Final,event,macro)

dim(merge)
head(merge)
View(merge)

sum(is.na(merge))

#lapply(Final_data, levels)

#x<-factor(Final_data$Sales.In.ThousandDollars.)


Final_data<-centralImputation(data = merge)


sum(is.na(Final_data))
#View(Final_data)
Final_data$`AdvertisingExpenses (in Thousand Dollars)`=NULL

names(Final_data$Sales.In.ThousandDollars.)
str(Final_data)
Final_data$Sales.In.ThousandDollars.
Final_data$ProductCategory
Final_data$`Dew Point high (°C)`=NULL
Final_data$`Humidity (%) low`=NULL
Final_data$`Sea Level Press. (hPa) high`=NULL
Final_data$`Sea Level Press. (hPa) avg`=NULL
Final_data$`Sea Level Press. (hPa) low`=NULL
Final_data$`Wind (km/h) low`=NULL
Final_data$`Wind (km/h) avg`=NULL
Final_data$`Wind (km/h) high`=NULL
Final_data$`Precip. (mm) sum`=NULL
Final_data$`Year-Month`=NULL
library(caret)
library(MASS)
library(readr)
library(caTools)
library(e1071)
library(rpart)
library(rpart.plot)

#View(Final_data)
table(Final_data$Year)
#Final_data[Final_data$Year == "2012.5" ] = NULL
#Final_data1 = Final_data
set.seed(987)
split <- sample.split(Final_data$Sales.In.ThousandDollars., SplitRatio = 0.75)
train <- subset(Final_data, split == T)

test <- subset(Final_data, split == F)
colnames(train)
colnames(test)


train.data = Final_data[which(Final_data$Year < as.POSIXct("2015", format="%Y")),]
test.data = Final_data[which(Final_data$dateRange >= as.POSIXct("2016",format="%Y")),]








library(rpart)
library(caret)
rpart(Sales.In.ThousandDollars.~.,data =train.data)->rf
summary(rf)


prediction1 = predict(rf, train.data)



regr.eval(train$Sales.In.ThousandDollars., prediction1)

prediction.test = predict(rf, test.data)

prediction.test


#regr.eval(test.data$Sales.In.ThousandDollars., prediction.test)


write.csv(x = prediction.test,file = "sanjeeva.csv")

#predict(rf,test$Sales.In.ThousandDollars.)


#mytree <- rpart(Sales.In.ThousandDollars. ~ ., data = train, method = "class", minsplit = 2, minbucket = 1)





























