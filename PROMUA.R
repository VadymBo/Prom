require('data.table') || install.packages('data.table') 
library(data.table)


data<-fread('2013.csv', colClasses='character')
data<-as.data.frame(data)


cl.kiev<-data[data$V1=="UPM00033345",]
cl.bor<-data[data$V1=="UPM00033347",]

### as we can see there is no data for Boryspil
any(data$V1=="UPM00033345")
any(data$V1=="UPM00033347")


##  subset data
kiev<-cl.kiev[,2:4]
stopifnot(!any(is.na(kiev)))
names(kiev)<-c("date", "label", "value")

kiev$year<-substr(kiev$date, start=1, stop=4)
kiev$month<-substr(kiev$date, start=5, stop=6)
unique(kiev$year)

kiev<-kiev[,c("month", "label", "value")]

kiev$value<-as.numeric(kiev$value)
kiev$month<-as.numeric(kiev$month)



### function for calculating mean, max, min for the kiev data
calc<- function(data, label, FUN)
{ 
  vals<-data.frame(month=1:12, val=0)
  for( i in 1:12)
  { vals[i,2]<- FUN(data[data$month==i & data$label==label,'value']) }
  
  ## all values are in (tenths of mm,C), the result should be in mm,C!!
  vals[,2]<-round(vals[,2]/10,2)
  
  vals[,1]<-month.name[vals[,1]]
  vals
}




nms<-month.name[1:12]
### MAX TEMPERATURE
max.t<-calc(kiev, 'TMAX', max)

barplot(max.t[,2], main="Max temperature in Kiev, 2013",  ylab="temperature, C", names.arg=nms, las=2, col='lightblue' )

### MIN TEMPERATURE
min.t<-calc(kiev, "TMIN", min)

barplot(min.t[,2], main="Min temperature in Kiev, 2013",  ylab="temperature, C", names.arg=nms, las=2, col='lightblue' )

## MEAN  Precipitation
mean.p<-calc(kiev,'PRCP', mean)

barplot(mean.p[,2], main="Mean precipitation in Kiev, 2013",  ylab="precipitation, mm", names.arg=nms, las=2, col='lightblue' )
