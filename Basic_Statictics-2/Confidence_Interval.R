#In R to Calculate confidence interval use library(gmodels) 
#ci(sample)


#first install nycflights13 package
data<-nycflights13::flights
data
dep_delay<-data$dep_delay  #used to store a particular column data in data frame
ar_delay<-data$arr_delay  #used to store a particular column data in data frame

dep_delay
ar_delay


dep_delay1<-dep_delay[!is.na(dep_delay)]
ar_delay1<-ar_delay[!is.na(ar_delay)]

library(gmodels)  #install this package
ci(dep_delay)     #used to display the confidence interval for dep_delay
ci(ar_delay)      #used to display the confidence interval for ar_delay
ci(dep_delay1)    
ci(ar_delay1)