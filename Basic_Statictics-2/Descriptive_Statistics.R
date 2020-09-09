airquality<-datasets::airquality
airquality
head(airquality)  #to see first few rows
tail(airquality)  #to see last few rows
airquality[,c(1,2)]     #to show the first 2 columns
airquality[,c(5,6)]     #to show the last 2 columns
airquality$Temp
summary(airquality$Ozone)  #to plot the summarise data.i.e mean,median,mode
summary(airquality$Temp)
plot(airquality$Ozone)
plot(airquality$Ozone,airquality$Temp)  
plot(airquality)              #scatterplot
summary(airquality)

#points and lines
plot(airquality$Ozone,type="l") #p:point,l:lines,b: both,h
plot(airquality$Ozone, xlab = "ozone concentration",ylab = "No of Instances", main = "ozone levels in NY city", col="blue")

#hORIZONTAL Barplot
barplot(airquality$Ozone,main = "ozone Concentrationin air",xlab = "ozone levels",col = "PINK",horiz = FALSE)


#Histogram
hist(airquality$Solar.R,col = 'green')
hist(airquality$Solar.R,main = 'Solar Radiation Values in air',xlab = 'Solar rad',col = 'pink')

#single box plot
boxplot(airquality$Solar.R)


#Multiple Box plot
boxplot(airquality[,1:4],main ='Multiple')


#to plot various graphs in one 

par(mfrow=c(3,3),mar=c(2,5,2,1),las = 0,bty="n")  #mfrow is used to partition and mar is used for margin length
plot(airquality$Ozone)
plot(airquality$Ozone,airquality$Temp)
plot(airquality$Ozone,type="c")
plot(airquality$Ozone,type="s")
plot(airquality$Ozone,type="h")
barplot(airquality$Ozone,main = "ozone Concentrationin air",xlab = "ozone levels",col = "PINK",horiz = FALSE)
hist(airquality$Solar.R,col = 'brown')
boxplot(airquality$Solar.R)
boxplot(airquality[,0:4],main ='Multiple box plots')