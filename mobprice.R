getwd()
setwd("C:/Users/Gowri/OneDrive/Desktop/1 SEM/FDA")

Mob_data=read.csv("Mobile Price.csv",head=TRUE,sep=",")
Mob_data
Mobile_data=(Mob_data[c("battery_power","clock_speed","fc","int_memory","m_dep","mobile_wt","n_cores","pc","px_height","px_width","ram","sc_h","sc_w","talk_time")])
Mobile_data
write.csv(Mobile_data$int_memory,"mobile total data.csv", row.names =TRUE)
cate_data=Mob_data[c("blue","dual_sim","four_g","three_g","touch_screen","wifi","price_range")]
cate_data




install.packages("dplyr")
library("dplyr") #dplyr is used to cal columns
install.packages("ggplot2")
library("ggplot2") #ggplot2 is used to get graphs


inner_join_result <- merge(Mobile_data,cate_data, by.x = "fc", by.y = "wifi", all.x = TRUE)
inner_join_result

inn=merge(Mobile_data,cate_data )

sum(is.na(Mob_data))
na.omit(Mob_data)
#column names
name_data=data.frame(colnames(Mobile_data))

#columns mean data
mean_data=data.frame(colMeans(Mobile_data))


#columns median data
median_data=data.frame(apply(Mobile_data,2,median))
median_data

#columns range data
range_data=data.frame(apply(Mobile_data,2,range))

#columns standard deviation data
sd_data=data.frame(apply(Mobile_data,2,sd))
write.csv(sd_data,"mobile SD data.csv", row.names =TRUE)
sd_data

#columns varaince data  
var_data=data.frame(apply(Mobile_data,2,var))
write.csv(var_data,"mobile varianc data.csv", row.names =TRUE)
var_data

#columns summary data
sum_data=data.frame(apply(Mobile_data,2,summary))
write.csv(sum_data,"mobile summary data.csv", row.names =TRUE)

z=7.28884036
l=pnorm(-abs(z))

# Own Mode method to calculate mode
mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Evaluate mode of numbers
mode_data <- data.frame(apply(Mobile_data,2,mode))
mode_data
catemode_data= data.frame(apply(cate_data,2,mode))
catemode_data

#1 battery power
hist(Mobile_data$battery_power, freq = FALSE, main = "Histogram and density",xlab ="Total battery_power")
lines(density(Mobile_data$battery_power),col="red",lwd=3)
abline(v=mean(Mobile_data$battery_power),col="purple",lwd=2)
boxplot((Mob_data$battery_power),main="Box plot of battery power",ylab="battery_power",col="blue")

#2 clock speed
hist(Mobile_data$clock_speed, freq = FALSE, main = "Histogram and density",xlab ="Clock_ Speed")
lines(density(Mobile_data$clock_speed),col="red",lwd=3)
abline(v=mean(Mobile_data$clock_speed),col="purple",lwd=2)
boxplot((Mob_data$battery_power),main="Box plot of a Clock speed",ylab="clock speed",col="green")

#3front camera
hist(Mobile_data$fc, freq = FALSE, main = "Histogram and density",xlab ="fc")
lines(density(Mobile_data$fc),col="red",lwd=3)
abline(v=mean(Mobile_data$fc),col="purple",lwd=2)
boxplot((Mob_data$battery_power),main="Box plot of a front camera",ylab="FC",col="yellow")

#4internal memory
hist(Mobile_data$int_memory, freq = FALSE, main = "Histogram and density",xlab ="Internal Memory")
lines(density(Mobile_data$int_memory),col="red",lwd=3)
abline(v=mean(Mobile_data$int_memory),col="purple",lwd=2)
boxplot((Mob_data$int_memory),main="internal memory summary",ylab="internal memomry",col="orange")

#5mobile depth
hist(Mobile_data$m_dep, freq = FALSE, main = "Histogram and density",xlab ="M_depth")
lines(density(Mobile_data$m_dep),col="red",lwd=3)
abline(v=mean(Mobile_data$m_dep),col="purple",lwd=2)
boxplot((Mob_data$battery_power),main="Box plot of m_depth summary",ylab="m_depth",col="blue")

#6 n_cores
hist(Mobile_data$n_cores, freq = FALSE, main = "Histogram and density",xlab ="N_cores")
lines(density(Mobile_data$n_cores),col="red",lwd=3)
abline(v=mean(Mobile_data$n_cores),col="purple",lwd=2)
boxplot((Mob_data$mobile_wt),main="Box plot of n_cores summary",ylab="n_cores",col="yellow")

#7mobile weight 
hist(Mobile_data$mobile_wt, freq = FALSE, main = "Histogram and density",xlab ="Mobile_Weight")
lines(density(Mobile_data$mobile_wt),col="red",lwd=3)
abline(v=mean(Mobile_data$mobile_wt),col="purple",lwd=2)
boxplot((Mob_data$mobile_wt),main="mobile_weight summary",ylab="mobile_wt",col="blue")

#8primary camera
hist(Mobile_data$pc, freq = FALSE, main = "Histogram and density",xlab ="PC")
lines(density(Mobile_data$pc),col="red",lwd=3)
abline(v=mean(Mobile_data$pc),col="purple",lwd=2)
boxplot((Mob_data$fc),main="boxplot pc  summary",ylab="primary camera",col="green")

#9pixel height
hist(Mobile_data$px_height, freq = FALSE, main = "Histogram and density",xlab ="Px_height")
lines(density(Mobile_data$px_height),col="red",lwd=3)
abline(v=mean(Mobile_data$px_height),col="purple",lwd=2)
boxplot((Mob_data$px_height),main="boxplot pixel height summary",ylab="px_height",col="yellow")

#10pixel width
hist(Mobile_data$px_width, freq = FALSE, main = "Histogram and density",xlab ="Px_width")
lines(density(Mobile_data$px_width),col="red",lwd=3)
abline(v=mean(Mobile_data$px_width),col="purple",lwd=2)
boxplot((Mob_data$px_width),main="boxplot pixel width summary",ylab="px_width",col="grey")

#11RAM
hist(Mobile_data$ram, freq = FALSE, main = "Histogram and density",xlab ="RAM")
lines(density(Mobile_data$ram),col="red",lwd=3)
abline(v=mean(Mobile_data$ram),col="purple",lwd=2)
boxplot((Mob_data$ram),main="boxplot RAM  summary",ylab="RAM",col="green")

#12screen height
hist(Mobile_data$sc_h, freq = FALSE, main = "Histogram and density",xlab ="SC_H")
lines(density(Mobile_data$sc_h),col="red",lwd=3)
abline(v=mean(Mobile_data$sc_h),col="purple",lwd=2)
boxplot((Mob_data$sc_h),main="boxplot screen hieght  summary",ylab="sc_h",col="blue")

#13screen weight
hist(Mobile_data$sc_w, freq = FALSE, main = "Histogram and density",xlab ="Sc_w")
lines(density(Mobile_data$sc_w),col="red",lwd=3)
abline(v=mean(Mobile_data$sc_w),col="purple",lwd=2)
boxplot((Mob_data$sc_w),main="boxplot screen width  summary",ylab="sc_w",col="orange")

#14talk time
hist(Mobile_data$talk_time, freq = FALSE, main = "Histogram and density",xlab ="Talk Time")
lines(density(Mobile_data$talk_time),col="red",lwd=3)
abline(v=mean(Mobile_data$talk_time),col="purple",lwd=2)
boxplot((Mob_data$talk_time),main="boxplot talk_time  summary",ylab="talk time",col="yellow")




hist(cate_data$blue, freq = FALSE, main = "Histogram and density",xlab ="Talk Time")
hist(cate_data$dual_sim, freq = FALSE, main = "Histogram and density",xlab ="Talk Time")
hist(cate_data$price_range, freq = FALSE, main = "Histogram and density",xlab ="Talk Time")

x<-Mobile_data$battery_power
dens<-dnorm(x, mean=1238.58,sd=439.4)
plot(x,dens, main = "x normal distribution of battery power")
abline(v=1235.518,col="red",lwd=3)

y<-Mobile_data$clock_speed
dens<-dnorm(y, mean=1.552,sd=0.816)
plot(y,dens,  main = "x normal distribution of Clock speed")
abline(v=1.552,col="red",lwd=3)

z<-Mobile_data$int_memory
dens<-dnorm(z, mean=32.0465,sd=18.145)
plot(z,dens  , main = "x normal distribution of internal memory")
abline(v=32.0465,col="red",lwd=3)

a<-Mobile_data$mobile_wt
dens<-dnorm(a, mean=140.249,sd=35.39)
plot(a,dens  , main = "x normal distribution of mobile weight")
abline(v=140.249,col="red",lwd=3)









boxplot((Mob_data$battery_power),main="Box plot of battery power",ylab="battery_power",col="blue")
abline(h=mean(Mob_data$clock_speed),col="purple",lwd=2,)

boxplot((Mob_data$fc),main="fc summary",ylab="front Camera",col="green")

boxplot((Mob_data$int_memory),main="internal memory summary",ylab="internal memomry",col="orange")

boxplot((Mob_data$m_dep),main="m_depth summary",ylab="mdepth",col="pink")

boxplot((Mob_data$mobile_wt),main="mobile weight summary",ylab="battery_power",col="yellow")

boxplot((Mob_data$n_cores),main="mobile n_cores summary",ylab="n_cores",col="green")

boxplot((Mob_data$pc),main="mobile pc summary",ylab="pc",col="yellow")

boxplot((Mob_data$ram),main="mobile RAM summary",ylab="RAM",col="green")


install.packages("moments") 
library(moments)
skew_data=data.frame(apply(Mobile_data,2,skewness))
plot(skewness(Mobile_data$battery_power))
kurt_data=data.frame(apply(Mobile_data,2,kurtosis))

plot(density(Mob_data$battery_power))

x=summary(Mob_data$battery_power)
y=c(1,2,3,4,5)
summary(y)

 percentile=ecdf(Mob_data$battery_power)
plot(percentile)

###Plotting the x and checking skewness and Kurtosis

hist(Mobile_data$battery_power, freq = FALSE, main = "Histogram and density",xlab ="Total battery_power")

lines(density(Mobile_data$battery_power),col="red",lwd=3)

abline(v=mean(Mob_data$battery_power),col="purple",lwd=2)





 # Calculate density
dx <- density(Mob_data$battery_power)


 help(density)

# Add density
lines(dx, lwd = 2, col = "red")

# Plot the density without histogram
plot(dx, lwd = 2, col = "red",
     main = "Density")

col_name=c("battery_power","clock_speed","fc")

for (item in col_name) {
  print(item)
  
  print(mean(Mob_data[[item]]))
  
}
install.packages("dplyr")
library("dplyr")
colMeans(Mob_data)

hist(Mob_data$mobile_wt,type="l")
abline(v=summary(Mob_data$mobile_wt),col="purple",lwd=2)



