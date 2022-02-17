rm(list=ls())
library(tidyverse)
library(zoo)


##############################
###       parameter        ###
##############################

## x-data 
category_x <- "Hosp"

## y-data  
category_y <- "HospCapacity"

# data age
data_age <- -0


wave_3 <- seq(as.Date("2021/02/15"), as.Date("2021/06/25"), 1)
wave_4 <- seq(as.Date("2021/06/26"), as.Date("2021/10/10"), 1)
wave_5 <- seq(as.Date("2021/10/11"), as.Date("2021/12/15"), 1)
wave_6 <- seq(as.Date("2021/12/16"), Sys.Date()-2+data_age, 1)

start_4 <- (length(wave_3)+1)
start_5 <- (length(wave_3)+length(wave_4)+1)
start_6 <- (length(wave_3)+length(wave_4)+length(wave_5)+1)

## latest FOPH data
## download latest data (CSV!) from: https://www.covid19.admin.ch/ 
## path to folder with extracted files (starts from home) 

path <- "~/documents/data/"






##############################
###       functions        ###
##############################

moving_average <- function(x, n = 14) {             
  stats::filter(x, rep(1 / n, n), sides = 2)
}

##############################
###       read data        ###
##############################

filename_x <- paste("",path,"COVID19",category_x,"_geoRegion.csv",sep="")
filename_y <- paste("",path,"COVID19",category_y,"_geoRegion.csv",sep="")

data_x <- read.table(filename_x, sep=",", header=TRUE)
data_y <- read.table(filename_y, sep=",", header=TRUE)


days <- seq.Date((wave_3[1]), Sys.Date()-2+data_age, 1)


  par_x <- NULL
  par_y <- NULL
  j <- 1
  for(j in 1:length(days)) {
    
 par_x <-  c(par_x, subset(data_x, datum == (days[j]) & geoRegion == "CH" )[1,3])
 par_y <-  c(par_y, subset(data_y, date == (days[j]) & geoRegion == "CH" )[1,7])
    j <- j+1
  }
 
  
##############################
###         calc           ###
##############################

  par_x
  par_y

  
  
## change NA with value before
 par_y <- na.locf(par_y) 
  
  
m_a_par_x <- moving_average(par_x)
m_a_par_y <- moving_average(par_y)


##############################
###         plot           ###
##############################


subtitle <- paste("data: FOPH - ",Sys.Date()+data_age,", Plot v0.1 Fabian Spühler (Twitter: @fabianspuehler)",sep="")
main_title <- paste("Phase portrait of Switzerland COVID hospitalisations",sep="")



plot(m_a_par_x, m_a_par_y, sub=subtitle, ylab="Beds full", xlab="Daily admissions", main = main_title,
     ylim=c(0,max(m_a_par_y, na.rm = TRUE)*1.1), xlim=c(0,max(m_a_par_x, na.rm = TRUE)*1.1),
     pch=1, lwd = 1, col = "white")



points(m_a_par_x[1:length(wave_3)], m_a_par_y[1:length(wave_3)], col="red")

points(m_a_par_x[start_4:start_5-1], m_a_par_y[start_4:start_5-1], col="blue")

points(m_a_par_x[start_5:start_6-1], m_a_par_y[start_5:start_6-1], col="green")

points(m_a_par_x[start_6:length(m_a_par_x)-8], m_a_par_y[start_6:length(m_a_par_x)-8], col="black")


points(m_a_par_x[(length(m_a_par_x)-7)], m_a_par_y[(length(m_a_par_x)-7)], col="black", pch=4, cex=2, lwd=3)



l_wave3 <- paste("wave 3: from ",wave_3[1]," to ",wave_3[length(wave_3)],"",sep="")
l_wave4 <- paste("wave 4: from ",wave_4[1]," to ",wave_4[length(wave_4)],"",sep="")
l_wave5 <- paste("wave 5: from ",wave_5[1]," to ",wave_5[length(wave_5)],"",sep="")
l_wave6 <- paste("wave 6: from ",wave_6[1]," to ...",sep="")

legend(80, 400, legend=c(l_wave3,l_wave4, l_wave5, l_wave6),
       col=c("red","blue","green","black"), lwd=2, cex=1)

