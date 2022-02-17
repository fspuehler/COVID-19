rm(list=ls())
library(tidyverse)
library(zoo)


##############################
###       parameter        ###
##############################


category <- "Cases"



# data age
data_age <- -0




## latest FOPH data
## download latest data (CSV!) from: https://www.covid19.admin.ch/ 
## path to folder with extracted files (starts from home) 

path <- "~/documents/data/"




##############################
###       functions        ###
##############################

moving_average <- function(x, n = 7) {             
  stats::filter(x, rep(1 / n, n), sides = 2)
}

moving_average5 <- function(x, n = 5) {             
  stats::filter(x, rep(1 / n, n), sides = 2)
}

##############################
###       read data        ###
##############################

filename <- paste("",path,"COVID19",category,"_geoRegion.csv",sep="")

data <- read.table(filename, sep=",", header=TRUE)


days <- seq.Date(Sys.Date()-200, Sys.Date()-3+data_age, 1)







par_x <- NULL

j <- 1
for(j in 1:length(days)) {
  
  par_x <-  c(par_x, subset(data, datum == (days[j]) & geoRegion == "CHFL" )[1,3])
  j <- j+1
}


##############################
###         calc           ###
##############################



m_a_par_x <- moving_average(par_x)

ratio_x <- m_a_par_x[8:length(m_a_par_x)]/m_a_par_x[1:(length(m_a_par_x)-7)]

m_a_ratio_x <- moving_average5(ratio_x)

##############################
###         plot           ###
##############################


subtitle <- paste("data: FOPH - ",Sys.Date()+data_age,", Plot v0.1 Fabian Spühler (Twitter: @fabianspuehler)",sep="")
main_title <- paste("7 day case ratio by specimen date Switzerland & Liechtenstein",sep="")



plot(days[8:length(days)], ratio_x, sub=subtitle, ylab="Case Ratio", xlab="(N_Cases_7d)/(N_Cases_7d(t-7d))", main = main_title,
     ylim=c(0.5,2.0), log = "y", xlim = c(days[28],days[length(days)]),
     pch=1, lwd = 1, col = "red")



lines(days[8:length(days)], m_a_ratio_x, col = "blue", lwd=2)


abline(h=1, col="black", lwd = 2)
abline(h=2^(0.5), col="gray", lty="dotted", lwd = 2)
text(y= 2^(0.5), x = days[50], labels = "Two week doubling", col = "black", cex = 1.8)

abline(h=2^(0.25), col="lightgray", lty="dotted", lwd = 2)
text(y= 2^(0.25), x = days[50], labels = "Four week doubling", col = "black", cex = 1.8)

abline(h=0.5^(0.5), col="lightgray", lty="dotted", lwd = 2)
text(y= 0.5^(0.5), x = days[50], labels = "Two week halving", col = "black", cex = 1.8)

abline(h=0.5^(0.25), col="lightgray", lty="dotted", lwd = 2)
text(y= 0.5^(0.25), x = days[50], labels = "Four week halving", col = "black", cex = 1.8)


