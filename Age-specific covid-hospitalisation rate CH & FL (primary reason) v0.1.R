rm(list=ls())

##############################
###       parameter        ###
##############################

## x-data 
category_x <- "Hosp"

## y-data  
category_y <- "Cases"


## frame size rate in weeks
frame <- 20

## time from case to admission in weeks
shift <- 1

## accumulated (how many weeks?)
accumulated <- 3

## how old is data in -days
data_age <- -0

first_week_day <- Sys.Date()-wday(Sys.Date(), week_start = TRUE)+1


## latest FOPH data
## download latest data (CSV!) from: https://www.covid19.admin.ch/ 
## path to folder with extracted files (starts from home) 

path <- "~/documents/data/"



##############################
###       parameter  2     ###
##############################
age <- c("0 - 9","10 - 19","20 - 29","30 - 39", "40 - 49", "50 - 59", "60 - 69", "70 - 79", "80+")
color <- c("yellow","orange","red","brown","pink","green","skyblue","blue","black")
status <- c("covid","unknown","other")




##############################
###       functions        ###
##############################

moving_average <- function(x, n = 7) {             
  stats::filter(x, rep(1 / n, n), sides = 2)
}

##############################
###       read data        ###
##############################

filename_x <- paste("",path,"COVID19",category_x,"_reason_AKL10_w.csv",sep="")
filename_y <- paste("",path,"COVID19",category_y,"_geoRegion_AKL10_w.csv",sep="")

data_adm <- read.table(filename_x, sep=",", header=TRUE)
data_cas <- read.table(filename_y, sep=",", header=TRUE)

weeks <- seq.Date(first_week_day-(frame)*7, Sys.Date()-7, 7)
weeks2 <- seq.Date(first_week_day-(frame+shift)*7, Sys.Date()-(1+shift)*7, 7)


admi_covid <- NULL
admi_unknown <- NULL
admi_other <- NULL

i <- 1
for(i in 1:length(age)) {
  h <- 1
  adm_covid <- NULL
  adm_unknown <- NULL
  adm_other <- NULL
  for(h in 1:(length(weeks)-accumulated+1)) {
    j <- 1
    adm_covid_1 <- 0
    adm_unknown_1 <- 0
    adm_other_1 <- 0
    for(j in 1:accumulated){
      adm_covid_1 <-    adm_covid_1+    subset(data_adm, date ==  year(weeks[(h+j-1)])*100+week(weeks[(h+j-1)]) & primary_hosp_reason == status[1] & geoRegion == "CHFL" & altersklasse_covid19 == age[i] )[1,6]
      adm_unknown_1 <-  adm_unknown_1+  subset(data_adm, date ==  year(weeks[(h+j-1)])*100+week(weeks[(h+j-1)]) & primary_hosp_reason == status[2] & geoRegion == "CHFL" & altersklasse_covid19 == age[i] )[1,6]
      adm_other_1 <-    adm_other_1+    subset(data_adm, date ==  year(weeks[(h+j-1)])*100+week(weeks[(h+j-1)]) & primary_hosp_reason == status[3] & geoRegion == "CHFL" & altersklasse_covid19 == age[i] )[1,6]
      j <- j+1
    }
    adm_covid <- c(adm_covid,adm_covid_1)
    adm_unknown <- c(adm_unknown,adm_unknown_1)
    adm_other <- c(adm_other,adm_other_1)
    h <- h+1
  }
  admi_covid <- rbind(admi_covid, adm_covid)
  admi_unknown <- rbind(admi_unknown, adm_unknown)
  admi_other <- rbind(admi_other, adm_other)
  i <- i+1
}


adm_covid_1 <- adm_covid_1+subset(data_adm, date ==  year(weeks[(h+j-1)])*100+week(weeks[(h+j-1)]) & primary_hosp_reason == status[1] & geoRegion == "CHFL" & altersklasse_covid19 == age[i] )[1,6]


cases_covid <- NULL
i <- 1
for(i in 1:length(age)) {
  h <- 1
  cas_covid <- NULL
  
  for(h in 1:(length(weeks2)-accumulated+1)) {
    j <- 1
    cas_covid_1 <- 0
    
    for(j in 1:accumulated){
      
      cas_covid_1 <- cas_covid_1+subset(data_cas, datum ==  year(weeks2[(h+j-1)])*100+week(weeks2[(h+j-1)]) & geoRegion == "CHFL" & altersklasse_covid19 == age[i] )[1,4]
      
      j <- j+1
    }
    
    cas_covid <- c(cas_covid,cas_covid_1)
    h <- h+1
  }
  
  
  cases_covid <- rbind(cases_covid, cas_covid)
  i <- i+1
}


##############################
###   calc: hosp_rate      ###
##############################


admi_covid_unknown <- admi_covid+admi_unknown

hosp_rate <- admi_covid_unknown/cases_covid*100


##############################
###   plot: hosp_rate      ###
##############################

subtitle <-paste("",accumulated," week(s) accumulated, data: FOPH - ",Sys.Date()+data_age,", Plot v0.1 Fabian Spühler (Twitter: @fabianspuehler)",sep="")
main_title <- paste("Age-specific covid-hospitalisation rate Switzerland & Liechtenstein (w/o other primary reason)",sep="")

### labels as calendar weeks
#labels_week <- NULL
#k <- 1
#for(k in 1:(frame-accumulated+1)) {
#  label <- paste("",week(weeks[k]),"(",year(weeks[k])-2000,")-",week(weeks[k+accumulated-1]),"(",year(weeks[k+accumulated-1])-2000,")",sep="")
#  labels_week <- rbind(labels_week, label)
#  k <- k+1
#}


### labels as days (from .. to ..)
labels_week <- NULL
k <- 1
for(k in 1:(frame-accumulated+1)) {
 from_date <- date(weeks[k])
  to_date <- date(weeks[k+accumulated-1])+6
  label <- paste("",format(from_date, format="%d.%m."),"-",format(to_date, format="%d.%m."),"",sep="")
  labels_week <- rbind(labels_week, label)
  k <- k+1
}


plot(1:(frame-accumulated+1), hosp_rate[9,], sub=subtitle, ylab="Rate log scale %", xlab="", main = main_title, xaxt = "n", yaxt = "n",
     ylim=c(0.018,max(hosp_rate)*1.1), log = "y",
     pch=1, lwd = 1, col = "white")

blank_labels <- c("","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","")

axis(1, at=1:(frame-accumulated+1),
     labels = blank_labels[1:(frame-accumulated+1)])


axis(side = 2,
     las = 2,
     mgp = c(3, 0.75, 0),
     cex.axis=1
)

text(x=1:(frame-accumulated+1),
     y = 0.0108,
     
     labels=labels_week,
     xpd = NA,
     adj = 0.9,
     srt = 30)

poly_x <- c(frame-accumulated-1, frame-accumulated-1,frame-accumulated+2,frame-accumulated+2)
poly_y <- c(0.002,30,30,0.002)

polygon(poly_x, poly_y,
        col = "grey90")
h <- 1
for(h in 1:11) {
  abline(h=2^(h-7), col="darkgrey", lty="dotted", lwd = 1)
  h <- h+1
} 

abline(v=frame-accumulated-1, col="white", lty="dotted", lwd = 1)

abline(v=frame-accumulated-1, col="darkgrey", lty="dotted", lwd = 1)

text(y= 18, x = frame-accumulated+1.4, labels = "Highly incomplete data", col = "black", cex = 1, adj = 1,srt=90)

i <- 1
for(i in 3:length(age)) {
  lines(1:(frame-accumulated+1), hosp_rate[i,], col = color[i], lwd=2)
  i <- i+1
}

legend(1, 0.13, legend= rev(age[3:9]),
       col= rev(color[3:9]), lwd=2, cex=1)

