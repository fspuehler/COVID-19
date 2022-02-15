
library(lubridate)
##############################
###       parameter        ###
##############################

## category (options: "Hosp" or "Death") 
category <- "Hosp"

## frame (how many weeks?)
frame <- 20

## accumulated (how many weeks?)
accumulated <- 3

## latest FOPH data
## download latest data (CSV!) from: https://www.covid19.admin.ch/ 
## path to folder with extracted files (starts from home) 

path <- "~/documents/data/"

path_export <- "~/documents/"

##############################
###       parameter  2     ###
##############################
age <- c("30 - 39", "40 - 49", "50 - 59", "60 - 69", "70 - 79", "80+")
color <- c("brown","pink","green","skyblue","blue","black")
status <- c("not_vaccinated","fully_vaccinated_no_booster","fully_vaccinated_first_booster")

##############################
###       read data        ###
##############################

filename <- paste("",path,"COVID19",category,"_vaccpersons_AKL10_w.csv",sep="")

data <- read.table(filename, sep=",", header=TRUE)

weeks <- seq.Date(Sys.Date()-(frame)*7, Sys.Date()-7, 7)

h <- 1
for(h in 1:length(age)) {
  rr1 <- NULL
  rr2 <- NULL
for(i in 1:(frame-accumulated+1)) {
  inz1 <- 0
  inz2 <- 0
  inz3 <- 0
  j <- 1
  for(j in 1:accumulated) {
  inz1 <- inz1+subset(data, date ==  year(weeks[(i+j-1)])*100+week(weeks[(i+j-1)]) & vaccination_status == status[1] & geoRegion == "CHFL" & altersklasse_covid19 == age[h] )[1,7]
  inz2 <- inz2+subset(data, date ==  year(weeks[(i+j-1)])*100+week(weeks[(i+j-1)]) & vaccination_status == status[2] & geoRegion == "CHFL" & altersklasse_covid19 == age[h] )[1,7]
  inz3 <- inz3+subset(data, date ==  year(weeks[(i+j-1)])*100+week(weeks[(i+j-1)]) & vaccination_status == status[3] & geoRegion == "CHFL" & altersklasse_covid19 == age[h] )[1,7]
  j <- j+1
  }
  rr1 <- rbind(rr1, inz2/inz1)
  rr2 <- rbind(rr2, inz3/inz1)
  
  i <- i+1
}
  filename_chart <- paste("",path_export,"VE Covid ",category," ",age[h],".png",sep="")
    png(file=filename_chart ,
      width=1000, height=800)
  
  subtitle_fs <- paste("",accumulated," week(s) accumulated, data: FOPH - ",Sys.Date(),", Plot v0.1 Fabian Spühler (Twitter: @fabianspuehler)",sep="")
  main_title <- paste("relative risk reduction age ",age[h]," COVID ",category," by vaccination status (CH&FL)",sep="")
  labels_week <- NULL
  k <- 1
  for(k in 1:(frame-accumulated+1)) {
    label <- paste("",week(weeks[k]),"(",year(weeks[k])-2000,")-",week(weeks[k+accumulated-1]),"(",year(weeks[k+accumulated-1])-2000,")",sep="")
  labels_week <- rbind(labels_week, label)
  k <- k+1
  }
 
  plot(1:(frame-accumulated+1), (1-rr2)*100,  xaxt = "n", yaxt = "n", sub=subtitle_fs, ylab="RRR %", xlab="", main = main_title,
       ylim=c(0,100), pch="+", lwd = 1, col = "white", cex.main= 2, cex.lab=1.2)
  abline(h=100, col="lightgray", lty="dotted", lwd = 2)
  abline(h=95, col="lightgray", lty="dotted", lwd = 2)
  abline(h=90, col="lightgray", lty="dotted", lwd = 2)
  abline(h=85, col="lightgray", lty="dotted", lwd = 2)
  abline(h=80, col="lightgray", lty="dotted", lwd = 2)
  
  axis(side = 2,
       ## Rotate labels perpendicular to y-axis.
       las = 2,
       ## Adjust y-axis label positions.
      mgp = c(3, 0.75, 0),
     cex.axis=2
     )
  
  text(x=1:(frame-accumulated+1),
       y = par("usr")[3] - 4,
       
       labels=labels_week,
       xpd = NA,
       adj = 0.9,
       srt = 25)
  
  blank_labels <- c("","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","")
  
  axis(1, at=1:(frame-accumulated+1),
   labels = blank_labels[1:(frame-accumulated+1)]  ,
   cex.axis=1.2)
  
  lines(1:(frame-accumulated+1), (1-rr2)*100, col = color[h], lty=1, lwd = 4, )
  lines(1:(frame-accumulated+1), (1-rr1)*100, col = color[h], lty=3, lwd = 4, )

  legend(1, 20, legend=c("fully vaccinated first booster","fully vaccinated no booster"),
         col=c(color[h],color[h]), lty=c(1,3) , lwd=4, cex=2)
  dev.off()
  h <- h+1
}