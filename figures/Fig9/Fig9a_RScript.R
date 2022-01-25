#### 1. Enter data ####

setwd("INTRODUCE DIRECTORY HERE")
mydata <- read.csv("Fig9a_Data.csv", sep = ";", dec =",")

attach(mydata)

Time<-NULL
for (i in 6:42){
  Time<-c(Time,rep(mydata[i,3],3))
}
Time<-rep(Time,4)

#Mutant<-c(rep("Blank",222),rep("WT",222),rep("KO1 - A3",222),rep("KO2 - E4",222),rep("KO3 - F5",222))

# KO1 = A3 # KO2 = E4 # KO3 = F5 (No genotyping)#

Replicate<-rep(c("1","2","3"),4)
Replicate<-rep(Replicate,111/3)

Thiamine<-c(rep("Blank",111),rep("No Thiamine",111),rep("1然 HMP - Day6",111),rep("1然 Thiamine - Day6",111))

OD<-NULL
for (i in 6:42){ #Time(37)
  for (j in 5:7){ #Replicate(3)
    OD<-c(OD,mydata[i,j])
  }
}
for (i in 6:42){ #Time(37)
  for (j in 40:42){ #Replicate(3)
    OD<-c(OD,mydata[i,j])
  }
}
for (i in 6:42){ #Time(37)
  for (j in 100:102){ #Replicate(3)
    OD<-c(OD,mydata[i,j])
  }
}
for (i in 6:42){ #Time(37)
  for (j in 160:162){ #Replicate(3)
    OD<-c(OD,mydata[i,j])
  }
}
rm(i,j)

OD<-sub(",",".",OD)
Time<-sub(",",".",Time)

data<-as.data.frame(cbind(Time,Replicate,Thiamine,OD))

data$Time<-as.numeric(data$Time)
data$Replicate<-as.factor(data$Replicate)
data$Thiamine<-as.factor(data$Thiamine)
data$OD<-as.numeric(data$OD)
rm(Time,Mutant,Replicate,Thiamine,OD)

#### 2. Discount blanks ####

library(dplyr)

Blank <- data[which(data$Thiamine %in% "Blank") , ]

cdata <- ddply(Blank, c("Time"), summarise,
               mean = mean(OD),
               SD = sd(OD))

Blk <- NULL
for(i in 1:(nrow(cdata))){
  Blk <- c(Blk,rep(cdata[i,2],3))
}
rm(i)

Blk<- rep(Blk,4)

data$OD <- data$OD - Blk

data <- data[ which( ! data$Thiamine %in% "Blank") , ]

data <- droplevels.data.frame(data)

rm(Blank,Blk,Bk,cdata)

#### 3. Average and SD all Treatments and Time ####

Treatment <- ddply(data, c("Thiamine", "Time"), summarise,
                mean = mean(OD),
                sd = sd(OD))

#### 4. Plot growth curve ####

library(ggplot2)
library(ggthemes)
library(extrafont)
library(extrafontdb)
library(Rttf2pt1)
theme_get <- theme_set(theme_get)
theme_get$text$size <- 7.29
theme_get$text$family <- "Calibri"
theme_get$axis.text$size <- 7.29
theme_get$legend.text$size <- 7.29
theme_get$axis.title.x$face <- "plain"
theme_get$axis.title.y$face <- "plain"
theme_get$legend.title$face <- "plain"
theme_get$panel.background <- element_blank()
theme_get$axis.line.x <- element_line()
theme_get$axis.line.y <- element_line()
theme_set(theme_get)
cbPalette <- c("#56A4DA", "#CF2628", "#8BC341")

Treatment$Thiamine <- factor(Treatment$Thiamine, levels = c("No Thiamine", "1然 Thiamine - Day6", "1然 HMP - Day6"))
Treatment$Time <- Treatment$Time/24

Plot <- ggplot(Treatment, aes(x = Time, y = mean, group= Thiamine, colour=Thiamine))+
  geom_line(size=0.1) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) +
  geom_point(size=1, fill="white")+
  theme(legend.position = "none")
Plot<- Plot+
  scale_colour_manual(values = cbPalette) + scale_x_continuous(breaks = c(0,7,14,21,28,35)) + 
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8))+
  theme(axis.title = element_blank(), axis.text.x = element_blank())
Plot

ggsave("Fig9a.tiff", plot = Plot, device = "tiff", width = 7.8, height = 3.925, units = "cm", dpi = "retina")
