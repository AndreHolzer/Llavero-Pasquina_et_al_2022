#### 1. Import and setup data ####

setwd("INTRODUCE DIRECTORY HERE")
mydata <- read.csv("THIC_KO_Data.csv", sep = ";", dec =",")

attach(mydata)

Time<-NULL
for (i in 6:42){
  Time<-c(Time,rep(mydata[i,3],3))
}
Time<-rep(Time,10)

Mutant<-c(rep("Blank",222),rep("WT",222),rep("KO1 - A3",222),rep("KO2 - E4",222),rep("KO3 - F5",222))

# KO1 = A3 # KO2 = E4 # KO3 = F5 #

Replicate<-rep(c("1","2","3"),10)
Replicate<-rep(Replicate,111/3)

Thiamine<-c(rep("No Thiamine",111),rep("1µM Thiamine",111))
Thiamine<-rep(Thiamine,5)

OD<-NULL
for (t in 0:1){ #Mutant(2)
  for (y in 0:1){ #Thiamine(2)
    for (i in 6:42){ #Time(37)
      for (j in ((y*180)+(5+t*5)):((y*180)+(7+t*5))){ #Replicate(3)
        OD<-c(OD,mydata[i,j])
      }
    }
  }
}

for (t in 0:1){ #Mutant(2)
  for (y in 0:1){ #Thiamine(2)
    for (i in 6:42){ #Time(37)
      for (j in ((y*180)+(40+t*5)):((y*180)+(42+t*5))){ #Replicate(3)
        OD<-c(OD,mydata[i,j])
      }
    }
  }
}

for (y in 0:1){ #Thiamine(2)
    for (i in 6:42){ #Time(37)
      for (j in ((y*180)+(30)):((y*180)+(32))){ #Replicate(3)
        OD<-c(OD,mydata[i,j])
      }
    }
  }
rm(t,y,i,j)

OD<-sub(",",".",OD)
Time<-sub(",",".",Time)

data<-as.data.frame(cbind(Time,Mutant,Replicate,Thiamine,OD))

data$Time<-as.numeric(data$Time)
data$Mutant<-as.factor(data$Mutant)
data$Replicate<-as.factor(data$Replicate)
data$Thiamine<-as.factor(data$Thiamine)
data$OD<-as.numeric(data$OD)
rm(Time,Mutant,Replicate,Thiamine,OD)

#### 2. Discount blanks ####

library(dplyr)
library(plyr)

Blank <- data[which(data$Mutant %in% "Blank") , ]

cdata <- ddply(Blank, c("Thiamine","Time"), summarise,
               mean = mean(OD),
               SD = sd(OD))

Blk <- NULL
for(i in 1:(nrow(cdata))){
  Blk <- c(Blk,rep(cdata[i,3],3))
}
rm(i)

Blk<-rep(c(Blk[112:222],Blk[1:111]),5)

data$OD <- data$OD - Blk

data <- data[ which( ! data$Mutant %in% "Blank") , ]

data <- droplevels.data.frame(data)

rm(Blank,Blk,Bk,cdata)

#### 3. Average and SD all Treatments and Time ####

Treatment <- ddply(data, c("Mutant","Thiamine", "Time"), summarise,
                mean = mean(OD),
                sd = sd(OD))

Treatment <- Treatment[which(! Treatment$Mutant %in% "KO2 - E4"),]

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
cbPalette <- c("#56A4DA", "#CF2628")

Treatment$Mutant <- factor(Treatment$Mutant, levels = c("WT", "KO1 - A3", "KO3 - F5"))
Treatment$Thiamine <- factor(Treatment$Thiamine, levels = c("No Thiamine", "1µM Thiamine"))
Treatment$Time <- Treatment$Time/24

Plot <- ggplot(Treatment, aes(x = Time, y = mean, group= Thiamine, colour=Thiamine))+
  geom_line(size=0.1) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) +
  geom_point(size=1, fill="white")+
  facet_grid(.~Mutant)+
  theme(legend.position = "none")
Plot<- Plot+
  scale_colour_manual(values = cbPalette) + scale_x_continuous(breaks = c(0,10,20,30)) + scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8))+
  theme(axis.title = element_blank(),strip.text.x = element_blank(), axis.text.x = element_blank())
Plot

ggsave("THIC_KO.tiff", plot = Plot, device = "tiff", width = 7.8, height = 3, units = "cm", dpi = "retina")
