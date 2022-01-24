#### 1. Import data ####

setwd("INTRODUCE DIRECTORY HERE")
mydata <- na.omit(read.csv("Fig_S3_data.csv", sep = ",", dec =","))

attach(mydata)

mydata$Time<-as.numeric(mydata$Time)
mydata$Mutant<-as.factor(mydata$Mutant)
mydata$Replicate<-as.factor(mydata$Replicate)
mydata$Thiamine<-as.factor(mydata$Thiamine)
mydata$OD<-as.numeric(mydata$OD)
rm(Time,Mutant,Replicate,Thiamine,OD)

#### 2. Discount blanks ####

library(dplyr)
library(plyr)

Blank <- mydata[which(mydata$Mutant %in% "Blank") , ]

cdata <- ddply(Blank, c("Thiamine","Time"), summarise,
               mean = mean(OD),
               SD = sd(OD))

Blk <- c(rep(cdata[1:18,3],3),rep(cdata[19:36,3],3))
Blk <- c(rep(Blk,3),cdata[1:18,3],cdata[19:36,3])

mydata$OD <- mydata$OD - Blk

mydata <- mydata[ which( ! mydata$Mutant %in% "Blank") , ]

mydata <- droplevels.mydata.frame(mydata)

rm(Blank,Blk,Bk,cdata)

#### 3. Average and SD all Treatments and Time ####

Treatment <- ddply(mydata, c("Mutant","Thiamine", "Time"), summarise,
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
cbPalette <- c("#56A4DA", "#CF2628")

Treatment$Mutant <- factor(Treatment$Mutant, levels = c("WT", "G4", "F3"))
levels(Treatment$Thiamine) <- c("No Thiamine", "1µM Thiamine")
Treatment$Thiamine <- factor(Treatment$Thiamine, levels = c("No Thiamine", "1µM Thiamine"))
Treatment$Time <- Treatment$Time/24

Plot <- ggplot(Treatment, aes(x = Time, y = mean, group= Thiamine, colour=Thiamine))+
  geom_line(size=0.1) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) +
  geom_point(size=1, fill="white")+
  facet_grid(.~Mutant)+
  theme(legend.position = "none")
Plot<- Plot+
  scale_colour_manual(values = cbPalette) + scale_x_continuous(breaks = c(0,5,10,15)) + scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8))+
  theme(axis.title = element_blank(),strip.text.x = element_blank(), axis.text.x = element_blank())
Plot

ggsave("Fig_S3_growthcurve.tiff", plot = Plot, device = "tiff", width = 7.8, height = 3, units = "cm", dpi = "retina")
