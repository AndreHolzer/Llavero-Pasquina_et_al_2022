#### 1. Enter data ####

setwd("INTRODUCE DIRECTORY HERE")
mydata <- read.csv("Fig9b_Data.csv", sep = ";", dec =",")

attach(mydata)

Time<-NULL
for (i in 5:31){
  Time<-c(Time,mydata[i,3])
}
Time<-rep(Time,6)

Mutant<-c(rep("Blank",length(Time)),rep("WT",length(Time)*4),rep("KO1 - A3",length(Time)*10))

# KO1 = A3 #

Replicate<-c(rep("1",length(Time)/6),rep("2",length(Time)/6),rep("3",length(Time)/6),rep("4",length(Time)/6),rep("5",length(Time)/6),rep("6",length(Time)/6))

Thiamine<-c(rep("0",length(Time)*2),rep(0.5,length(Time)),rep(10,length(Time)),rep(500,length(Time)),rep(0,length(Time)),rep(0.1,length(Time)),rep(0.25,length(Time)),rep(0.5,length(Time)),rep(1,length(Time)),rep(5,length(Time)),rep(10,length(Time)),rep(50,length(Time)),rep(100,length(Time)),rep(500,length(Time)))

OD<-NULL
for (j in 330:335){
  for (i in 5:31){
    OD<-c(OD,mydata[i,j])
  }
}
for (j in 306:329){
  for (i in 5:31){
    OD<-c(OD,mydata[i,j])
  }
}
for (j in 6:65){
  for (i in 5:31){
    OD<-c(OD,mydata[i,j])
  }
}
rm(i,j)

Time<-rep(Time,length(OD)/length(Time))
Replicate<-rep(Replicate,length(OD)/length(Replicate))
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

Blank <- data[which(data$Mutant %in% "Blank") , ]

cdata <- ddply(Blank, c("Time"), summarise,
               mean = mean(OD),
               SD = sd(OD))

Blk <- NULL
for(i in 1:(nrow(cdata))){
  Blk <- c(Blk,rep(cdata[i,2],nrow(data)/nrow(cdata)))
}
rm(i)

data$OD <- data$OD - Blk

data <- data[ which( ! data$Mutant %in% "Blank") , ]

data <- droplevels.data.frame(data)

rm(Blank,Blk,Bk,cdata)

#### 3. Average and SD all Treatments and Time ####

Treatment <- ddply(data, c("Mutant","Thiamine", "Time"), summarise,
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

Treatment$Mutant <- factor(Treatment$Mutant, levels = c("WT", "KO1 - A3"))
#Treatment$Thiamine <- factor(Treatment$Thiamine, levels = c("No Thiamine", "1µM Thiamine"))
Treatment$Thiamine <- as.numeric(Treatment$Thiamine)
Treatment$Time <- Treatment$Time/24

Plot <- ggplot(Treatment, aes(x = Time, y = mean, group= Thiamine, colour=Thiamine))+
  geom_line(size=0.1) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) +
  geom_point(size=1, fill="white")+
  facet_grid(Mutant~.)+
  theme(legend.position = "none")
Plot<- Plot+
  scale_colour_gradient(low = "#56A4DA", high = "#CF2628") + scale_x_continuous(breaks = c(0,5,10,15,20,25)) + scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8))+
  theme(axis.title = element_blank(),strip.text.y = element_blank(), axis.text.x = element_blank())
Plot

ggsave("Fig9b.tiff", plot = Plot, device = "tiff", width = 7.8, height = 8, units = "cm", dpi = "retina")
