#### 1. Import data ####

setwd("C:/Users/Usuario/Desktop/Marcel/Pt Riboswitch Paper/Figures and Raw Data - cleaned for GitHub/figures/Fig5")
Raw <- read.csv(file = "20190214 pMLP2047 Growth.csv", header = TRUE, sep = ";", dec = ",")

mydata <- Raw
attach(mydata)
mydata$Time <- as.numeric(Time)
mydata <- as.data.frame(mydata)
attach(mydata)
#### 2. Discount blanks ####

library(plyr)

Blank <- mydata[which(mydata$Blank == "Blank") , ]

cdata <- ddply(Blank, c("Time","Construct","Thiamine", "Zeocin"), summarise,
               mean = mean(OD),
               SD = sd(OD))

rm(Blank)

Blk <- NULL
for(i in 1:24){
  Blk <- c(Blk,replicate(48,cdata[(i*4),5]),replicate(48,cdata[(i*4-1),5]),replicate(48,cdata[(i*4-2),5]),replicate(48,cdata[(i*4-3),5]))
  }
rm(i)

mydata$OD <- mydata$OD - Blk

mydata <- mydata[ which( ! mydata$Blank %in% "Blank") , ]

rm(Blank,Blk,Bk,cdata)

#### 4. Average and SD all Treatments and Time ####

Treatment <- ddply(mydata, c("Thiamine","Zeocin", "Time", "Construct"), summarise,
                mean = mean(OD),
                sd = sd(OD))
#### 6. Represent growth curve ####

levels(Treatment$Thiamine) <- c("10µM Thiamine", "No Thiamine")
Treatment$Thiamine <- relevel(Treatment$Thiamine, "No Thiamine")
levels(Treatment$Zeocin) <- c("75mg/L Zeocin", "No Zeocin")

Treatment$Construct = factor(Treatment$Construct, levels=c("WT", "pMLP2047", "pMLP2048"))
library(ggplot2)
library(ggthemes)
library(extrafont)
library(extrafontdb)
library(Rttf2pt1)
theme_get <- theme_set(theme_get)
theme_get$text$size <- 7.29
theme_get$text$family <- "Calibri"
theme_get$axis.text$size <- 7.29
theme_get$axis.text$size <- 7.29
theme_get$legend.text$size <- 7.29
theme_get$axis.title.x$face <- "plain"
theme_get$axis.title.y$face <- "plain"
theme_get$legend.title$face <- "plain"
theme_get$panel.background <- element_blank()
theme_get$axis.line.x <- element_line()
theme_get$axis.line.y <- element_line()
theme_set(theme_get)
cbPalette <- c("#CF2628","#56A4DA")

plot <- ggplot(Treatment, aes(x = Time, y = mean, group= Zeocin, colour=Zeocin))+
  geom_line(size=0.1) +
  scale_y_continuous()+
  scale_x_continuous()+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) +
  geom_point(size=1, fill="white")+
  xlab(expression("Time (h)"))+
  ylab(expression('OD'[730][nm]))+
  facet_grid(Construct ~ Thiamine)+
  theme(legend.position="top",strip.background = element_blank(),strip.text = element_blank())
plot<- plot+
  scale_colour_manual(values = cbPalette)
plot

ggsave("Fig5.tiff", plot = plot, device = "tiff", width = 8.2, height = 10, units = "cm", dpi = "retina")
