#### 1. Enter data ####

setwd("INTRODUCE DIRECTORY HERE")
Raw <- read.csv(file = "Fig3_Data.csv", header = TRUE, sep = ";", dec = ",")

mydata <- Raw
attach(mydata)
mydata$Pyri <- as.factor(Pyri)
mydata$Thi <- as.factor(Thi)
mydata$Time <- as.numeric(Time)
mydata$Blank <- as.factor(Blank)
mydata$Species <- as.factor(Species)
mydata$OD <- as.numeric(mydata$OD)
mydata <- as.data.frame(mydata)

#### 2. Discount blanks ####

library(plyr)
cdata <- ddply(mydata, c("Species","Time","Blank"), summarise,
               mean = mean(OD),
               SD = sd(OD))
Blank <- cdata[which(cdata$Blank == "Blank") , ]

Blk <- NULL
for(i in 1:nrow(Blank)){
  Blk<- c(Blk, rep(Blank[i,4],12))
}
rm(i)

mydata <- mydata[ which( ! mydata$Blank %in% "Blank") , ]
mydata$OD <- mydata$OD - Blk

rm(Blank,Blk.Cr,Blk.Pt,Blk,cdata)

#### 3. Average and SD all Treatments, Species and Time ####

Treatment <- ddply(mydata, c("Thi","Pyri","Species", "Time"), summarise,
                mean = mean(OD),
                sd = sd(OD))

#### 4. Growth curve graph ####

levels(Treatment$Thi) <- c("No Thiamine", "10µM Thiamine")
levels(Treatment$Pyri) <- c("No Pyrithiamine", "10µM Pyrithiamine")

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
cbPalette <- c("#56A4DA", "#CF2628")

Plot <- ggplot(Treatment[which(Treatment$Species %in% "C. reinhardtii"),], aes(x = Time, y = mean, group= Thi, colour=Thi))+
  geom_line(size=0.1) +
  scale_y_continuous()+
  scale_x_continuous()+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) +
  geom_point(size=1, fill="white")+
  xlab(expression("Time (h)"))+
  ylab(expression('OD'[730][nm]))+
  facet_grid(.~Pyri)+
  theme(legend.position="top", strip.text = element_blank(),strip.background = element_blank())
Plot<- Plot+
  scale_colour_manual(values = cbPalette)
Plot

ggsave("PyriCr.tiff", plot = Plot, device = "tiff", width = 8.2, height = 4, units = "cm", dpi = "retina")

Plot <- ggplot(Treatment[which(Treatment$Species %in% "P. tricornutum"),], aes(x = Time, y = mean, group= Thi, colour=Thi))+
  geom_line(size=0.1) +
  scale_y_continuous()+
  scale_x_continuous()+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) +
  geom_point(size=1, fill="white")+
  xlab(expression("Time (h)"))+
  ylab(expression('OD'[730][nm]))+
  facet_grid(.~Pyri)+
  theme(legend.position="top", strip.text = element_blank(),strip.background = element_blank())
Plot<- Plot+
  scale_colour_manual(values = cbPalette)
Plot

ggsave("PyriPt.tiff", plot = Plot, device = "tiff", width = 8.2, height = 4, units = "cm", dpi = "retina")

Plot <- ggplot(Treatment[which(Treatment$Species %in% "T. pseudonana"),], aes(x = Time, y = mean, group= Thi, colour=Thi))+
  geom_line(size=0.1) +
  scale_y_continuous()+
  scale_x_continuous()+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) +
  geom_point(size=1, fill="white")+
  xlab(expression("Time (h)"))+
  ylab(expression('OD'[730][nm]))+
  facet_grid(.~Pyri)+
  theme(legend.position="top", strip.text = element_blank(),strip.background = element_blank())
Plot<- Plot+
  scale_colour_manual(values = cbPalette)
Plot

ggsave("PyriTp.tiff", plot = Plot, device = "tiff", width = 8.2, height = 4, units = "cm", dpi = "retina")
