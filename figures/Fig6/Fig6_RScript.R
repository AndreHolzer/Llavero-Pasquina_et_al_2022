setwd("INTRODUCE DIRECTORY HERE")
Raw <- read.csv(file = "Fig6_Data.csv", header = TRUE, sep = ";", dec = ",")

mydata<-Raw
mydata$Construct <- as.factor(mydata$Construct)
mydata$Strain <- as.factor(mydata$Strain)

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
cbPalette <- c("#56A4DA", "#CF2628", "#8bc341")

mydata$Construct <- relevel(mydata$Construct, c("pMLP2065"))
mydata$Construct <- relevel(mydata$Construct, c("WT"))


plot <- ggplot(mydata, aes(x = Construct, y = Thi.pmol.FW, fill = Construct))+
  geom_boxplot(alpha=0.5,size=0.5 )+
  geom_jitter(width = 0.2, alpha = 1, aes(colour=Construct))+
  scale_fill_manual(values=cbPalette) +
  scale_color_manual(values = cbPalette)+
  theme(legend.position="top", strip.text = element_blank(),strip.background = element_blank())
plot <- plot+theme(legend.position="none")+
  ylab("Intracellular Thiamine (pmol/mg FW)") +
  scale_colour_manual(values=cbPalette)+
  ylim(0,0.04)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
plot

ggsave("Fig6Thi.tiff", plot = plot, device = "tiff", width = 4.5, height = 4.5, units = "cm", dpi = "retina")

plot1 <- ggplot(mydata, aes(x = Construct, y = TPPpmol.FW, fill = Construct))+
  geom_boxplot(alpha=0.5,size=0.5 )+
  geom_jitter(width = 0.2, alpha = 1, aes(colour=Construct))+
  scale_fill_manual(values=cbPalette) +
  scale_color_manual(values = cbPalette)+
  theme(legend.position="top", strip.text = element_blank(),strip.background = element_blank())
plot1 <- plot1 + theme(legend.position="none")+
  ylab("Intracellular TPP (pmol/mg FW)") +
  theme(axis.title.x=element_blank(),
  axis.text.x=element_blank(),
  axis.ticks.x=element_blank())+
  scale_colour_manual(values=cbPalette)+
  ylim(0,12)
plot1

ggsave("Fig6TPP.tiff", plot = plot1, device = "tiff", width = 4.5, height = 4.5, units = "cm", dpi = "retina")
