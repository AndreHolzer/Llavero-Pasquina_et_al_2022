#### 1. Import data ####

setwd("INTRODUCE DIRECTORY HERE")
Raw <- read.csv(file = "Fig_S4_data.csv", header = TRUE, sep = ";", dec = ",")

mydata<-Raw
mydata$Thiamine<-as.factor(mydata$Thiamine)

library(plyr)
cdata <- ddply(mydata, c("Thiamine","Organism"), summarise,
               meanThi = mean(Thiamine.1),
               SDThi = sd(Thiamine.1),
               meanTPP = mean(TPP),
               SDTPP = sd(TPP))

cdata$Thiamine <- as.numeric(cdata$Thiamine)
cdata$Thiamine <- c(0,0,10,10,250,250,1000,1000)

#### 2. Plot values ####

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

plot <- ggplot(mydata, aes(x = Thiamine, y = Thiamine.1, fill = Organism))+
  geom_boxplot(alpha=0.5,size=0.5)+
  geom_jitter(width = 0.2, alpha = 1, aes(fill = Organism, colour = Organism))+
  scale_fill_manual(values=cbPalette) +
  scale_y_log10()+
  facet_grid(.~Organism)+
  theme(legend.position="top", strip.text = element_blank(),strip.background = element_blank())
plot <- plot + theme(legend.position="none")+
    ylab("Intracellular Thiamine (pmol/mg FW)") +
    xlab(expression("Thiamine supplementation (µM)"))+
    scale_colour_manual(values=cbPalette)
plot

ggsave("Fig_S4Thi.tiff", plot = plot, device = "tiff", width = 8, height = 5, units = "cm", dpi = "retina")

plot1 <- ggplot(mydata, aes(x = Thiamine, y = TPP, fill = Organism))+
  geom_boxplot(alpha=0.5,size=0.5)+
  geom_jitter(width = 0.2, alpha = 1, aes(fill = Organism, colour = Organism))+
  scale_fill_manual(values=cbPalette) +
  facet_grid(.~Organism)+
  theme(legend.position="top", strip.text = element_blank(),strip.background = element_blank())
plot1 <- plot1 + theme(legend.position="none")+
  ylab("Intracellular TPP (pmol/mg FW)") +
  xlab(expression("Thiamine supplementation (µM)"))+
  scale_colour_manual(values=cbPalette)
plot1

ggsave("Fig_S4TPP.tiff", plot = plot1, device = "tiff", width = 7.63, height = 5, units = "cm", dpi = "retina")
