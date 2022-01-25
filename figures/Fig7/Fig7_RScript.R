#### 1. Import data ####

setwd("C:/Users/Usuario/Desktop/Marcel/Pt Riboswitch Paper/Figures and Raw Data - cleaned for GitHub/figures/Fig7")
Raw <- read.csv(file = "Fig7_Data.csv", header = TRUE, sep = ";", dec = ",")

mydata <- Raw
attach(mydata)
mydata$Time <- as.numeric(Time)
mydata <- as.data.frame(mydata)
attach(mydata)

#### 2. Discount blanks ####

library(plyr)

Edges <- mydata[which(mydata$Construct == "Edge") , ]
Edges <- Edges[which(!Edges$OD > 0.1) , ]

cdata <- ddply(Edges, c("Time","Thiamine", "Plate"), summarise,
               mean = mean(OD),
               SD = sd(OD))

rm(Edges)

Blk <- NULL
for(i in 1:10){
  for(j in 1:5)
    Blk <- c(Blk,replicate(96,cdata[(i*10)-5+j,4]))
  for(j in 1:5)
    Blk <- c(Blk,replicate(96,cdata[(i*10)-10+j,4]))
  }
rm(i,j,cdata)

mydata$OD <- mydata$OD - Blk
rm(Blk)

#### 3. Remove negative value ####

mydata <- mydata[-2516,]

mydata <- mydata[ which( ! mydata$Construct %in% "Edge") , ]

rm(Blank,Blk,Bk,cdata)

#### 4. Filter data and select final time point ####

mydata.clean <- mydata[which(!mydata$Construct %in% c("1'","2'","3'","4'","5'","6'","7'","8'","9'","10'","11'","12'")),]
pot<-mydata.clean[which((mydata.clean$Time == 219) & (mydata.clean$OD < 0.1)),] 
mydata.clean <- mydata.clean[which(!mydata.clean$Sample %in% pot$Sample),]
mydata.clean <- mydata.clean[which(!mydata.clean$Sample %in% c(214,371)),]
rm(pot)

#### 5. Pick colonies expressing transgene ####

pMLP1225 <- mydata.clean[which(mydata.clean$Construct == "pMLP1225"),]
pMLP1225 <- pMLP1225[which(pMLP1225$Well %in% c("B4","B6","B7","C3","C5")),]

pMLP1226 <- mydata.clean[which(mydata.clean$Construct == "pMLP1226"),]
pMLP1226 <- pMLP1226[which(pMLP1226$Well %in% c("E6","D7","E4","D3","D8")),]

pMLP1227 <- mydata.clean[which(mydata.clean$Construct == "pMLP1227"),]
pMLP1227 <- pMLP1227[which(pMLP1227$Well %in% c("H10","H11","D5","C3","C4")),]

pMLP1228 <- mydata.clean[which(mydata.clean$Construct == "pMLP1228"),]
pMLP1228 <- pMLP1228[which(pMLP1228$Well %in% c("E5","F10","F11","G1","G10")),]

pMLP1229 <- mydata.clean[which(mydata.clean$Construct == "pMLP1229"),]
pMLP1229 <- pMLP1229[which(pMLP1229$Well %in% c("C9","D2","D3","B9","C10")),]

pMLP1230 <- mydata.clean[which(mydata.clean$Construct == "pMLP1230"),]
pMLP1230 <- pMLP1230[which(pMLP1230$Well %in% c("E6","E9","H6","H8","H3")),]

pMLP1231 <- mydata.clean[which(mydata.clean$Construct == "pMLP1231"),]
pMLP1231 <- pMLP1231[which(pMLP1231$Well %in% c("D10","D11","D2","D3","D4")),]

indy5 <- as.data.frame(rbind(pMLP1225,pMLP1226,pMLP1227,pMLP1228,pMLP1229,pMLP1230,pMLP1231))

#### 6. Calculate OD Ratio No Thiamine/Thiamine ####

indy5.thi <- indy5[which(indy5$Thiamine %in% "10µM Thiamine"),]
indy5.nothi <- indy5[which(indy5$Thiamine %in% "No Thiamine"),]

indy5.thi$ratio <- indy5.nothi$OD/indy5.thi$OD
rm(indy5.nothi)

indy5.thi$Time <- as.factor(indy5.thi$Time)

indy5.thi <- indy5.thi[which(indy5.thi$Time %in% "219"),]

#### 8. Represent growth curve ####

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

plot <- ggplot(indy5.thi, aes(x = Construct, y = ratio))+
  geom_boxplot()+
  geom_jitter(width = 0.2, alpha = 1)+
  geom_hline(yintercept=1, col ="grey")+
  xlab(element_blank())+
  ylab(element_blank())+
  scale_y_continuous(breaks = c(0,2,4,6))+
  ylim(0,6)+
  theme(axis.text.x = element_blank())
plot

ggsave("Fig7.tiff", plot = plot, device = "tiff", width = 8, height = 5, units = "cm", dpi = "retina")
