#### 1. Import and set up data ####

setwd("INTRODUCE DIRECTORY HERE")
Raw <- read.csv(file = "TpThiamine.csv", header = TRUE, sep = ";", dec = ".")

mydata<-Raw
mydata$Ct <- as.numeric(mydata$Ct)
mydata$Eff <- as.numeric(mydata$Eff)
mydata$Primer <- as.factor(mydata$Primer)
mydata$Thiamine <- as.factor(mydata$Thiamine)
mydata$BioRep <- as.factor(mydata$BioRep)
attach(mydata)

#### 2. Plot Efficiencies ####

library(ggplot2)
library(ggthemes)
library(extrafont)
library(extrafontdb)
library(Rttf2pt1)
theme_get <- theme_set(theme_get)
theme_get$text$size <- 7.29
theme_get$text$family <- "Calibri"
theme_get$axis.text$size <- 7.29
theme_get$axis.title <- element_blank()
theme_get$legend.text$size <- 7.29
theme_get$axis.title.x$face <- "plain"
theme_get$axis.title.y$face <- "plain"
theme_get$legend.title$face <- "plain"
theme_get$panel.background <- element_blank()
theme_get$axis.line.x <- element_line()
theme_get$axis.line.y <- element_line()
theme_set(theme_get)
cbPalette <- c("#56A4DA", "#CF2628")
plot <- ggplot(mydata, aes(x = Thiamine, y = Eff), colour = Primer)+
  geom_point()+
  facet_grid(.~Primer)
plot
plot + ylim(1.45,1.9)

#### 3. Clean up data points with low efficiencies and false positives ####

mydata <- mydata[-which(mydata$Eff<1.525),]
mydata <- mydata[-which(mydata$BioRep %in% c("NoTemp","RT-")),]
mydata <- droplevels.data.frame(mydata)

#### 4. Visualise Efficiencies again, and make sure there is negligible ####
####    errors within each primer pair ####

plot <- ggplot(mydata, aes(x = Thiamine, y = Eff), colour = Primer)+
  geom_boxplot()+
  facet_grid(.~Primer)
plot
plot + ylim(1,2)

#### 5. Plot Ct values to visualise changes ####

plot1 <- ggplot(mydata, aes(x = Thiamine, y = Ct), colour = Primer)+
  geom_boxplot()+
  facet_grid(.~Primer)
plot1



#### 6. Average technical duplicates ####

library(plyr)
Tdata <- ddply(mydata, c("Primer","Thiamine","BioRep"), summarise,
               Eff = mean(Eff),
               Ct = mean(Ct))
mydata<-Tdata

#### 7. Calculate RQ ####

mydata$RQ <- mydata$Eff^-mydata$Ct
mydata

#### 8. Geometric Mean of HKG RQ ####

Gene <- NULL
for (i in 1:as.numeric(nrow(mydata))){
  Gene <- c(Gene,if(mydata$Primer[i] %in% c("Actin","EF1a","rbcs")) "HKG" else
    "Test")
}

mydata$Gene <- Gene
mydata
library(plyr)
RQdata <- ddply(mydata, c("Gene","BioRep"), summarise,
                RQ = exp(mean(log(RQ))))
RQdata
detach(package:plyr)
rm(Gene)

#### 9. Calculate relative gene expression ####

mydata <- mydata[order(mydata$BioRep),]
mydata  

library(dplyr)
nBioRep<- mydata %>% 
  group_by(BioRep) %>%
  summarise(no_rows = length(BioRep))

GeoMean_RQ_HKG<-NULL
for (i in 1:nlevels(mydata$BioRep)){
  GeoMean_RQ_HKG <- c(GeoMean_RQ_HKG,rep(RQdata[i,3], nBioRep[i,2]))
}
mydata$GeoMean_RQ_HKG <- GeoMean_RQ_HKG
mydata

mydata$RelExp <- mydata$RQ/mydata$GeoMean_RQ_HKG
mydata

rm(nBioRep,GeoMean_RQ_HKG,RQdata)

#### 10. Calculate DeltaRelExp ####

library(plyr)
cdata <- ddply(mydata, c("Thiamine","Primer"), summarise,
               RelExp = mean(RelExp))
cdata
detach(package:plyr)
Cal <- cdata[1:nlevels(mydata$Primer),3]

mydata <- mydata[order(mydata$Primer),]

library(dplyr)
Nota <- mydata %>% 
  group_by(Primer) %>%
  summarise(no_rows = length(Primer))
Nota

Calibrator <- NULL
for(i in 1:nlevels(mydata$Primer)){
  Calibrator <- c(Calibrator,rep(Cal[i],Nota[i,2]))  
}

mydata$Calibrator <- Calibrator

mydata$DeltaRelExp <- mydata$RelExp/mydata$Calibrator
mydata

rm(Cal,Calibrator)

#### 11. T-tests ####

ttest_input <- mydata %>% 
  select(RelExp, Thiamine, Primer) %>% 
  group_by(Primer,Thiamine) %>%
  summarise(RelExp = list(RelExp))

ttest_input$Primer<-droplevels(ttest_input$Primer)

ttry <- NULL
for(i in 1:nlevels(ttest_input$Primer)){
  ttry <- append(ttry,try(t.test(unlist(ttest_input[(i*2)-1,3]),unlist(ttest_input[i*2,3]))$p.value))
}

Primer <- levels(ttest_input$Primer)
p.value <- as.numeric(ttry)
T.test_Result<-data.frame(Primer,p.value)
print(T.test_Result)

#### 12. Plot results ####

mydata$Primer <- factor(mydata$Primer, levels = c("Actin","EF1a","rbcs","THIC","SSSP","NMT1","THIG"))
levels(mydata$Thiamine) <- c("0","10")
attach(mydata)

mydata.plot <- as.data.frame(rep(1,nrow(mydata)))

mydata.plot$Primer <- as.factor(mydata$Primer)
mydata.plot$Thiamine <- as.factor(mydata$Thiamine)
mydata.plot$DeltaRelExp <- as.numeric(mydata$DeltaRelExp)
mydata.plot$Species <- as.factor(rep("T. pseudonana",nrow(mydata.plot)))
mydata.plot <- mydata.plot[,-1]
mydata.plot <- mydata.plot[-which(mydata.plot$Primer %in% c("Actin","EF1a","rbcs","THIG")),]

plot2 <- ggplot(mydata.plot, aes(x = Thiamine, y = DeltaRelExp, fill = Thiamine))+
  geom_hline(yintercept = 1, color = "grey")+
  geom_boxplot(alpha=0.5,size=0.5)+
  geom_jitter(width = 0.2, alpha = 1, aes(fill = Thiamine, colour = Thiamine))+
  scale_fill_manual(values=cbPalette) +
  facet_grid(.~Primer)
plot2 <- plot2 +  ylim(0,3) + theme(legend.position="none")+
  theme(strip.text.x = element_text(face = "italic")) +
  scale_colour_manual(values = cbPalette) +
  ylab("Relative Expression")
plot2

ggsave("TpThiamine.tiff", plot = plot2, device = "tiff", width = 5.7, height = 5, units = "cm", dpi = "retina")

print(T.test_Result)
