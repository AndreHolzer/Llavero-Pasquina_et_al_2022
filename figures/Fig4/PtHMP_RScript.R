#### 1. Import and set up data ####

Primer <- c("UBC","UBC","H4","H4","UBQ","UBQ","SSSP","SSSP","THIG","THIG","NMT1","NMT1","THIC","THIC","UBC","UBC","H4","H4","UBQ","UBQ","SSSP","SSSP","THIG","THIG","NMT1","NMT1","THIC","THIC","UBC","UBC","H4","H4","UBQ","UBQ","SSSP","SSSP","THIG","THIG","NMT1","NMT1","THIC","THIC","UBC","UBC","H4","H4","UBQ","UBQ","SSSP","SSSP","THIG","THIG","NMT1","NMT1","THIC","THIC","UBC","UBC","H4","H4","UBQ","UBQ","SSSP","SSSP","THIG","THIG","NMT1","NMT1","THIC","THIC","UBC","UBC","H4","H4","UBQ","UBQ","SSSP","SSSP","THIG","THIG","NMT1","NMT1","THIC","THIC","UBC","UBC","H4","H4","UBQ","UBQ","SSSP","SSSP","THIG","THIG","NMT1","NMT1","THIC","THIC","UBC","UBC")
Thiamine <- c("0","0","0","0","0","0","0","0","0","0","0","0","0","0","10","10","10","10","10","10","10","10","10","10","10","10","10","10","10","10","10","10","10","10","10","10","10","10","10","10","10","10","10","10","10","10","10","10","10","10","10","10","10","10","10","10","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","10")
BioRep <- c("0","0","0","0","0","0","0","0","0","0","0","0","0","0","A","A","A","A","A","A","A","A","A","A","A","A","A","A","B","B","B","B","B","B","B","B","B","B","B","B","B","B","C","C","C","C","C","C","C","C","C","C","C","C","C","C","D","D","D","D","D","D","D","D","D","D","D","D","D","D","E","E","E","E","E","E","E","E","E","E","E","E","E","E","F","F","F","F","F","F","F","F","F","F","F","F","F","F","0","0")
Ct <- c(33.7,34,34.7,40.3,23.1,37.6,40.3,39.5,29.1,33.3,39.1,22.4,36.5,37,30.3,30.4,24.2,24.3,28.3,28.1,29.4,29.3,27.8,28.2,39.4,38.7,24.4,24.6,29.8,30,23.7,23.5,29.7,28.4,30.1,29.1,28.6,28.3,38.5,37.8,25,24.7,30.3,30.9,24.1,24.2,28.8,28.5,31,31,28.8,28.7,35.4,34,27.1,26.8,29.8,29.2,23.8,23.7,28.5,27.8,29.3,29.1,26.6,26.6,33.8,33.6,24.4,24.2,30,29.7,23.8,23.8,28.6,28.5,30.5,30.7,26.9,27.3,32.2,34.8,26.7,25.9,29.4,30,24,24.2,28.9,28.9,30.5,29.6,27,27,31.7,30.9,26,25.9,33.2,39.2)
Eff <- c(0,0.8,0,0,0,1.49,0,0.68,0,0,0,0,0,0,1.73,1.65,1.75,1.67,1.67,1.67,1.66,1.69,1.58,1.57,0.08,0.24,1.67,1.67,1.65,1.65,1.68,1.68,1.72,1.7,1.71,1.66,1.6,1.6,1.31,1.31,1.71,1.7,1.66,1.65,1.69,1.72,1.69,1.7,1.62,1.67,1.59,1.6,1.62,1.61,1.64,1.65,1.64,1.66,1.71,1.72,1.68,1.7,1.71,1.65,1.64,1.62,1.61,1.46,1.67,1.69,1.63,1.66,1.69,1.73,1.68,1.73,1.65,1.69,1.6,1.64,1.7,1.49,1.63,1.64,1.69,1.61,1.68,1.66,1.68,1.68,1.67,1.67,1.62,1.6,1.48,1.52,1.6,1.65,1.35,1.44)
Raw <- as.data.frame(cbind(Primer,Thiamine,BioRep,Ct,Eff))
rm(Primer,Thiamine,BioRep)

mydata<-Raw
mydata$Ct <- as.numeric(Ct)
mydata$Eff <- as.numeric(Eff)
mydata$Primer <- as.factor(mydata$Primer)
mydata$Thiamine <- as.factor(mydata$Thiamine)
mydata$BioRep <- as.factor(mydata$BioRep)
attach(mydata)
mydata

rm(Ct,Eff)

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

plot <- ggplot(mydata, aes(x = Thiamine, y = Eff), colour = Primer)+
  geom_point()+
  facet_grid(.~Primer)
plot
plot + ylim(1.2,2)

#### 3. Clean up data points with low efficiencies and false positives ####

attach(mydata)
mydata <- mydata[-which(mydata$Eff < 1.525),]
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
for (i in 1:nrow(mydata)){
  Gene <- c(Gene,if(mydata$Primer[i] %in% c("H4","UBC","UBQ")) "HKG" else
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

mydata$Primer <- factor(mydata$Primer, levels = c("H4","UBC","UBQ","THIC","SSSP","NMT1","THIG"))
levels(mydata$Thiamine) <- c("0","10")
attach(mydata)

mydata.plot <- as.data.frame(rep(1,nrow(mydata)))

mydata.plot$Primer <- as.factor(mydata$Primer)
mydata.plot$Thiamine <- as.factor(mydata$Thiamine)
mydata.plot$DeltaRelExp <- as.numeric(mydata$DeltaRelExp)
mydata.plot$Species <- as.factor(rep("P. tricornutum",nrow(mydata.plot)))
mydata.plot <- mydata.plot[,-1]
mydata.plot <- mydata.plot[-which(mydata.plot$Primer %in% c("H4","UBC","UBQ","THIG")),]

plot2 <- ggplot(mydata.plot, aes(x = Thiamine, y = DeltaRelExp, fill = Thiamine))+
  geom_hline(yintercept = 1, color = "grey")+
  geom_boxplot(alpha=0.5,size=0.5)+
  geom_jitter(width = 0.2, alpha = 1, aes(fill = Thiamine, colour = Thiamine))+
  scale_fill_manual(values=cbPalette) +
  facet_grid(.~Primer)
plot2 <- plot2 +  ylim(0,3) + theme(legend.position="none")+
  theme(strip.text.x = element_text(face = "italic")) +
  scale_colour_manual(values = cbPalette) +
  ylab("Relative Expression")+
  xlab(expression("HMP"))
plot2


setwd("INTRODUCE DIRECTORY HERE")
ggsave("PtHMP.tiff", plot = plot2, device = "tiff", width = 5.7, height = 5, units = "cm", dpi = "retina")


print(T.test_Result)
