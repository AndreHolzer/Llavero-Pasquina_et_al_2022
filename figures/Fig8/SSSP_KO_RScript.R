#### 1. Enter data ####

setwd("INTRODUCE DIRECTORY HERE")
mydata <- read.csv("20200318 SSSP KO Thiamine Uptake.csv", sep = ";", dec =",")

mydata <- mydata[,names(mydata) %in% c("Num","Clone","Thiamine","BioRep","TechRep","Wet_Weight","Thiamine.1","TPP","Thiamine_mg","TPP_mg")]

mydata <- mydata[-which(mydata$Clone %in% c("B3","G9")),]

mydata$TechRep <- as.factor(mydata$TechRep)

mydata <- droplevels.data.frame(mydata)
mydata$Thiamine <- as.factor(mydata$Thiamine)
levels(mydata$Thiamine) <- c("10 µM Thiamine", "No Thiamine")
mydata$Thiamine <- relevel(mydata$Thiamine, "No Thiamine")
mydata$Clone <- factor(mydata$Clone, levels = c("WT", "D1", "C3"))

#### 2. Plot uptake data ####

library(ggplot2)
library(ggthemes)
library(extrafont)
library(extrafontdb)
library(Rttf2pt1)
theme_get <- theme_set(theme_get)
theme_get$text$size <- 7.29
theme_get$text$family <- "Calibri"
theme_get$axis.text$size <- 7.29
theme_get$axis.title$size <- 7.29
theme_get$legend.text$size <- 7.29
theme_get$axis.title.x$face <- "plain"
theme_get$axis.title.y$face <- "plain"
theme_get$legend.title$face <- "plain"
theme_get$panel.background <- element_blank()
theme_get$axis.line.x <- element_line()
theme_get$axis.line.y <- element_line()
theme_set(theme_get)
cbPalette <- c("#56A4DA", "#CF2628")

levels(mydata$Thiamine) <- list("0" = "No Thiamine", "10" = "10 µM Thiamine")

plot <- ggplot(mydata, aes(x = Thiamine, y = Thiamine_mg))+
  geom_boxplot(alpha=0.5,size=0.5)+
  geom_jitter(width = 0.2, alpha = 1, size = 1)+
  scale_fill_manual(values=cbPalette)+
  scale_colour_manual(values=cbPalette)+
  facet_grid(.~Clone)+
  theme(legend.position = "none", axis.title = element_blank(), strip.text.x = element_blank())+
  ylim(0,1.5)
plot

ggsave("SSSP_KO.tiff", plot = plot, device = "tiff", width = 7.8, height = 3.5, units = "cm", dpi = 300)

#### 3. ANOVA + Tukey Test ####

library(multcompView)

mydata$Treatment<-with(mydata, interaction(Clone,  Thiamine))

model=lm(mydata$Thiamine_mg ~ mydata$Treatment)
ANOVA=aov(model)
TUKEY <- TukeyHSD(x=ANOVA, "mydata$Treatment", conf.level=0.95)
ANOVA
TUKEY

# I need to group the treatments that are not different each other together.
generate_label_df <- function(TUKEY, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- TUKEY[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labels$treatment=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$treatment) , ]
  return(Tukey.labels)
}
# Apply the function on my dataset
LABELS <- generate_label_df(TUKEY , "mydata$Treatment")
LABELS
