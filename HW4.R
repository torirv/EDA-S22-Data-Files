library(readr)
library(wesanderson)
library(tidyverse)
library(ggplot2)
library(ggpubr)


x <- "https://raw.githubusercontent.com/torirv/EDA-S22-Data-Files/main/SKU%20Master.csv"
SKUMaster <- read.csv(x)
theme_set(theme_pubr())
#Load in data set 


#Filter according to UOM Cube 
SKUMaster$Uom <- as.factor(SKUMaster$Uom)
SKUMaster$Flow <- as.factor(SKUMaster$Flow)
SKUMaster <- SKUMaster[SKUMaster$UomCube>0,]
SKUMaster <- SKUMaster[SKUMaster$UomCube<2,]
#Filter according to UOM Weight 
SKUMaster<-SKUMaster[SKUMaster$UomWeight>0,]
SKUMaster<-SKUMaster[SKUMaster$UomWeight<50,]
#Filter by UOM & drop all NAs 
SKUMaster<-SKUMaster[SKUMaster$Uom %in% c("CA","EA","PL","LB"),]
SKUMaster<-na.omit(SKUMaster)
SKUMaster<-droplevels(SKUMaster)


par(bg = "gray81")
plot(NULL)
table(SKUMaster$Uom)
rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],
       col = "#f7f7f7") 
par(new = TRUE)
a <- barplot(table(SKUMaster$Uom),border= "Black",
             col=wes_palette(n=4, "GrandBudapest2"),xlab=("Units of Measure"), 
             ylab=("Frequency"),main="Frequency of UOM Types",font.lab=2,cex.lab=1,
             ylim = c(0,3000))


#Create a side-by-side boxplot of Cubic Feet by UoM by the types of Flow.
plot(NULL)
rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],
     col = "#f7f7f7")
par(new = TRUE)
c <- boxplot(SKUMaster$UomCube~SKUMaster$Flow,col=wes_palette(n=3, "FantasticFox1"),
            xlab=("Flow Type"), ylab=("UOM Cubic Feet"),font.lab=2,cex.lab=1.2,
            main="Cubic Feet of UOM by Flow Type")

#---------------------------2nd Data Set----------------------------------------
#Only use data points where Flow = Direct to Store (DD) 
SKUMasterDD <-SKUMaster[SKUMaster$Flow %in% c("DD"),] 
SKUMasterDD <-droplevels(SKUMasterDD)
plot(NULL)
rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],
     col = "#f7f7f7") # Color
par(new = TRUE)


#Create a boxplot on the Weight per UOM.  
d <- boxplot(SKUMasterDD$UomWeight,
             main="Weight per UOM for Direct to Store Flow",
             cex.main=1.5,
             col=wes_palette(n=1,"Zissou1")) 









