#Creating a boxplot of average number of species per site 
#4/8/2019
#Donovan Spaulding 

#Basic commands
rm(list=ls())
library(ggplot2)
library(dplyr)


#load the dataset 
Data <- read.csv("Camera_Trapping_Ch5_Work/Data.csv")

#Quick overview of the dataset 
str(Data)

#Species richness per camera per forest 
Species_Richness <- Data %>% 
     group_by(ForestName.y, Camera.y, ForestType) %>% 
     summarize(num_species=n_distinct(choice))


#Change the label headings for each of the forests 
Species_Richness$ForestName.y <- as.character(Species_Richness$ForestName.y)

Wrong <- which(Species_Richness$ForestName.y  == "BC")
Wrong
Species_Richness$ForestName.y[Wrong]<-"Beaver Creek"

Wrong <- which(Species_Richness$ForestName.y  == "DEG")
Wrong
Species_Richness$ForestName.y[Wrong]<-"Degrasse"

Wrong <- which(Species_Richness$ForestName.y  == "DON")
Wrong
Species_Richness$ForestName.y[Wrong]<-"Donnerville"

Wrong <- which(Species_Richness$ForestName.y  == "SH")
Wrong
Species_Richness$ForestName.y[Wrong]<-"South Hammond"

Wrong <- which(Species_Richness$ForestName.y  == "WF")
Wrong
Species_Richness$ForestName.y[Wrong]<-"Whiskey Flats"

Wrong <- which(Species_Richness$ForestName.y  == "WHIP")
Wrong
Species_Richness$ForestName.y[Wrong]<-"Whippoorwill Corners"


str(Species_Richness)

#Change the order of the forest names
#Change back to a factor variable 
Species_Richness$ForestName.y <- as.factor(Species_Richness$ForestName.y)

Species_Richness$ForestName.y <- factor(
     Species_Richness$ForestName.y, 
     levels = c("Beaver Creek", "Donnerville", "South Hammond",
                "Degrasse", "Whippoorwill Corners", "Whiskey Flats"))

str(Species_Richness)

#Create the boxplot 
ggplot(Species_Richness, aes(x = ForestName.y, y = num_species, fill = ForestType)) + 
     geom_boxplot() + theme_bw() +
     labs(x = "Forest Name", y = "Number of Species") + geom_point() + theme(
          axis.text.x = element_text(angle = 45, vjust = 0.6)
     ) + scale_fill_manual(values=cbbPalette) 

ggsave("Boxplot number of species per forest.PNG",device = "png")

getwd()

#colors
# The palette with black:
cbbPalette <- c("#7b3294", "#008837")


# To use for fills, add
scale_fill_manual(values=cbPalette)

#Calculating how many pictures had nothing in them 
Data %>% group_by(Data$choice == "NOTHINGHERE") %>% summarise(
     num_nothing = n()
)

#Calculating the total number of camera trap days 

Cam.days <- Data %>% group_by(Camera.y) %>% summarise(
     n_cam_days = sum(Data$Camera_Trap_Days.y)
)

total <- Data %>% summarise(
     total_days = sum(Data$Camera_Trap_Days.y)
)

n_distinct(Data$Camera.y)


