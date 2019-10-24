acc.curve1 <- function(Data, year) {
     require(reshape)
     require(vegan)
     yr <- Data[Data$Sampling.Event == 2019, ]
     mat <- f.matrix.creator.1(yr)
     pr <- melt(mat)
     colnames(pr) <- c("Camera", "Date", "value")
          #Add the species column
          pr$Species <- NA
     ev.sp <- cast(na.omit(pr), Camera + Date ~ species, sum)
     ac <- specaccum(accumulation_data[, -c(1:2)], method = "random", permutations = 100)
     mt <- data.frame(ac$sites, ac$richness, ac$sd)
     colnames(mt) <- c("Camera.trap.events", "species", "sd")
     return(mt)
}

#Plot species accum
ggplot(mt, aes(x = Camera.trap.events, y = species)) +
     geom_line(aes(y = species - sd), linetype = "dotted") +
     geom_line(aes(y = species + sd), linetype = "dotted") + 
     geom_line() + theme_bw() + labs(x = "Number of Camera Trap Events", y = "Number of Species")

ggsave("Species Accumulation curve by camera trap events.PNG",device = "png")


 #Manually created pr dataframe from above, and trying to work through that
Data_copy <- read_csv("Camera_Trapping_Ch5_Work/Data_copy.csv")
glimpse(Data_copy)
Data_copy$Date <- as.POSIXct(Data_copy$Date, format = "%m/%Y/%d %H:%M")

Data_copy$Image1 <- as.factor(Data_copy$Image1)
Data_copy$Camera <- as.factor(Data_copy$Camera)
Data_copy$Species <- as.factor(Data_copy$Species)

######IMPORTANT USE THE DCAST FUNCTION FROM RESHAPE2 AND NOT THE CAST FUNCTION FROM RESHAPE
library(reshape2)
ev.sp <- dcast(Data_copy, Camera + Date ~ Species)

#Changing the character variables within each column to numeric values. 1 = present, 0 = absent.
accumulation_data <- ev.sp  

accumulation_data[ ,accumulation_data == "BOBCAT" ] <- 1

##########Selecting the wrong species in each column and replacing with a 1
#BOBCAT SPECIES 
Wrong <- which(accumulation_data$BOBCAT == "BOBCAT")
Wrong
accumulation_data$BOBCAT[Wrong]<-"1"

accumulation_data$BOBCAT <- as.numeric(accumulation_data$BOBCAT)
accumulation_data$BOBCAT[is.na(accumulation_data$BOBCAT)] <- 0

#COYOTE SPECIES 
Wrong <- which(accumulation_data$COYOTE == "COYOTE")
Wrong
accumulation_data$COYOTE[Wrong]<-"1"

accumulation_data$COYOTE <- as.numeric(accumulation_data$COYOTE)
accumulation_data$COYOTE[is.na(accumulation_data$COYOTE)] <- 0

#DEERWHITETAILED SPECIES 
Wrong <- which(accumulation_data$DEERWHITETAILED  == "DEERWHITETAILED")
Wrong
accumulation_data$DEERWHITETAILED [Wrong]<-"1"

accumulation_data$DEERWHITETAILED  <- as.numeric(accumulation_data$DEERWHITETAILED )
accumulation_data$DEERWHITETAILED[is.na(accumulation_data$DEERWHITETAILED)] <- 0

#DOMESTICDOG SPECIES 
Wrong <- which(accumulation_data$DOMESTICDOG  == "DOMESTICDOG")
Wrong
accumulation_data$DOMESTICDOG [Wrong]<-"1"

accumulation_data$DOMESTICDOG  <- as.numeric(accumulation_data$DOMESTICDOG )
accumulation_data$DOMESTICDOG[is.na(accumulation_data$DOMESTICDOG)] <- 0

#FISHER SPECIES 
Wrong <- which(accumulation_data$FISHER  == "FISHER")
Wrong
accumulation_data$FISHER [Wrong]<-"1"

accumulation_data$FISHER  <- as.numeric(accumulation_data$FISHER )
accumulation_data$FISHER[is.na(accumulation_data$FISHER)] <- 0

#FOXRED SPECIES 
Wrong <- which(accumulation_data$FOXRED  == "FOXRED")
Wrong
accumulation_data$FOXRED [Wrong]<-"1"

accumulation_data$FOXRED  <- as.numeric(accumulation_data$FOXRED )
accumulation_data$FOXRED[is.na(accumulation_data$FOXRED)] <- 0

#GROUSERUFFED SPECIES 
Wrong <- which(accumulation_data$GROUSERUFFED  == "GROUSERUFFED")
Wrong
accumulation_data$GROUSERUFFED [Wrong]<-"1"

accumulation_data$GROUSERUFFED  <- as.numeric(accumulation_data$GROUSERUFFED )
accumulation_data$GROUSERUFFED[is.na(accumulation_data$GROUSERUFFED)] <- 0

#HUMAN SPECIES 
Wrong <- which(accumulation_data$HUMAN  == "HUMAN")
Wrong
accumulation_data$HUMAN [Wrong]<-"1"

accumulation_data$HUMAN  <- as.numeric(accumulation_data$HUMAN )
accumulation_data$HUMAN[is.na(accumulation_data$HUMAN)] <- 0

#OTHERBIRD SPECIES 
Wrong <- which(accumulation_data$OTHERBIRD  == "OTHERBIRD")
Wrong
accumulation_data$OTHERBIRD [Wrong]<-"1"

accumulation_data$OTHERBIRD  <- as.numeric(accumulation_data$OTHERBIRD )
accumulation_data$OTHERBIRD[is.na(accumulation_data$OTHERBIRD)] <- 0

#OTHERSMALLMAMMAL SPECIES 
Wrong <- which(accumulation_data$OTHERSMALLMAMMAL  == "OTHERSMALLMAMMAL")
Wrong
accumulation_data$OTHERSMALLMAMMAL [Wrong]<-"1"

accumulation_data$OTHERSMALLMAMMAL  <- as.numeric(accumulation_data$OTHERSMALLMAMMAL )
accumulation_data$OTHERSMALLMAMMAL[is.na(accumulation_data$OTHERSMALLMAMMAL)] <- 0

#PORCUPINE SPECIES 
Wrong <- which(accumulation_data$PORCUPINE  == "PORCUPINE")
Wrong
accumulation_data$PORCUPINE [Wrong]<-"1"

accumulation_data$PORCUPINE  <- as.numeric(accumulation_data$PORCUPINE )
accumulation_data$PORCUPINE[is.na(accumulation_data$PORCUPINE)] <- 0

#RACCOON SPECIES 
Wrong <- which(accumulation_data$RACCOON  == "RACCOON")
Wrong
accumulation_data$RACCOON [Wrong]<-"1"

accumulation_data$RACCOON  <- as.numeric(accumulation_data$RACCOON )
accumulation_data$RACCOON[is.na(accumulation_data$RACCOON)] <- 0

#SNOWSHOEHARE SPECIES 
Wrong <- which(accumulation_data$SNOWSHOEHARE  == "SNOWSHOEHARE")
Wrong
accumulation_data$SNOWSHOEHARE [Wrong]<-"1"

accumulation_data$SNOWSHOEHARE  <- as.numeric(accumulation_data$SNOWSHOEHARE )
accumulation_data$SNOWSHOEHARE[is.na(accumulation_data$SNOWSHOEHARE)] <- 0

#SQUIRRELGRAY SPECIES 
Wrong <- which(accumulation_data$SQUIRRELGRAY  == "SQUIRRELGRAY")
Wrong
accumulation_data$SQUIRRELGRAY [Wrong]<-"1"

accumulation_data$SQUIRRELGRAY  <- as.numeric(accumulation_data$SQUIRRELGRAY )
accumulation_data$SQUIRRELGRAY[is.na(accumulation_data$SQUIRRELGRAY)] <- 0

#SQUIRRELRED SPECIES 
Wrong <- which(accumulation_data$SQUIRRELRED  == "SQUIRRELRED")
Wrong
accumulation_data$SQUIRRELRED [Wrong]<-"1"

accumulation_data$SQUIRRELRED  <- as.numeric(accumulation_data$SQUIRRELRED )
accumulation_data$SQUIRRELRED[is.na(accumulation_data$SQUIRRELRED)] <- 0

#TURKEY SPECIES 
Wrong <- which(accumulation_data$TURKEY  == "TURKEY")
Wrong
accumulation_data$TURKEY [Wrong]<-"1"

accumulation_data$TURKEY  <- as.numeric(accumulation_data$TURKEY )
accumulation_data$TURKEY[is.na(accumulation_data$TURKEY)] <- 0

#WEASEL SPECIES 
Wrong <- which(accumulation_data$WEASEL  == "WEASEL")
Wrong
accumulation_data$WEASEL [Wrong]<-"1"

accumulation_data$WEASEL  <- as.numeric(accumulation_data$WEASEL )
accumulation_data$WEASEL[is.na(accumulation_data$WEASEL)] <- 0

#Remove the nothing here and something here columbs 
accumulation_data$NOTHINGHERE <- NULL
accumulation_data$SOMETHINGHERE <- NULL

