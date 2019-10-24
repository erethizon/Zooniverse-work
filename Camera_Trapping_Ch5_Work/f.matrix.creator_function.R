f.matrix.creator.1 <- function(Data){
     #results object
     res<-list()
     
     #get the dimensions of the matrix
     
     #list if sanpling units
     cams<-unique(Data$Camera.y)
     cams<-sort(cams)
     rows<-length(cams)
     #start and end dates of sampling periods
          #Change the date_out & date_checked to date format 
          Data$Date_Out <- as.Date(Data$Date_Out)
          Data$Date_Checked <- as.Date(Data$Date_Checked)
     min<-min(Data$Date_Out)
     max<-max(Data$Date_Checked)
     cols<-max-min+1
     
    
    #sampling period
     date.header<-seq(from=min,to=max, by=1)
     mat<-matrix(NA,rows,cols,dimnames=list(cams,as.character(date.header)))
     
     #for all cameras, determine the open and close date and mark in the matrix
     start.dates<-tapply(as.character(Data$Date_Out),Data$Camera.y,unique)
     end.dates<-tapply(as.character(Data$Date_Checked),Data$Camera.y,unique)
     
     #outline the sampling periods for each camera j
     for(j in 1:length(start.dates)){
          #for each camera beginning and end of sampling
          low<-which(date.header==start.dates[j])
          hi<-which(date.header==end.dates[j])
          indx<-seq(from=low,to=hi)
          mat[j,indx]<-0
     }
     mat.template<-mat
     #get the species
     species<-unique(Data$choice)
     #construct the matrix for each species i
     for(i in 1:length(species)){
          indx<-which(Data$choice==species[i])
          #dates and cameras when/where the species was photographed
          dates<-Data$Image_Date[indx]
          cameras<-Data$Camera.y[indx]
          dates.cameras<-data.frame(dates,cameras)
          #unique combination of dates and cameras 
          dates.cameras<-unique(dates.cameras)
               #Change the dates to date format 
               dates.cameras$dates <- as.Date(dates.cameras$dates)
          #fill in the matrix
          for(j in 1:length(dates.cameras[,1])){
               col<-which(date.header==dates.cameras[j,1])
               row<-which(cams==dates.cameras[j,2])
               mat[row,col]<-1
          }
          mat.nas<-is.na(mat)
          sum.nas<-apply(mat.nas,2,sum)
          indx.nas<-which(sum.nas==rows)
          if(length(indx.nas)>0){
               mat<-mat[,-indx.nas]
          }
          
          res<-c(res,list(mat))
          #return the matrix to its original form
          mat<-mat.template
     }
     
     names(res)<-species
     #res<-lapply(res,f.dum)
     res
     
}
