#following the instructions found here: https://projects.ncsu.edu/cals/course/zo501/2016%20Sampling%20Lab/IntroR.and.Occupancy.pdf


library(unmarked)
#Make some data



camera_num<-c(1:20)
s1<-c(0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 1)
s2<-c(0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1)
s3<-c(0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0)


#make data frame
MyData<-data.frame(cbind(camera_num, s1, s2, s3))
Detection<-MyData[,2:4]
#now make df of covariates
forest<-c("P", "P", "P", "P", "P", "P", "P", "P", "P", "P","D", "D", "D", "D", "D", "D", "D", "D", "D")
tree_density<-c(0.3, 0.1, 0.05, 0.3, 0.21, 0.2, 0.31, 0.13, 0.29, 0,25, 0.6, 0.42, 0.8, 0.55, 0.59, 0.58, 0.72, 0.64, 0.5)
covars<-data.frame(cbind(forest, tree_density))

#now format for unmarked
umf<-unmarkedFrameOccu(y = Detection, siteCovs = covars)
umf

#occupancy model with no covariates
#occu(~detection~occupancy, umf)
fm1<-occu(~1 ~1, umf) #build model
fm1 #look at model

#now get the estimates for detection
backTransform(fm1['det'])

#get estimates for occupancy, our "state" variable
backTransform(fm1['state'])

#now look at covariates
#occu(~detection~occupancy, umf)
fm2<-occu(~1 ~forest, umf)
fm2
#model does not converge.  Need more data?

backTransform(fm2['det'])
backTransform(fm2['state'])

