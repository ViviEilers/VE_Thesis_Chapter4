#Creating a data set from the folder with images

# create list of file names in folder (pictures to be selected from):
#install.packages("fs")
library(fs)
CAM<- list.files("C:/Users/Vivianne Eilers/Dropbox/Vivianne/Doutorado/Uni documents/Fieldwork Scotland/Videos/Processed_videos/Field_2/Images_2", recursive = T) 

CAM.split<-sapply(CAM, FUN=function(x){
  strsplit(x, "/")
})

Videos<-do.call(rbind, CAM.split)
head(Videos)

Videos<-unique(Videos[, 1:3])
table(Videos[,1], Videos[,2])

Videos<-data.frame(Videos)
Videos$Time<-substring(Videos[,3], 12, 19) # Getting the time from the name of the video

#Changing Time to POSIXct####
# I need Date and time in the same column###
Videos$date.time<- paste(Videos$X2, Videos$Time)
class(Videos$date.time)
Videos$P.time<-as.POSIXct(Videos$date.time, format="%Y-%m-%d %H-%M-%S", tz="UTC") 
class(Videos$P.time)

# Using one sunrise date for all days to simplify
Videos$sunrise<-"08:20:30"
Videos$sunrise.date.time<- paste(Videos$X2, Videos$sunrise)
class(Videos$sunrise.date.time)
Videos$sunrise.date.time<-as.POSIXct(Videos$sunrise.date.time, format="%Y-%m-%d %H:%M:%S", tz="UTC") 

# Calculating time in minutes after sunrise####
Videos$M.A.sunrise<-difftime(Videos$P.time, Videos$sunrise.date.time, units="mins") # in minutes
str(Videos)
Videos$M.A.sunrise<-as.numeric(Videos$M.A.sunrise) # minutes after sunrise as numeric
#Videos$M.A.sunrise<-as.integer(Videos$M.A.sunrise) # minutes after sunrise as integer

#Videos$bin10<-cut(Videos$M.A.sunrise, breaks = 50) # using minutes after sunrise to create the bins
#Videos$bin10min<-cut(Videos$M.A.sunrise, breaks = c(seq(-19:131, 391:501, by = 10))) # using minutes after sunrise to create the bins
#summary(Videos$bin10min)

#Removing bins with no videos###
Videos$bin10min<-cut(Videos$M.A.sunrise, breaks = c(-19, -9, 1, 11, 21, 31, 41, 51, 61, 71, 81, 91, 101, 111, 121, 131, 391, 401, 411, 421, 431, 441, 451, 461, 471, 481, 491, 501)) # using minutes after sunrise to create the bins
summary(Videos$bin10min)

# Reducing the number of bins
Videos$bin20min<-cut(Videos$M.A.sunrise, breaks = c(-19, 1, 21, 41, 61, 81, 101, 121, 141, 391, 411, 431, 451, 471, 491, 511)) # using minutes after sunrise to create the bins
summary(Videos$bin20min)
head(Videos)
str(Videos)

# Reducing the number of bins
Videos$bin30min<-cut(Videos$M.A.sunrise, breaks = c(-19, 11, 41, 71, 101, 131, 391, 421, 451, 481, 511)) # using minutes after sunrise to create the bins
summary(Videos$bin30min)
head(Videos)
str(Videos)

#Videos$CAM<-Videos$X1
#Videos$Date<-Videos$X2

#data.frame(matrix(table(Videos[,1], Videos[,2], Videos[,10]), nrow=8, byrow=T))

#data.frame(matrix(table(Videos$CAM, Videos$Date, Videos$bin10), nrow=8, byrow=T))

#Videos_2<-as.data.frame.matrix(table(Videos[,1], Videos[,2], Videos[,10]))

# Splitting data by CAM
# Counting the numbers of videos per bin and per date
B1C1.L<-Videos[Videos$X1=="Bresil_1_CAM1",]
str(B1C1.L)
#B1C1.Lc<-as.data.frame.matrix(table(B1C1.L[,10], B1C1.L[,2])) # 10 min bin
B1C1.Lc<-as.data.frame.matrix(table(B1C1.L[,11], B1C1.L[,2]))  # 20 min bin
B1C1.Lc$CAM<-"B1C1"
str(B1C1.Lc)
head(B1C1.Lc)

B1C2.R<-Videos[Videos$X1=="Bresil_1_CAM2",]
#B1C2.Rc<-as.data.frame.matrix(table(B1C2.R[,10], B1C2.R[,2]))
B1C2.Rc<-as.data.frame.matrix(table(B1C2.R[,11], B1C2.R[,2]))
B1C2.Rc$CAM<-"B1C2"

B1C3.L<-Videos[Videos$X1=="Bresil_1_CAM3",]
#B1C3.Lc<-as.data.frame.matrix(table(B1C3.L[,10], B1C3.L[,2]))
B1C3.Lc<-as.data.frame.matrix(table(B1C3.L[,11], B1C3.L[,2]))
B1C3.Lc$CAM<-"B1C3"

B1C4.R<-Videos[Videos$X1=="Bresil_1_CAM4",]
#B1C4.Rc<-as.data.frame.matrix(table(B1C4.R[,10], B1C4.R[,2]))
B1C4.Rc<-as.data.frame.matrix(table(B1C4.R[,11], B1C4.R[,2]))
B1C4.Rc$CAM<-"B1C4"

B2C1.R<-Videos[Videos$X1=="Bresil_2_CAM1",]
#B2C1.Rc<-as.data.frame.matrix(table(B2C1.R[,10], B2C1.R[,2]))
B2C1.Rc<-as.data.frame.matrix(table(B2C1.R[,11], B2C1.R[,2]))
B2C1.Rc$CAM<-"B2C1"

B2C2.L<-Videos[Videos$X1=="Bresil_2_CAM2",]
#B2C2.Lc<-as.data.frame.matrix(table(B2C2.L[,10], B2C2.L[,2]))
B2C2.Lc<-as.data.frame.matrix(table(B2C2.L[,11], B2C2.L[,2]))
B2C2.Lc$CAM<-"B2C2"

B2C3.R<-Videos[Videos$X1=="Bresil_2_CAM3",]
#B2C3.Rc<-as.data.frame.matrix(table(B2C3.R[,10], B2C3.R[,2]))
B2C3.Rc<-as.data.frame.matrix(table(B2C3.R[,11], B2C3.R[,2]))
B2C3.Rc$CAM<-"B2C3"

B2C4.L<-Videos[Videos$X1=="Bresil_2_CAM4",]
#B2C4.Lc<-as.data.frame.matrix(table(B2C4.L[,10], B2C4.L[,2]))
B2C4.Lc<-as.data.frame.matrix(table(B2C4.L[,11], B2C4.L[,2]))
B2C4.Lc$CAM<-"B2C4"

######
#Data from 22/Oct to 03/Dec 2020#####
S.data<-read.csv('C:/Users/Vivianne Eilers/Dropbox/Vivianne/Doutorado/Uni documents/Fieldwork Scotland/Dist_sampling/data/raw_data/Field_data.csv', header = TRUE, stringsAsFactors = TRUE, na.strings=c(""," ","NA"))

S.data$N_Birds<- gsub(">", "", S.data$N_Birds, fixed=T) # replacing the symbol ">" by nothing ""
S.data$N_Birds<-as.numeric(S.data$N_Birds) # Number of birds as numeric variable

str(S.data)
summary(S.data)

#There are data from 20/11/2020 to 03/12/2020 but
#I'm using images from 26/11/03/12/2020 so
#I need to remove the other dates

#S.data<-S.data[S.data$Date==   c("26/11/2020", "27/11/2020", "01/12/2020", "03/12/2020"),] #Doesn't work, gives less results

S.data.26<-S.data[S.data$Date==("26/11/2020"),]
S.data.27<-S.data[S.data$Date==("27/11/2020"),]
S.data.01<-S.data[S.data$Date==("01/12/2020"),]
S.data.03<-S.data[S.data$Date==("03/12/2020"),]

S.data<-rbind(S.data.26, S.data.27, S.data.01, S.data.03)

str(S.data)
summary(S.data)

#Changing date to PoSIXct####
S.data$Date<-as.character(S.data$Date)
S.data$Ddate<-as.Date(S.data$Date, format="%d/%m/%Y") 
S.data$P.date<-as.POSIXct(S.data$Date, format="%d/%m/%Y") 
str(S.data)

#Calculating sunrise times####
library(StreamMetabolism)
Lat<-57.211076
Long<--2.139298

sunrise.set(Lat, Long, "2020/12/03", timezone="UTC") # sunrise at 08:27:03, with no warnings
sunrise.set(Lat, Long, "2020/12/01", timezone="UTC") # sunrise at 08:23:41, with no warnings
sunrise.set(Lat, Long, "2020/11/27", timezone="UTC") # sunrise at 08:16:26, with no warnings
sunrise.set(Lat, Long, "2020/11/26", timezone="UTC") # sunrise at 08:14:31, with no warnings

# S.data$sunrise<-ifelse(S.data$Ddate=="2020-12-03", "08:27:03",
#                        ifelse(S.data$Ddate=="2020-12-01", "08:23:41",
#                               ifelse(S.data$Ddate=="2020-11-27", "08:16:26",
#                                      ifelse(S.data$Ddate=="2020-11-26", "08:14:31", NA))))

# Using one sunrise date for all days to simplify
S.data$sunrise<-"08:20:30"
class(S.data$sunrise.date.time)

#Changing time to POSIXct####
# I need Date and time in the same column###
S.data$date.time<- paste(S.data$Date, S.data$Time)
class(S.data$date.time)
S.data$P.time<-as.POSIXct(S.data$date.time, format="%d/%m/%Y %H:%M:%S", tz="UTC") 
S.data$sunrise.date.time<- paste(S.data$Date, S.data$sunrise)
class(S.data$sunrise.date.time)
S.data$sunrise.date.time<-as.POSIXct(S.data$sunrise.date.time, format="%d/%m/%Y %H:%M:%S", tz="UTC") 
str(S.data)

# Calculating time in minutes after sunrise####
#S.data$T.A.sunrise<-S.data$P.time - S.data$sunrise.date.time # in seconds
S.data$M.A.sunrise<-difftime(S.data$P.time, S.data$sunrise.date.time, units="mins") # in minutes

# Splitting the time in bins####
S.data$M.A.sunrise<-as.numeric(S.data$M.A.sunrise) # minutes after sunrise as numeric
#S.data$bin10<-cut(S.data$M.A.sunrise, breaks = 50) # using minutes after sunrise to create the bins
#S.data$bin10min<-cut(S.data$M.A.sunrise, breaks = c(seq(-19, 501, by = 10))) # using minutes after sunrise to create the bins

S.data$bin10min<-cut(S.data$M.A.sunrise, breaks = c(-19, -9, 1, 11, 21, 31, 41, 51, 61, 71, 81, 91, 101, 111, 121, 131, 391, 401, 411, 421, 431, 441, 451, 461, 471, 481, 491, 501)) # using minutes after sunrise to create the bins

# Reducing the number of bins
S.data$bin20min<-cut(S.data$M.A.sunrise, breaks = c(-19, 1, 21, 41, 61, 81, 101, 121, 141, 391, 411, 431, 451, 471, 491, 511)) # using minutes after sunrise to create the bins
summary(S.data$bin20min)

# Reducing the number of bins
S.data$bin30min<-cut(S.data$M.A.sunrise, breaks = c(-19, 11, 41, 71, 101, 131, 391, 421, 451, 481, 511)) # using minutes after sunrise to create the bins
summary(S.data$bin30min)

# Counting the number of observations in each bin of 10 min per date####
table(S.data[,25], S.data[,19])

# Counting the number of observations in each bin of 20 min per date####
table(S.data[,26], S.data[,19])

# Counting the number of observations in each bin of 30 min per date####
table(S.data[,27], S.data[,19])

#I need to split the data per side (left and right) before counting the observations per bin
#Side filled with NA is associated with 1, the central tower that splits the stretch into right and left
#NAs is considered left because the tower is placed to the left of the cameras and the observer

S.data$Side[is.na(S.data$Side)] <- "left"
S.data.L<-S.data[S.data$Side == "left",]
S.data.R<-S.data[S.data$Side == "right",]

# Counting the number of observations in each bin of 10 min per side per day####
#S.data.Lc<-as.data.frame.matrix(table(S.data.L[,25], S.data.L[,19]))
#head(S.data.Lc)
#S.data.Rc<-as.data.frame.matrix(table(S.data.R[,25], S.data.R[,19]))
#head(S.data.Rc)

# Counting the number of observations in each bin of 20 min per side####
S.data.Lc<-as.data.frame.matrix(table(S.data.L[,26], S.data.L[,19]))
S.data.Lc$Count<-rowSums(table(S.data.L[,26], S.data.L[,19]))
head(S.data.Lc)
S.data.Rc<-as.data.frame.matrix(table(S.data.R[,26], S.data.R[,19]))
S.data.Rc$Count<-rowSums(table(S.data.R[,26], S.data.R[,19]))
head(S.data.Rc)

# Counting the number of observations in each bin of 30 min per side####
S.data.Lc.2<-as.data.frame.matrix(table(S.data.L[,27], S.data.L[,19]))
S.data.Lc.2$Count<-rowSums(table(S.data.L[,27], S.data.L[,19]))
head(S.data.Lc.2)
S.data.Rc.2<-as.data.frame.matrix(table(S.data.R[,27], S.data.R[,19]))
S.data.Rc.2$Count<-rowSums(table(S.data.R[,27], S.data.R[,19]))
head(S.data.Rc)

#####
#Merging all data frames to plot the number of observation per time slot
#Time.bin<-levels(S.data.L$bin10min)
#Time.bin<-levels(S.data.L$bin20min)
Time.bin<-levels(S.data.L$bin30min)

#Visual data
Count.data<-data.frame(Time.bin)
Count.data$Visual.L<-S.data.Lc.2$Count
Count.data$Visual.R<-S.data.Rc.2$Count

#Adding camera data
Count.data$B1C1.L<-rowSums(table(B1C1.L[,12], B1C1.L[,2]))
Count.data$B1C2.R<-rowSums(table(B1C2.R[,12], B1C2.R[,2]))
Count.data$B1C3.L<-rowSums(table(B1C3.L[,12], B1C3.L[,2]))
Count.data$B1C4.R<-rowSums(table(B1C4.R[,12], B1C4.R[,2]))
Count.data$B2C1.R<-rowSums(table(B2C1.R[,12], B2C1.R[,2]))
Count.data$B2C2.L<-rowSums(table(B2C2.L[,12], B2C2.L[,2]))
Count.data$B2C3.R<-rowSums(table(B2C3.R[,12], B2C3.R[,2]))
Count.data$B2C4.L<-rowSums(table(B2C4.L[,12], B2C4.L[,2]))

# Calculating the difference in abundance for each combination of methods
Count.data$VL_B1C1L<-Count.data$Visual.L-Count.data$B1C1.L
Count.data$VL_B1C3L<-Count.data$Visual.L-Count.data$B1C3.L
Count.data$VL_B2C2L<-Count.data$Visual.L-Count.data$B2C2.L
Count.data$VL_B2C4L<-Count.data$Visual.L-Count.data$B2C4.L
Count.data$VR_B1C2R<-Count.data$Visual.R-Count.data$B1C2.R
Count.data$VR_B1C4R<-Count.data$Visual.R-Count.data$B1C4.R
Count.data$VR_B2C1R<-Count.data$Visual.R-Count.data$B2C1.R
Count.data$VR_B2C3R<-Count.data$Visual.R-Count.data$B2C3.R

str(Count.data)
#Count.data$OrderBins<-c(1:15)
Count.data$OrderBins<-c(1:10)
Count.data$Time.bin.or<-reorder(Count.data$Time.bin, Count.data$OrderBins)

#Comparing the number of observations for each side
library(ggplot2)
library(reshape2)
#library(tidyverse)

#All methods
Count.all<-Count.data[, c(2:11, 21)]
Count.all.2<-melt(Count.all, id.vars="Time.bin.or")
ggplot(Count.all.2, aes(x=Time.bin.or, y=value, fill=variable)) + geom_bar(stat='identity', position='dodge')

par(mfrow=c(2,2))
#Left side
Count.L<-Count.data[, c(2, 4, 6, 9, 11, 21)]
Count.L.2<-melt(Count.L, id.vars="Time.bin.or")
p1<-ggplot(Count.L.2, aes(x=Time.bin.or, y=value, fill=variable)) +
  ylim(0, 52) +
  geom_bar(stat='identity', position='dodge')

#Right side
Count.R<-Count.data[, c(3, 5, 7, 8, 10, 21)]
Count.R.2<-melt(Count.R, id.vars="Time.bin.or")
p2<-ggplot(Count.R.2, aes(x=Time.bin.or, y=value, fill=variable)) + 
  ylim(0, 52) +
  geom_bar(stat='identity', position='dodge')

#Using dots instead of bars
#Left side
Count.L<-Count.data[, c(2, 4, 6, 9, 11, 21)]
Count.L.2<-melt(Count.L, id.vars="Time.bin.or")
Count.L.2$Method<-ifelse(Count.L.2$variable == "Visual.L", "Visual - left",
                             ifelse(Count.L.2$variable == "B1C1.L", "Set 1 - 2.8mm",
                                    ifelse(Count.L.2$variable == "B1C3.L", "Set 1 - 6mm",
                                           ifelse(Count.L.2$variable == "B2C2.L", "Set 2 - 2.8mm",
                                                  ifelse(Count.L.2$variable == "B2C4.L", "Set 2 - 6mm", NA)))))

p1.1<-ggplot(Count.L.2, aes(x=Time.bin.or, y=value, fill=Method)) +
  ylim(0, 52) +
  geom_point(aes(colour=Method, shape=Method), size = 3) + 
  geom_line(aes(colour=Method, group = Method))+
  scale_colour_manual(values=c("green3", "green3", "pink2", "pink2", "blue")) +
  scale_fill_manual(values=c("green3", "green3", "pink2", "pink2", "blue")) +
  scale_shape_manual(values=c(22, 24, 22, 24, 21))+
  labs(y="Count", x = "Time after sunrise (min)")

#Right side
Count.R<-Count.data[, c(3, 5, 7, 8, 10, 21)]
Count.R.2<-melt(Count.R, id.vars="Time.bin.or")
Count.R.2$Method<-ifelse(Count.R.2$variable == "Visual.R", "Visual - right",
                         ifelse(Count.R.2$variable == "B1C2.R", "Set 1 - 2.8mm",
                                ifelse(Count.R.2$variable == "B1C4.R", "Set 1 - 6mm",
                                       ifelse(Count.R.2$variable == "B2C1.R", "Set 2 - 2.8mm",
                                              ifelse(Count.R.2$variable == "B2C3.R", "Set 2 - 6mm", NA)))))

p2.1<-ggplot(Count.R.2, aes(x=Time.bin.or, y=value, fill=Method)) +
  ylim(0, 52) +
  geom_point(aes(colour=Method, shape=Method), size = 3) + 
  geom_line(aes(colour=Method, group = Method))+
  scale_colour_manual(values=c("green3", "green3", "pink2", "pink2", "blue")) +
  scale_fill_manual(values=c("green3", "green3", "pink2", "pink2", "blue")) +
  scale_shape_manual(values=c(22, 24, 22, 24, 21))+
  labs(y="Count", x = "Time after sunrise (min)")

#Plotting the difference in the methods to compare the abundance

#All methods
Count.data.all<-Count.data[, c(12:19, 21)]
Count.data.all.2<-melt(Count.data.all, id.vars="Time.bin.or")
ggplot(Count.data.all.2, aes(x=Time.bin.or, y=value, fill=variable)) + geom_bar(stat='identity', position='dodge')

#Left side
Count.data.L<-Count.data[, c(12:15, 21)]
Count.data.L.4<-melt(Count.data.L, id.vars="Time.bin.or")
p3<-ggplot(Count.data.L.4, aes(x=Time.bin.or, y=value, fill=variable)) +
  ylim(-26, 32) +
  geom_bar(stat='identity', position='dodge')

#Right side
Count.data.R<-Count.data[, c(16:19, 21)]
Count.data.R.2<-melt(Count.data.R, id.vars="Time.bin.or")
p4<-ggplot(Count.data.R.2, aes(x=Time.bin.or, y=value, fill=variable)) + 
  ylim(-26, 32) +
  geom_bar(stat='identity', position='dodge')

library(gridExtra)

grid.arrange(p1, p2, p3, p4, nrow =2)

#Using dots instead of bars
#Left side
Count.data.L<-Count.data[, c(12:15, 21)]
Count.data.L.4<-melt(Count.data.L, id.vars="Time.bin.or")
Count.data.L.4$Camera<-ifelse(Count.data.L.4$variable == "VL_B1C1L", "Set 1 - 2.8mm",
                                ifelse(Count.data.L.4$variable == "VL_B1C3L", "Set 1 - 6mm",
                                       ifelse(Count.data.L.4$variable == "VL_B2C2L", "Set 2 - 2.8mm",
                                              ifelse(Count.data.L.4$variable == "VL_B2C4L", "Set 2 - 6mm", NA))))

p3.1<-ggplot(Count.data.L.4, aes(x=Time.bin.or, y=value, fill=Camera)) +
  ylim(-26, 32) +
  geom_point(aes(colour=Camera, shape=Camera), size = 3) + 
  geom_line(aes(colour=Camera, group = Camera))+
  scale_colour_manual(values=c("green3", "green3", "pink2", "pink2")) +
  scale_fill_manual(values=c("green3", "green3", "pink2", "pink2")) +
  scale_shape_manual(values=c(22, 24, 22, 24))+
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  labs(y="Difference visual - camera", x = "Time after sunrise (min)")

#Right side
Count.data.R<-Count.data[, c(16:19, 21)]
Count.data.R.2<-melt(Count.data.R, id.vars="Time.bin.or")
Count.data.R.2$Camera<-ifelse(Count.data.R.2$variable == "VR_B1C2R", "Set 1 - 2.8mm",
                              ifelse(Count.data.R.2$variable == "VR_B1C4R", "Set 1 - 6mm",
                                     ifelse(Count.data.R.2$variable == "VR_B2C1R", "Set 2 - 2.8mm",
                                            ifelse(Count.data.R.2$variable == "VR_B2C3R", "Set 2 - 6mm", NA))))

p4.1<-ggplot(Count.data.R.2, aes(x=Time.bin.or, y=value, fill=Camera)) + 
  ylim(-26, 32) +
  geom_point(aes(colour=Camera, shape=Camera), size = 3) + 
  geom_line(aes(colour=Camera, group = Camera))+
  scale_colour_manual(values=c("green3", "green3", "pink2", "pink2")) +
  scale_fill_manual(values=c("green3", "green3", "pink2", "pink2")) +
  scale_shape_manual(values=c(22, 24, 22, 24))+
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  labs(y="Difference visual - camera", x = "Time after sunrise (min)")

grid.arrange(p1.1, p2.1, p3.1, p4.1, nrow =2)

####
# Calculating only the useful information based on overlapping of two images####

#Left side
#2.8mm cameras B1C1>B2C2
Count.data$L2.8<- ifelse(Count.data$B1C1.L >= Count.data$B2C2.L, Count.data$B2C2.L,Count.data$B1C1.L)
                      
#6 and 2.8 B1C3>B2C2
Count.data$L6_2.8<-ifelse(Count.data$B1C3.L >= Count.data$B2C2.L, Count.data$B2C2.L, Count.data$B1C3.L)

#2.8 and 6 B1C1>B2C4
Count.data$L2.8_6<-ifelse(Count.data$B1C1.L >= Count.data$B2C4.L, Count.data$B2C4.L, Count.data$B1C1.L)

# 6 mm cameras B1C3>B2C4
Count.data$L6<-ifelse(Count.data$B1C3.L >= Count.data$B2C4.L, Count.data$B2C4.L, Count.data$B1C3.L)

#Right side
#2.8mm cameras B2C1>B1C2
Count.data$R2.8<- ifelse(Count.data$B2C1.R >= Count.data$B1C2.R, Count.data$B1C2.R,Count.data$B2C1.R)

#6 and 2.8 B1C3>B2C2
Count.data$R6_2.8<-ifelse(Count.data$B2C3.R >= Count.data$B1C2.R, Count.data$B1C2.R, Count.data$B2C3.R)

#2.8 and 6 B1C1>B2C4
Count.data$R2.8_6<-ifelse(Count.data$B2C1.R >= Count.data$B1C4.R, Count.data$B1C4.R, Count.data$B2C1.R)

# 6 mm cameras B1C3>B2C4
Count.data$R6<-ifelse(Count.data$B2C3.R >= Count.data$B1C4.R, Count.data$B1C4.R, Count.data$B2C3.R)

# Getting only visual data with binoculars ####
WB.data<-S.data[!is.na(S.data$B_distance),]
str(WB.data)
WB.data.L<-WB.data[WB.data$Side == "left",]
WB.data.R<-WB.data[WB.data$Side == "right",]

# Counting the number of observations in each bin of 30 min per side####
#Left side
WB.data.Lc.2<-as.data.frame.matrix(table(WB.data.L[,27], WB.data.L[,19]))
WB.data.Lc.2$Count<-rowSums(table(WB.data.L[,27], WB.data.L[,19]))
head(WB.data.Lc.2)
#Right side
WB.data.Rc.2<-as.data.frame.matrix(table(WB.data.R[,27], WB.data.R[,19]))
WB.data.Rc.2$Count<-rowSums(table(WB.data.R[,27], WB.data.R[,19]))
head(WB.data.Rc.2)

Count.data$Bin.L<-WB.data.Lc.2$Count
Count.data$Bin.R<-WB.data.Rc.2$Count

#Left side graph
WB.count.L<-Count.data[, c(30, 22:25,  21)]
WB.count.L.2<-melt(WB.count.L, id.vars="Time.bin.or")
p5<-ggplot(WB.count.L.2, aes(x=Time.bin.or, y=value, fill=variable)) + 
  ylim(0, 28) +
    geom_bar(stat='identity', position='dodge')

#Right side graph
WB.count.R<-Count.data[, c(31, 26:29, 21)]
WB.count.R.2<-melt(WB.count.R, id.vars="Time.bin.or")
p6<-ggplot(WB.count.R.2, aes(x=Time.bin.or, y=value, fill=variable)) + 
  ylim(0, 28) +
  geom_bar(stat='identity', position='dodge')

#Using dots instead of bars
#Left side
WB.count.L.2$Method<-ifelse(WB.count.L.2$variable == "Bin.L", "Binoculars - left",
                            ifelse(WB.count.L.2$variable == "L2.8", "2.8mm - 2.8mm",
                                   ifelse(WB.count.L.2$variable == "L6_2.8", "6mm - 2.8mm",
                                          ifelse(WB.count.L.2$variable == "L2.8_6", "2.8mm - 6mm",
                                                 ifelse(WB.count.L.2$variable == "L6", "6mm - 6mm", NA)))))

p5.1<-ggplot(WB.count.L.2, aes(x=Time.bin.or, y=value, fill=Method)) +
  ylim(0, 28) +
  geom_point(aes(colour=Method, shape=Method), size = 3) + 
  geom_line(aes(colour=Method, group = Method))+
  scale_colour_manual(values=c("orange", "blue", "purple", "pink", "green3")) +
  scale_fill_manual(values=c("orange", "blue", "purple", "pink", "green3")) +
  scale_shape_manual(values=c(22, 23, 24, 21, 25))+
  labs(y="Count", x = "Time after sunrise (min)")

#Right side
WB.count.R.2$Method<-ifelse(WB.count.R.2$variable == "Bin.R", "Binoculars - right",
                            ifelse(WB.count.R.2$variable == "R2.8", "2.8mm - 2.8mm",
                                   ifelse(WB.count.R.2$variable == "R6_2.8", "6mm - 2.8mm",
                                          ifelse(WB.count.R.2$variable == "R2.8_6", "2.8mm - 6mm",
                                                 ifelse(WB.count.R.2$variable == "R6", "6mm - 6mm", NA)))))

p6.1<-ggplot(WB.count.R.2, aes(x=Time.bin.or, y=value, fill=Method)) +
  ylim(0, 28) +
  geom_point(aes(colour=Method, shape=Method), size = 3) + 
  geom_line(aes(colour=Method, group = Method))+
  scale_colour_manual(values=c("orange", "blue", "purple", "pink", "green3")) +
  scale_fill_manual(values=c("orange", "blue", "purple", "pink", "green3")) +
  scale_shape_manual(values=c(22, 23, 24, 21, 25))+
  labs(y="Count", x = "Time after sunrise (min)")

# Calculating the difference in abundance for each combination of methods

Count.data$BL_L2.8<-Count.data$Bin.L-Count.data$L2.8
Count.data$BL_L6_2.8<-Count.data$Bin.L-Count.data$L6_2.8
Count.data$BL_L2.8_6<-Count.data$Bin.L-Count.data$L2.8_6
Count.data$BL_L6<-Count.data$Bin.L-Count.data$L6
Count.data$BR_R2.8<-Count.data$Bin.R-Count.data$R2.8
Count.data$BR_R6_2.8<-Count.data$Bin.R-Count.data$R6_2.8
Count.data$BR_R2.8_6<-Count.data$Bin.R-Count.data$R2.8_6
Count.data$BR_R6<-Count.data$Bin.R-Count.data$R6
str(Count.data)

#Left side graph
WB.count.data.L<-Count.data[, c(32:35, 21)]
WB.count.data.L.2<-melt(WB.count.data.L, id.vars="Time.bin.or")
p7<-ggplot(WB.count.data.L.2, aes(x=Time.bin.or, y=value, fill=variable)) + 
  ylim(-15, 23) +
  geom_bar(stat='identity', position='dodge')

#Right side graph
WB.count.data.R<-Count.data[, c(36:39, 21)]
WB.count.data.R.2<-melt(WB.count.data.R, id.vars="Time.bin.or")
p8<-ggplot(WB.count.data.R.2, aes(x=Time.bin.or, y=value, fill=variable)) + 
  ylim(-15, 23) +
  geom_bar(stat='identity', position='dodge')

grid.arrange(p5, p6, p7, p8, nrow =2)

#Using dots instead of bars
#Left side
WB.count.data.L.2$Camera<-ifelse(WB.count.data.L.2$variable == "BL_L2.8", "2.8mm - 2.8mm",
                              ifelse(WB.count.data.L.2$variable == "BL_L6_2.8", "6mm - 2.8mm",
                                     ifelse(WB.count.data.L.2$variable == "BL_L2.8_6", "2.8mm - 6mm",
                                            ifelse(WB.count.data.L.2$variable == "BL_L6", "6mm - 6mm", NA))))

p7.1<-ggplot(WB.count.data.L.2, aes(x=Time.bin.or, y=value, fill=Camera)) +
  ylim(-26, 32) +
  geom_point(aes(colour=Camera, shape=Camera), size = 3) + 
  geom_line(aes(colour=Camera, group = Camera))+
  scale_colour_manual(values=c("orange", "blue", "purple", "pink")) +
  scale_fill_manual(values=c("orange", "blue", "purple", "pink")) +
  scale_shape_manual(values=c(22, 23, 24, 21))+
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  labs(y="Difference binoculars - camera", x = "Time after sunrise (min)")

#Right side
WB.count.data.R.2$Camera<-ifelse(WB.count.data.R.2$variable == "BR_R2.8", "2.8mm - 2.8mm",
                              ifelse(WB.count.data.R.2$variable == "BR_R6_2.8", "6mm - 2.8mm",
                                     ifelse(WB.count.data.R.2$variable == "BR_R2.8_6", "2.8mm - 6mm",
                                            ifelse(WB.count.data.R.2$variable == "BR_R6", "6mm - 6mm", NA))))

p8.1<-ggplot(WB.count.data.R.2, aes(x=Time.bin.or, y=value, fill=Camera)) + 
  ylim(-26, 32) +
  geom_point(aes(colour=Camera, shape=Camera), size = 3) + 
  geom_line(aes(colour=Camera, group = Camera))+
  scale_colour_manual(values=c("orange", "blue", "purple", "pink")) +
  scale_fill_manual(values=c("orange", "blue", "purple", "pink")) +
  scale_shape_manual(values=c(22, 23, 24, 21))+
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  labs(y="Difference binoculars - camera", x = "Time after sunrise (min)")

grid.arrange(p5.1, p6.1, p7.1, p8.1, nrow =2)

# Binoculars data and difference between binoculars and cameras together
WB.count.data.L.3<-Count.data[, c(30, 32:35, 21)]
WB.count.data.L.4<-melt(WB.count.data.L.3, id.vars="Time.bin.or")
p9<-ggplot(WB.count.data.L.4, aes(x=Time.bin.or, y=value, fill=variable)) + 
  ylim(-15, 30) +
  geom_bar(stat='identity', position='dodge')

WB.count.data.R.3<-Count.data[, c(31, 36:39, 21)]
WB.count.data.R.4<-melt(WB.count.data.R.3, id.vars="Time.bin.or")
p10<-ggplot(WB.count.data.R.4, aes(x=Time.bin.or, y=value, fill=variable)) + 
  ylim(-15, 30) +
  geom_bar(stat='identity', position='dodge')

grid.arrange(p9, p10, nrow =1)

# 2.8 mm left
WB.count.data.2L<-Count.data[, c(39, 21)]
WB.count.data.2L.2<-melt(WB.count.data.2L, id.vars="Time.bin.or")
ggplot(WB.count.data.2L.2, aes(x=Time.bin.or, y=value, fill=variable)) + geom_bar(stat='identity', position='dodge')

# 2.8 mm  6mm left
WB.count.data.26L<-Count.data[, c(33, 21)]
WB.count.data.26L.2<-melt(WB.count.data.26L, id.vars="Time.bin.or")
ggplot(WB.count.data.26L.2, aes(x=Time.bin.or, y=value, fill=variable)) + geom_bar(stat='identity', position='dodge')

# 6 mm  2.8mm left
WB.count.data.62L<-Count.data[, c(32, 21)]
WB.count.data.62L.2<-melt(WB.count.data.62L, id.vars="Time.bin.or")
ggplot(WB.count.data.62L.2, aes(x=Time.bin.or, y=value, fill=variable)) + geom_bar(stat='identity', position='dodge')

# 6mm left
WB.count.data.6L<-Count.data[, c(34, 21)]
WB.count.data.6L.2<-melt(WB.count.data.6L, id.vars="Time.bin.or")
ggplot(WB.count.data.6L.2, aes(x=Time.bin.or, y=value, fill=variable)) + geom_bar(stat='identity', position='dodge')

# 2.8 mm right
WB.count.data.2R<-Count.data[, c(35, 21)]
WB.count.data.2R.2<-melt(WB.count.data.2R, id.vars="Time.bin.or")
ggplot(WB.count.data.2R.2, aes(x=Time.bin.or, y=value, fill=variable)) + geom_bar(stat='identity', position='dodge')

# 2.8 mm  6mm right
WB.count.data.26R<-Count.data[, c(37, 21)]
WB.count.data.26R.2<-melt(WB.count.data.26R, id.vars="Time.bin.or")
ggplot(WB.count.data.26R.2, aes(x=Time.bin.or, y=value, fill=variable)) + geom_bar(stat='identity', position='dodge')

# 6 mm  2.8mm right
WB.count.data.62R<-Count.data[, c(36, 21)]
WB.count.data.62R.2<-melt(WB.count.data.62R, id.vars="Time.bin.or")
ggplot(WB.count.data.62R.2, aes(x=Time.bin.or, y=value, fill=variable)) + geom_bar(stat='identity', position='dodge')

# 6mm right
WB.count.data.6R<-Count.data[, c(38, 21)]
WB.count.data.6R.2<-melt(WB.count.data.6R, id.vars="Time.bin.or")
ggplot(WB.count.data.6R.2, aes(x=Time.bin.or, y=value, fill=variable)) + geom_bar(stat='identity', position='dodge')

#Count the data in each position
hist(S.data$Position)
hist(S.data.L$Position)
hist(S.data.R$Position, xlim=rev(c(1,10)))

hist(S.data.R$Position, ylim=c(0, 25), xlim=c(1,10), breaks=8)
hist(WB.data.R$Position, ylim=c(0, 25), xlim=c(1,10), breaks=8)

WB.pos.R<-as.data.frame.matrix(table(WB.data.R[,13], WB.data.R[,19]))
WB.pos.R$Count<-rowSums(table(WB.data.R[,13], WB.data.R[,19]))

WB.pos.R$OrderPos<-c(2:10)

WB.pos.data.R2<-WB.pos.R[,5:6]
WB.pos.data.R3<-melt(WB.pos.data.R2, id.vars="OrderPos")
ggplot(WB.pos.data.R3, aes(x=OrderPos, y=value, fill=variable)) + geom_bar(stat='identity', position='dodge')

# Getting only visual data with binoculars ####
WB.data<-S.data[!is.na(S.data$B_distance),]

#Calculating the Horizontal distance from binoculars
WB.data$H_distance<-WB.data$B_distance*cos(WB.data$B_angle*3.14/180)
hist(WB.data$H_distance)

#Calculating the height with binoculars
WB.data$B_height<-WB.data$B_distance*sin(WB.data$B_angle*3.14/180)

# Plotting estimated distance against binoculars distance
library(ggplot2)

# Using angle as categories
p11<-ggplot(WB.data, aes(x= H_distance, y= E.distance)) + 
  geom_point(aes(colour= B_angle)) +
  scale_colour_gradientn(colours= terrain.colors(10)) +
  geom_abline(intercept= 0, slope= 1)

# Using altitude as categories
p12<-ggplot(WB.data, aes(x= H_distance, y= E.distance)) + 
  geom_point(aes(colour= B_height)) +
  scale_colour_gradientn(colours= terrain.colors(10)) +
  geom_abline(intercept= 0, slope= 1)

# Plotting estimated altitude against binoculars altitude

# Using angle as categories
p13<-ggplot(WB.data, aes(x= B_height, y= E_height)) + 
  geom_point(aes(colour= B_angle)) +
  ylim(0, 300) +
  scale_colour_gradientn(colours= terrain.colors(10)) +
  geom_abline(intercept= 0, slope= 1)

# Using distance as categories
p14<-ggplot(WB.data, aes(x= B_height, y= E_height)) + 
  geom_point(aes(colour= H_distance)) +
  ylim(0, 300) +
  scale_colour_gradientn(colours= terrain.colors(10)) +
  geom_abline(intercept= 0, slope= 1)

grid.arrange(p11, p13, nrow=1)
grid.arrange(p12, p14, nrow=1)

######
# Plotting the number of observations per distance step per side####
S.data.L$bin10m<-cut(S.data.L$E.distance, breaks = c(seq(10, 310, by=10))) 
S.data.R$bin10m<-cut(S.data.R$E.distance, breaks = c(seq(10, 310, by=10))) 

S.data.L$bin50m<-cut(S.data.L$E.distance, breaks = c(seq(10, 310, by=50))) 
S.data.R$bin50m<-cut(S.data.R$E.distance, breaks = c(seq(10, 310, by=50))) 

S.data.L$breaks<-cut(S.data.L$E.distance, breaks = 10) 
S.data.R$breaks<-cut(S.data.R$E.distance, breaks = 10) 

WB.data$H_distance<-WB.data$B_distance*cos(WB.data$B_angle*3.14/180)
WB.data.L$H_distance<-WB.data.L$B_distance*cos(WB.data.L$B_angle*3.14/180)
WB.data.L$bin10m<-cut(WB.data.L$H_distance, breaks = c(seq(10, 310, by=10)))
WB.data.L$breaks<-cut(WB.data.L$E.distance, breaks = 10)

WB.data.R$H_distance<-WB.data.R$B_distance*cos(WB.data.R$B_angle*3.14/180)
WB.data.R$bin10m<-cut(WB.data.R$H_distance, breaks = c(seq(10, 310, by=10))) 
WB.data.R$breaks<-cut(WB.data.R$E.distance, breaks = 10)

WB.data.L$bin50m<-cut(WB.data.L$H_distance, breaks = c(seq(10, 310, by=50))) 
WB.data.R$bin50m<-cut(WB.data.R$H_distance, breaks = c(seq(10, 310, by=50))) 

# Counting the number of observations by distance per side####
S.data.D.L<-as.data.frame.matrix(table(S.data.L[,30], S.data.L[,19]))
S.data.D.L$Count<-rowSums(table(S.data.L[,30], S.data.L[,19]))

S.data.D.R<-as.data.frame.matrix(table(S.data.R[,30], S.data.R[,19]))
S.data.D.R$Count<-rowSums(table(S.data.R[,30], S.data.R[,19]))

WB.D.L<-as.data.frame.matrix(table(WB.data.L[,31], WB.data.L[,19]))
WB.D.L$Count<-rowSums(table(WB.data.L[,31], WB.data.L[,19]))

WB.D.R<-as.data.frame.matrix(table(WB.data.R[,31], WB.data.R[,19]))
WB.D.R$Count<-rowSums(table(WB.data.R[,31], WB.data.R[,19]))

#Merging all data frames to plot the number of observation per bins of 50m
Bin50<-levels(S.data.L$bin50m)

Breaks<-levels(S.data.L$breaks)

#Visual data
S.data.D<-data.frame(Breaks)
S.data.D$Visual.L<-S.data.D.L$Count
S.data.D$Visual.R<-S.data.D.R$Count
S.data.D$Bin.L<-WB.D.L$Count
S.data.D$Bin.R<-WB.D.R$Count

S.data.D$OrderBins<-c(1:10)
S.data.D$Breaks.or<-reorder(S.data.D$Breaks, S.data.D$OrderBins)

par(mfrow=c(2,2))

Dist.data<-S.data.D[, c(2, 3, 5)]
Dist.data.2<-melt(Dist.data, id.vars="Breaks.or")
p15<-ggplot(Dist.data.2, aes(x=Breaks.or, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  ylim(0, 21) +
  scale_x_discrete(name ="Distance (m)")

Bin.data<-S.data.D[, c(4, 5, 5)]
Bin.data.2<-melt(Bin.data, id.vars="Breaks.or")
p16<-ggplot(Bin.data.2, aes(x=Breaks.or, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  ylim(0, 21) +
  scale_x_discrete(name ="Distance (m)")

grid.arrange(p15, p16, nrow=1)
