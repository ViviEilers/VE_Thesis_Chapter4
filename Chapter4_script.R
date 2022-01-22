# Triangulation - 3 frames before and after the chosen second image

# Getting images at 12s more between cameras and 19s more for visual observation for the date 26/11/21 in the morning
# Getting images at 44s more between cameras and 19s more for visual observation for the date 26/11/21 in the afternoon
# Using the first frame at the time stamp - Images were taken 5 frames per second

# 8 - Calculating altitude and distance of the bird using paired images####

# Getting information from the cameras ####
CAM_coor<-read.csv("C:/Users/Vivianne Eilers/Dropbox/Vivianne/Doutorado/Uni documents/Fieldwork Scotland/Dist_sampling/data/Cam_coordinates.csv") #HP address
#CAM_coor<-read.csv("C:/Users/r01ve17/Dropbox/Vivianne/Doutorado/Uni documents/Fieldwork Scotland/Dist_sampling/data/Cam_coordinates.csv") # uni address
# Horizontal degree of field of view divided by the number of horizontal pixels
CAM_coor$P_degree<-CAM_coor$field_view/CAM_coor$H_pixel
# 1 pixel is 0.056 degrees for a 2.8 mm camera
# 1 pixel is 0.029 degrees for a 6 mm camera

# Calculating the angle between the cameras and the top of the tower
CAM_coor$C_radians<-(CAM_coor$h_tower + (CAM_coor$z_tower - CAM_coor$zcoord)) /sqrt((CAM_coor$x_tower - CAM_coor$xcoord)^2 + (CAM_coor$y_tower - CAM_coor$ycoord)^2)
CAM_coor$C_degree<-CAM_coor$C_radians * 180/pi

# Bresil_1_CAM1 is C1 - because it is left side
C1<-CAM_coor[CAM_coor$folder=="Bresil_1_CAM1",]

# Bresil_2_CAM2 is C2 - because it is left side
C2<-CAM_coor[CAM_coor$folder=="Bresil_2_CAM2",]

# Distance from C1 to C2, considering that C1 is at 0 distance
x.C2<-sqrt((C1$xcoord - C2$xcoord)^2 + (C1$ycoord - C2$ycoord)^2)

######
# Distance from C1 to the left tower
x.LT<-sqrt((C1$xcoord - C1$x_tower)^2 + (C1$ycoord - C1$y_tower)^2)

# Distance from C1 to the right tower
C3<-CAM_coor[CAM_coor$folder=="Bresil_1_CAM2",]
x.RT<-sqrt((C1$xcoord - C3$x_tower)^2 + (C1$ycoord - C3$y_tower)^2)

# Distance from C1 to the observer
Obs.x<-391764.8224
Obs.y<-814398.5397
Obs.z<-83.15896999

x.Obs<-sqrt((C1$xcoord - Obs.x)^2 + (C1$ycoord - Obs.y)^2)

######

# Altitude of B2 in relation to B1, considering that B2CAM2(C1) is at 0 altitude
z.C2<-C2$zcoord - C1$zcoord

# Getting information from the images to be paired and to fill the data frame
setwd("C:/Users/Vivianne Eilers/Dropbox/Vivianne/Doutorado/Uni documents/Fieldwork Scotland/Videos/Processed_videos/Field_2/Images_2") #HP address
#setwd("C:/Users/r01ve17/Dropbox/Vivianne/Doutorado/Uni documents/Fieldwork Scotland/Videos/Processed_videos/Field_2/Images_2") # uni address

#Bird 1 ####
#Getting the paired images for Bird 1

C1.img1<- "Bresil_1_CAM1/2020-11-26/2020-11-26_09-00-23_Bresil-1_Cam1_record_2Det_7.avi/image_000011.png" # time stamp = 09:00:25
C2.img1<- "Bresil_2_CAM2/2020-11-26/2020-11-26_09-00-35_Bresil-2_Cam2_record_2Det_9.avi/image_000011.png" # time stamp = 09:00:37

# Calculating altitude and distance for the bird 1 using paired images####

# Number of pixels from the top of the pylon to the bird - (measured by hand in XnView)
# I could calculate that automatically given that I know how to calculate the central pixel in the image and
# I have the information for the pixel of the top of the tower

B1.C1.pix<-168
B1.C2.pix<-537

# Calculating the angle between the bird 1 and the top of the pylon
B1.C1.angle<-C1$P_degree * B1.C1.pix
B1.C2.angle<-C2$P_degree * B1.C2.pix

#complementary angle between the bird 1 and C1 at horizon
B1.C1a <- B1.C1.angle + C1$C_degree
B1.C2a <- B1.C2.angle + C2$C_degree

# Converting degrees into radians
B1.C1r<-B1.C1a * pi /180
B1.C2r<-B1.C2a * pi /180

# Calculating altitude of the bird 1
Z.B1<- (x.C2 * tan(B1.C1r) * tan(B1.C2r) - z.C2 * tan(B1.C1r)) / (tan(B1.C2r) - tan(B1.C1r))

# Calculating distance of the bird 1
X.B1<- (x.C2 * tan(B1.C2r) - z.C2) / (tan(B1.C2r) - tan(B1.C1r))

#Creating a data frame to store the information from the images
Birds.comp<-data.frame(Side = character(),
                  File1 = character(),
                  File2 = character(),
                  Altitude = double(),
                  Distance = double(),
                  Bin_ID = integer(),
                  Bin_altitude = double(),
                  Bin_distance = double(),
                  Bin_time = character(),
                  Bin_angle = integer(),
                  Bird = character(),
                  Frame = integer(),
                  stringsAsFactors=FALSE)

Birds.comp[1, "Side"] <-unique(C1$side)
Birds.comp[1, "File1"] <-paste(basename(dirname(C1.img1)), basename(C1.img1), sep="/")
Birds.comp[1, "File2"] <-paste(basename(dirname(C2.img1)), basename(C2.img1), sep="/")
Birds.comp[1, "Altitude"] <-Z.B1
Birds.comp[1, "Distance"] <-X.B1

#Bringing the information from the visual observations
S.data<-read.csv('C:/Users/Vivianne Eilers/Dropbox/Vivianne/Doutorado/Uni documents/Fieldwork Scotland/Dist_sampling/data/raw_data/Field_data.csv', header = TRUE, stringsAsFactors = TRUE, na.strings=c(""," ","NA")) #HP address
#S.data<-read.csv('C:/Users/r01ve17/Dropbox/Vivianne/Doutorado/Uni documents/Fieldwork Scotland/Dist_sampling/data/raw_data/Field_data.csv', header = TRUE, stringsAsFactors = TRUE, na.strings=c(""," ","NA")) #Uni address
#Side filled with NA is associated with 1, the central tower that splits the stretch into right and left
#NAs is considered left because the tower is placed to the left of the cameras and the observer
S.data$Side[is.na(S.data$Side)] <- "left"

# Changing time to character in S.data
S.data$CTime<- as.character(S.data$Time)

#Calculating the Horizontal distance from binoculars
S.data$H_distance<-S.data$B_distance*cos(S.data$B_angle*3.14/180)

#Calculating the altitude of the birds with binoculars
S.data$B_height<-S.data$B_distance*sin(S.data$B_angle*3.14/180)

# Completing the data frame
S.data$ID<-S.data$ï..ID
Birds.comp[1, "Bin_ID"] <-S.data[S.data$ID==833,21]
Birds.comp[1, "Bin_altitude"] <-S.data[S.data$ID==833,20]
Birds.comp[1, "Bin_distance"] <-S.data[S.data$ID==833,19]
Birds.comp[1, "Bin_time"] <-S.data[S.data$ID==833,18] # time stamp = 09:00:38
Birds.comp[1, "Bin_angle"]<-S.data[S.data$ID==833,11]
Birds.comp[1, "Bird"]<-"1"
Birds.comp[1, "Frame"]<-1

#Bird 1b ####
#Getting the paired images for Bird 1b
C1.img1b<- "Bresil_1_CAM1/2020-11-26/2020-11-26_09-00-23_Bresil-1_Cam1_record_2Det_7.avi/image_000011.png" # time stamp = 09:00:25
C2.img1b<- "Bresil_2_CAM2/2020-11-26/2020-11-26_09-00-35_Bresil-2_Cam2_record_2Det_9.avi/image_000010.png" # time stamp = 09:00:37

# Calculating altitude and distance for the bird 1b using paired images####
B1b.C1.pix<-168
B1b.C2.pix<-504

# Calculating the angle between the bird 1b and the top of the pylon
B1b.C1.angle<-C1$P_degree * B1b.C1.pix
B1b.C2.angle<-C2$P_degree * B1b.C2.pix

#complementary angle between the bird 1b and C1 at horizon
B1b.C1a <- B1b.C1.angle + C1$C_degree
B1b.C2a <- B1b.C2.angle + C2$C_degree

# Converting degrees into radians
B1b.C1r<-B1b.C1a * pi /180
B1b.C2r<-B1b.C2a * pi /180

# Calculating altitude of the bird 1b
Z.B1b<- (x.C2 * tan(B1b.C1r) * tan(B1b.C2r) - z.C2 * tan(B1b.C1r)) / (tan(B1b.C2r) - tan(B1b.C1r))

# Calculating distance of the bird 1b
X.B1b<- (x.C2 * tan(B1b.C2r) - z.C2) / (tan(B1b.C2r) - tan(B1b.C1r))

# Completing the data frame
Birds.comp[2, "Side"] <-unique(C1$side)
Birds.comp[2, "File1"] <-paste(basename(dirname(C1.img1b)), basename(C1.img1b), sep="/")
Birds.comp[2, "File2"] <-paste(basename(dirname(C2.img1b)), basename(C2.img1b), sep="/")
Birds.comp[2, "Altitude"] <-Z.B1b
Birds.comp[2, "Distance"] <-X.B1b
Birds.comp[2, "Bin_ID"] <-S.data[S.data$ID==833,21]
Birds.comp[2, "Bin_altitude"] <-S.data[S.data$ID==833,20]
Birds.comp[2, "Bin_distance"] <-S.data[S.data$ID==833,19]
Birds.comp[2, "Bin_time"] <-S.data[S.data$ID==833,18] # time stamp = 09:00:38
Birds.comp[2, "Bin_angle"]<-S.data[S.data$ID==833,11]
Birds.comp[2, "Bird"]<-"1b"
Birds.comp[2, "Frame"]<-2

#Bird 1a ####
#Getting the paired images for Bird 1a
C1.img1a<- "Bresil_1_CAM1/2020-11-26/2020-11-26_09-00-23_Bresil-1_Cam1_record_2Det_7.avi/image_000011.png" # time stamp = 09:00:25
C2.img1a<- "Bresil_2_CAM2/2020-11-26/2020-11-26_09-00-35_Bresil-2_Cam2_record_2Det_9.avi/image_000012.png" # time stamp = 09:00:37

# Calculating altitude and distance for the bird 1b using paired images####
B1a.C1.pix<-168
B1a.C2.pix<-555

# Calculating the angle between the bird 1b and the top of the pylon
B1a.C1.angle<-C1$P_degree * B1a.C1.pix
B1a.C2.angle<-C2$P_degree * B1a.C2.pix

#complementary angle between the bird 1a and C1 at horizon
B1a.C1a <- B1a.C1.angle + C1$C_degree
B1a.C2a <- B1a.C2.angle + C2$C_degree

# Converting degrees into radians
B1a.C1r<-B1a.C1a * pi /180
B1a.C2r<-B1a.C2a * pi /180

# Calculating altitude of the bird 1a
Z.B1a<- (x.C2 * tan(B1a.C1r) * tan(B1a.C2r) - z.C2 * tan(B1a.C1r)) / (tan(B1a.C2r) - tan(B1a.C1r))

# Calculating distance of the bird 1a
X.B1a<- (x.C2 * tan(B1a.C2r) - z.C2) / (tan(B1a.C2r) - tan(B1a.C1r))

# Completing the data frame
Birds.comp[3, "Side"] <-unique(C1$side)
Birds.comp[3, "File1"] <-paste(basename(dirname(C1.img1a)), basename(C1.img1a), sep="/")
Birds.comp[3, "File2"] <-paste(basename(dirname(C2.img1a)), basename(C2.img1a), sep="/")
Birds.comp[3, "Altitude"] <-Z.B1a
Birds.comp[3, "Distance"] <-X.B1a
Birds.comp[3, "Bin_ID"] <-S.data[S.data$ID==833,21]
Birds.comp[3, "Bin_altitude"] <-S.data[S.data$ID==833,20]
Birds.comp[3, "Bin_distance"] <-S.data[S.data$ID==833,19]
Birds.comp[3, "Bin_time"] <-S.data[S.data$ID==833,18] # time stamp = 09:00:38
Birds.comp[3, "Bin_angle"]<-S.data[S.data$ID==833,11]
Birds.comp[3, "Bird"]<-"1a"
Birds.comp[3, "Frame"]<-3

##Bird 2 ####
Birds.comp[4, "Side"] <-unique(C1$side)

#Getting the paired images for Bird 2
C1.img2<- "Bresil_1_CAM1/2020-11-26/2020-11-26_09-02-43_Bresil-1_Cam1_record_1Det_9.avi/image_000006.png" # time stamp = 09:02:44
C2.img2<- "Bresil_2_CAM2/2020-11-26/2020-11-26_09-02-55_Bresil-2_Cam2_record_1Det_11.avi/image_000009.png" # time stamp = 09:02:56
Birds.comp[4, "File1"] <-paste(basename(dirname(C1.img2)), basename(C1.img2), sep="/")
Birds.comp[4, "File2"] <-paste(basename(dirname(C2.img2)), basename(C2.img2), sep="/")

# Calculating altitude and distance for the bird 2 using paired images####
B2.C1.pix<-368
B2.C2.pix<-888

# Calculating the angle between the bird 2 and the top of the pylon
B2.C1.angle<-C1$P_degree * B2.C1.pix
B2.C2.angle<-C2$P_degree * B2.C2.pix

#complementary angle between the bird 2 and C1 at horizon
B2.C1a <- B2.C1.angle + C1$C_degree
B2.C2a <- B2.C2.angle + C2$C_degree

# Converting degrees into radians
B2.C1r<-B2.C1a * pi /180
B2.C2r<-B2.C2a * pi /180

# Calculating altitude of the bird 2
Z.B2<- (x.C2 * tan(B2.C1r) * tan(B2.C2r) - z.C2 * tan(B2.C1r)) / (tan(B2.C2r) - tan(B2.C1r))

# Calculating distance of the bird 2
X.B2<- (x.C2 * tan(B2.C2r) - z.C2) / (tan(B2.C2r) - tan(B2.C1r))

Birds.comp[4, "Altitude"] <-Z.B2
Birds.comp[4, "Distance"] <-X.B2

# Completing the data frame
Birds.comp[4, "Bin_ID"] <-S.data[S.data$ID==834,21]
Birds.comp[4, "Bin_altitude"] <-S.data[S.data$ID==834,20]
Birds.comp[4, "Bin_distance"] <-S.data[S.data$ID==834,19]
Birds.comp[4, "Bin_time"] <-S.data[S.data$ID==834,18] # time stamp = 09:01:15
Birds.comp[4, "Bin_angle"]<-S.data[S.data$ID==834,11]
Birds.comp[4, "Bird"]<-"2"
Birds.comp[4, "Frame"]<-1

##Bird 2b ####
Birds.comp[5, "Side"] <-unique(C1$side)

#Getting the paired images for Bird 2b
C1.img2b<- "Bresil_1_CAM1/2020-11-26/2020-11-26_09-02-43_Bresil-1_Cam1_record_1Det_9.avi/image_000006.png" # time stamp = 09:02:44
C2.img2b<- "Bresil_2_CAM2/2020-11-26/2020-11-26_09-02-55_Bresil-2_Cam2_record_1Det_11.avi/image_000008.png" # time stamp = 09:02:56
Birds.comp[5, "File1"] <-paste(basename(dirname(C1.img2b)), basename(C1.img2b), sep="/")
Birds.comp[5, "File2"] <-paste(basename(dirname(C2.img2b)), basename(C2.img2b), sep="/")

# Calculating altitude and distance for the bird 2 using paired images####
B2b.C1.pix<-368
B2b.C2.pix<-865

# Calculating the angle between the bird 2b and the top of the pylon
B2b.C1.angle<-C1$P_degree * B2b.C1.pix
B2b.C2.angle<-C2$P_degree * B2b.C2.pix

#complementary angle between the bird 2b and C1 at horizon
B2b.C1a <- B2b.C1.angle + C1$C_degree
B2b.C2a <- B2b.C2.angle + C2$C_degree

# Converting degrees into radians
B2b.C1r<-B2b.C1a * pi /180
B2b.C2r<-B2b.C2a * pi /180

# Calculating altitude of the bird 2b
Z.B2b<- (x.C2 * tan(B2b.C1r) * tan(B2b.C2r) - z.C2 * tan(B2b.C1r)) / (tan(B2b.C2r) - tan(B2b.C1r))

# Calculating distance of the bird 2b
X.B2b<- (x.C2 * tan(B2b.C2r) - z.C2) / (tan(B2b.C2r) - tan(B2b.C1r))

Birds.comp[5, "Altitude"] <-Z.B2b
Birds.comp[5, "Distance"] <-X.B2b

# Completing the data frame
Birds.comp[5, "Bin_ID"] <-S.data[S.data$ID==834,21]
Birds.comp[5, "Bin_altitude"] <-S.data[S.data$ID==834,20]
Birds.comp[5, "Bin_distance"] <-S.data[S.data$ID==834,19]
Birds.comp[5, "Bin_time"] <-S.data[S.data$ID==834,18] # time stamp = 09:01:15
Birds.comp[5, "Bin_angle"]<-S.data[S.data$ID==834,11]
Birds.comp[5, "Bird"]<-"2b"
Birds.comp[5, "Frame"]<-2

##Bird 2a ####
Birds.comp[6, "Side"] <-unique(C1$side)

#Getting the paired images for Bird 2
C1.img2a<- "Bresil_1_CAM1/2020-11-26/2020-11-26_09-02-43_Bresil-1_Cam1_record_1Det_9.avi/image_000006.png" # time stamp = 09:02:44
C2.img2a<- "Bresil_2_CAM2/2020-11-26/2020-11-26_09-02-55_Bresil-2_Cam2_record_1Det_11.avi/image_0000010.png" # time stamp = 09:02:56
Birds.comp[6, "File1"] <-paste(basename(dirname(C1.img2a)), basename(C1.img2a), sep="/")
Birds.comp[6, "File2"] <-paste(basename(dirname(C2.img2a)), basename(C2.img2a), sep="/")

# Calculating altitude and distance for the bird 2 using paired images####
B2a.C1.pix<-368
B2a.C2.pix<-921

# Calculating the angle between the bird 2a and the top of the pylon
B2a.C1.angle<-C1$P_degree * B2a.C1.pix
B2a.C2.angle<-C2$P_degree * B2a.C2.pix

#complementary angle between the bird 2a and C1 at horizon
B2a.C1a <- B2a.C1.angle + C1$C_degree
B2a.C2a <- B2a.C2.angle + C2$C_degree

# Converting degrees into radians
B2a.C1r<-B2a.C1a * pi /180
B2a.C2r<-B2a.C2a * pi /180

# Calculating altitude of the bird 2a
Z.B2a<- (x.C2 * tan(B2a.C1r) * tan(B2a.C2r) - z.C2 * tan(B2a.C1r)) / (tan(B2a.C2r) - tan(B2a.C1r))

# Calculating distance of the bird 2a
X.B2a<- (x.C2 * tan(B2a.C2r) - z.C2) / (tan(B2a.C2r) - tan(B2a.C1r))

Birds.comp[6, "Altitude"] <-Z.B2a
Birds.comp[6, "Distance"] <-X.B2a

# Completing the data frame
Birds.comp[6, "Bin_ID"] <-S.data[S.data$ID==834,21]
Birds.comp[6, "Bin_altitude"] <-S.data[S.data$ID==834,20]
Birds.comp[6, "Bin_distance"] <-S.data[S.data$ID==834,19]
Birds.comp[6, "Bin_time"] <-S.data[S.data$ID==834,18] # time stamp = 09:01:15
Birds.comp[6, "Bin_angle"]<-S.data[S.data$ID==834,11]
Birds.comp[6, "Bird"]<-"2a"
Birds.comp[6, "Frame"]<-3

####
# Bird 3 removed due to lack of match with visual observation
####

##Bird 4 ####
Birds.comp[7, "Side"] <-unique(C1$side)

#Getting the paired images for Bird 4
C1.img4<- "Bresil_1_CAM1/2020-11-26/2020-11-26_10-04-10_Bresil-1_Cam1_record_2Det_17.avi/image_000006.png" # time stamp = 10:04:12
C2.img4<- "Bresil_2_CAM2/2020-11-26/2020-11-26_10-04-22_Bresil-2_Cam2_record_2Det_15.avi/image_000014.png" # time stamp = 10:04:24
Birds.comp[7, "File1"] <-paste(basename(dirname(C1.img4)), basename(C1.img4), sep="/")
Birds.comp[7, "File2"] <-paste(basename(dirname(C2.img4)), basename(C2.img4), sep="/")

# Calculating altitude and distance for the bird 4 using paired images####
B4.C1.pix<--50
B4.C2.pix<--52

# Calculating the angle between the bird and the top of the pylon
B4.C1.angle<-C1$P_degree * B4.C1.pix
B4.C2.angle<-C2$P_degree * B4.C2.pix

#complementary angle between the bird and C1 at horizon
B4.C1a <- B4.C1.angle + C1$C_degree
B4.C2a <- B4.C2.angle + C2$C_degree

# Converting degrees into radians
B4.C1r<-B4.C1a * pi /180
B4.C2r<-B4.C2a * pi /180

# Calculating altitude of the bird 4
Z.B4<- (x.C2 * tan(B4.C1r) * tan(B4.C2r) - z.C2 * tan(B4.C1r)) / (tan(B4.C2r) - tan(B4.C1r))

# Calculating distance of the bird 4
X.B4<- (x.C2 * tan(B4.C2r) - z.C2) / (tan(B4.C2r) - tan(B4.C1r))

Birds.comp[7, "Altitude"] <-Z.B4
Birds.comp[7, "Distance"] <-X.B4

# Completing the data frame
Birds.comp[7, "Bin_ID"] <-S.data[S.data$ID==844,21]
Birds.comp[7, "Bin_altitude"] <-S.data[S.data$ID==844,20]
Birds.comp[7, "Bin_distance"] <-S.data[S.data$ID==844,19]
Birds.comp[7, "Bin_time"] <-S.data[S.data$ID==844,18] # time stamp = 10:04:29
Birds.comp[7, "Bin_angle"]<-S.data[S.data$ID==844,11]
Birds.comp[7, "Bird"]<-"4"
Birds.comp[7, "Frame"]<-1

##Bird 4b ####
Birds.comp[8, "Side"] <-unique(C1$side)

#Getting the paired images for Bird 4b
C1.img4b<- "Bresil_1_CAM1/2020-11-26/2020-11-26_10-04-10_Bresil-1_Cam1_record_2Det_17.avi/image_000006.png" # time stamp = 10:04:12
C2.img4b<- "Bresil_2_CAM2/2020-11-26/2020-11-26_10-04-22_Bresil-2_Cam2_record_2Det_15.avi/image_000013.png" # time stamp = 10:04:24
Birds.comp[8, "File1"] <-paste(basename(dirname(C1.img4b)), basename(C1.img4b), sep="/")
Birds.comp[8, "File2"] <-paste(basename(dirname(C2.img4b)), basename(C2.img4b), sep="/")

# Calculating altitude and distance for the bird 4 using paired images####
B4b.C1.pix<--50
B4b.C2.pix<--37

# Calculating the angle between the bird and the top of the pylon
B4b.C1.angle<-C1$P_degree * B4b.C1.pix
B4b.C2.angle<-C2$P_degree * B4b.C2.pix

#complementary angle between the bird 4b and C1 at horizon
B4b.C1a <- B4b.C1.angle + C1$C_degree
B4b.C2a <- B4b.C2.angle + C2$C_degree

# Converting degrees into radians
B4b.C1r<-B4b.C1a * pi /180
B4b.C2r<-B4b.C2a * pi /180

# Calculating altitude of the bird 4b
Z.B4b<- (x.C2 * tan(B4b.C1r) * tan(B4b.C2r) - z.C2 * tan(B4b.C1r)) / (tan(B4b.C2r) - tan(B4b.C1r))

# Calculating distance of the bird 4b
X.B4b<- (x.C2 * tan(B4b.C2r) - z.C2) / (tan(B4b.C2r) - tan(B4b.C1r))

Birds.comp[8, "Altitude"] <-Z.B4b
Birds.comp[8, "Distance"] <-X.B4b

# Completing the data frame
Birds.comp[8, "Bin_ID"] <-S.data[S.data$ID==844,21]
Birds.comp[8, "Bin_altitude"] <-S.data[S.data$ID==844,20]
Birds.comp[8, "Bin_distance"] <-S.data[S.data$ID==844,19]
Birds.comp[8, "Bin_time"] <-S.data[S.data$ID==844,18] # time stamp = 10:04:29
Birds.comp[8, "Bin_angle"]<-S.data[S.data$ID==844,11]
Birds.comp[8, "Bird"]<-"4b"
Birds.comp[8, "Frame"]<-2

##Bird 4a ####
Birds.comp[9, "Side"] <-unique(C1$side)

#Getting the paired images for Bird 4a
C1.img4a<- "Bresil_1_CAM1/2020-11-26/2020-11-26_10-04-10_Bresil-1_Cam1_record_2Det_17.avi/image_000006.png" # time stamp = 10:04:12
C2.img4a<- "Bresil_2_CAM2/2020-11-26/2020-11-26_10-04-22_Bresil-2_Cam2_record_2Det_15.avi/image_000015.png" # time stamp = 10:04:24
Birds.comp[9, "File1"] <-paste(basename(dirname(C1.img4a)), basename(C1.img4a), sep="/")
Birds.comp[9, "File2"] <-paste(basename(dirname(C2.img4a)), basename(C2.img4a), sep="/")

# Calculating altitude and distance for the bird 4 using paired images####
B4a.C1.pix<--50
B4a.C2.pix<--69

# Calculating the angle between the bird 4a and the top of the pylon
B4a.C1.angle<-C1$P_degree * B4a.C1.pix
B4a.C2.angle<-C2$P_degree * B4a.C2.pix

#complementary angle between the bird 4a and C1 at horizon
B4a.C1a <- B4a.C1.angle + C1$C_degree
B4a.C2a <- B4a.C2.angle + C2$C_degree

# Converting degrees into radians
B4a.C1r<-B4a.C1a * pi /180
B4a.C2r<-B4a.C2a * pi /180

# Calculating altitude of the bird 4a
Z.B4a<- (x.C2 * tan(B4a.C1r) * tan(B4a.C2r) - z.C2 * tan(B4a.C1r)) / (tan(B4a.C2r) - tan(B4a.C1r))

# Calculating distance of the bird 4a
X.B4a<- (x.C2 * tan(B4a.C2r) - z.C2) / (tan(B4a.C2r) - tan(B4a.C1r))

Birds.comp[9, "Altitude"] <-Z.B4a
Birds.comp[9, "Distance"] <-X.B4a

# Completing the data frame
Birds.comp[9, "Bin_ID"] <-S.data[S.data$ID==844,21]
Birds.comp[9, "Bin_altitude"] <-S.data[S.data$ID==844,20]
Birds.comp[9, "Bin_distance"] <-S.data[S.data$ID==844,19]
Birds.comp[9, "Bin_time"] <-S.data[S.data$ID==844,18] # time stamp = 10:04:29
Birds.comp[9, "Bin_angle"]<-S.data[S.data$ID==844,11]
Birds.comp[9, "Bird"]<-"4a"
Birds.comp[9, "Frame"]<-3

##Bird 5 ####
Birds.comp[10, "Side"] <-unique(C1$side)

#Getting the paired images for Bird 5
C1.img5<- "Bresil_1_CAM1/2020-11-26/2020-11-26_15-25-28_Bresil-1_Cam1_record 11Det_26.avi/image_000050.png" # time stamp = 15:25:38
C2.img5<- "Bresil_2_CAM2/2020-11-26/2020-11-26_15-26-11_Bresil-2_Cam2_record_13Det_4.avi/image_000046.png" # time stamp = 15:26:20
Birds.comp[10, "File1"] <-paste(basename(dirname(C1.img5)), basename(C1.img5), sep="/")
Birds.comp[10, "File2"] <-paste(basename(dirname(C2.img5)), basename(C2.img5), sep="/")

# Calculating altitude and distance for the bird 5 using paired images####
B5.C1.pix<-747
B5.C2.pix<-867

# Calculating the angle between the bird 5 and the top of the pylon
B5.C1.angle<-C1$P_degree * B5.C1.pix
B5.C2.angle<-C2$P_degree * B5.C2.pix

#complementary angle between the bird 5 and C1 at horizon
B5.C1a <- B5.C1.angle + C1$C_degree
B5.C2a <- B5.C2.angle + C2$C_degree

# Converting degrees into radians
B5.C1r<-B5.C1a * pi /180
B5.C2r<-B5.C2a * pi /180

# Calculating altitude of the bird 5
Z.B5<- (x.C2 * tan(B5.C1r) * tan(B5.C2r) - z.C2 * tan(B5.C1r)) / (tan(B5.C2r) - tan(B5.C1r))

# Calculating distance of the bird 5
X.B5<- (x.C2 * tan(B5.C2r) - z.C2) / (tan(B5.C2r) - tan(B5.C1r))

Birds.comp[10, "Altitude"] <-Z.B5
Birds.comp[10, "Distance"] <-X.B5

# Completing the data frame
Birds.comp[10, "Bin_ID"] <-S.data[S.data$ID==847,21]
Birds.comp[10, "Bin_altitude"] <-S.data[S.data$ID==847,20]
Birds.comp[10, "Bin_distance"] <-S.data[S.data$ID==847,19]
Birds.comp[10, "Bin_time"] <-S.data[S.data$ID==847,18] # time stamp = 15:26:02
Birds.comp[10, "Bin_angle"]<-S.data[S.data$ID==847,11]
Birds.comp[10, "Bird"]<-"5"
Birds.comp[10, "Frame"]<-1

##Bird 5b ####
Birds.comp[11, "Side"] <-unique(C1$side)

#Getting the paired images for Bird 5b
C1.img5b<- "Bresil_1_CAM1/2020-11-26/2020-11-26_15-25-28_Bresil-1_Cam1_record 11Det_26.avi/image_000050.png" # time stamp = 15:25:38
C2.img5b<- "Bresil_2_CAM2/2020-11-26/2020-11-26_15-26-11_Bresil-2_Cam2_record_13Det_4.avi/image_000045.png" # time stamp = 15:26:20
Birds.comp[11, "File1"] <-paste(basename(dirname(C1.img5b)), basename(C1.img5b), sep="/")
Birds.comp[11, "File2"] <-paste(basename(dirname(C2.img5b)), basename(C2.img5b), sep="/")

# Calculating altitude and distance for the bird 5 using paired images####
B5b.C1.pix<-747
B5b.C2.pix<-885

# Calculating the angle between the bird 5b and the top of the pylon
B5b.C1.angle<-C1$P_degree * B5b.C1.pix
B5b.C2.angle<-C2$P_degree * B5b.C2.pix

#complementary angle between the bird 5b and C1 at horizon
B5b.C1a <- B5b.C1.angle + C1$C_degree
B5b.C2a <- B5b.C2.angle + C2$C_degree

# Converting degrees into radians
B5b.C1r<-B5b.C1a * pi /180
B5b.C2r<-B5b.C2a * pi /180

# Calculating altitude of the bird 5b
Z.B5b<- (x.C2 * tan(B5b.C1r) * tan(B5b.C2r) - z.C2 * tan(B5b.C1r)) / (tan(B5b.C2r) - tan(B5b.C1r))

# Calculating distance of the bird 5b
X.B5b<- (x.C2 * tan(B5b.C2r) - z.C2) / (tan(B5b.C2r) - tan(B5b.C1r))

Birds.comp[11, "Altitude"] <-Z.B5b
Birds.comp[11, "Distance"] <-X.B5b

# Completing the data frame
Birds.comp[11, "Bin_ID"] <-S.data[S.data$ID==847,21]
Birds.comp[11, "Bin_altitude"] <-S.data[S.data$ID==847,20]
Birds.comp[11, "Bin_distance"] <-S.data[S.data$ID==847,19]
Birds.comp[11, "Bin_time"] <-S.data[S.data$ID==847,18] # time stamp = 15:26:02
Birds.comp[11, "Bin_angle"]<-S.data[S.data$ID==847,11]
Birds.comp[11, "Bird"]<-"5b"
Birds.comp[11, "Frame"]<-2

##Bird 5a ####
Birds.comp[12, "Side"] <-unique(C1$side)

#Getting the paired images for Bird 5a
C1.img5a<- "Bresil_1_CAM1/2020-11-26/2020-11-26_15-25-28_Bresil-1_Cam1_record 11Det_26.avi/image_000050.png" # time stamp = 15:25:38
C2.img5a<- "Bresil_2_CAM2/2020-11-26/2020-11-26_15-26-11_Bresil-2_Cam2_record_13Det_4.avi/image_000047.png" # time stamp = 15:26:20
Birds.comp[12, "File1"] <-paste(basename(dirname(C1.img5a)), basename(C1.img5a), sep="/")
Birds.comp[12, "File2"] <-paste(basename(dirname(C2.img5a)), basename(C2.img5a), sep="/")

# Calculating altitude and distance for the bird 5 using paired images####
B5a.C1.pix<-747
B5a.C2.pix<-855

# Calculating the angle between the bird 5a and the top of the pylon
B5a.C1.angle<-C1$P_degree * B5a.C1.pix
B5a.C2.angle<-C2$P_degree * B5a.C2.pix

#complementary angle between the bird 5a and C1 at horizon
B5a.C1a <- B5a.C1.angle + C1$C_degree
B5a.C2a <- B5a.C2.angle + C2$C_degree

# Converting degrees into radians
B5a.C1r<-B5a.C1a * pi /180
B5a.C2r<-B5a.C2a * pi /180

# Calculating altitude of the bird 5a
Z.B5a<- (x.C2 * tan(B5a.C1r) * tan(B5a.C2r) - z.C2 * tan(B5a.C1r)) / (tan(B5a.C2r) - tan(B5a.C1r))

# Calculating distance of the bird 5a
X.B5a<- (x.C2 * tan(B5a.C2r) - z.C2) / (tan(B5a.C2r) - tan(B5a.C1r))

Birds.comp[12, "Altitude"] <-Z.B5a
Birds.comp[12, "Distance"] <-X.B5a

# Completing the data frame
Birds.comp[12, "Bin_ID"] <-S.data[S.data$ID==847,21]
Birds.comp[12, "Bin_altitude"] <-S.data[S.data$ID==847,20]
Birds.comp[12, "Bin_distance"] <-S.data[S.data$ID==847,19]
Birds.comp[12, "Bin_time"] <-S.data[S.data$ID==847,18] # time stamp = 15:26:02
Birds.comp[12, "Bin_angle"]<-S.data[S.data$ID==847,11]
Birds.comp[12, "Bird"]<-"5a"
Birds.comp[12, "Frame"]<-3

##Bird 6 ####
Birds.comp[13, "Side"] <-unique(C1$side)

#Getting the paired images for Bird 6
C1.img6<- "Bresil_1_CAM1/2020-11-26/2020-11-26_15-27-25_Bresil-1_Cam1_record 1Det_27.avi/image_000054.png" # time stamp = 15:27:35
C2.img6<- "Bresil_2_CAM2/2020-11-26/2020-11-26_15-28-14_Bresil-2_Cam2_record_1Det_5.avi/image_000013.png" # time stamp = 15:28:17
Birds.comp[13, "File1"] <-paste(basename(dirname(C1.img6)), basename(C1.img6), sep="/")
Birds.comp[13, "File2"] <-paste(basename(dirname(C2.img6)), basename(C2.img6), sep="/")

# Calculating altitude and distance for the bird 6 using paired images####
B6.C1.pix<-590
B6.C2.pix<-1026

# Calculating the angle between the bird 6 and the top of the pylon
B6.C1.angle<-C1$P_degree * B6.C1.pix
B6.C2.angle<-C2$P_degree * B6.C2.pix

#complementary angle between the bird 6 and C1 at horizon
B6.C1a <- B6.C1.angle + C1$C_degree
B6.C2a <- B6.C2.angle + C2$C_degree

# Converting degrees into radians
B6.C1r<-B6.C1a * pi /180
B6.C2r<-B6.C2a * pi /180

# Calculating altitude of the bird 6
Z.B6<- (x.C2 * tan(B6.C1r) * tan(B6.C2r) - z.C2 * tan(B6.C1r)) / (tan(B6.C2r) - tan(B6.C1r))

# Calculating distance of the bird 6
X.B6<- (x.C2 * tan(B6.C2r) - z.C2) / (tan(B6.C2r) - tan(B6.C1r))

Birds.comp[13, "Altitude"] <-Z.B6
Birds.comp[13, "Distance"] <-X.B6

# Completing the data frame
Birds.comp[13, "Bin_ID"] <-S.data[S.data$ID==850,21]
Birds.comp[13, "Bin_altitude"] <-S.data[S.data$ID==850,20]
Birds.comp[13, "Bin_distance"] <-S.data[S.data$ID==850,19]
Birds.comp[13, "Bin_time"] <-S.data[S.data$ID==850,18] # time stamp = 15:28:23
Birds.comp[13, "Bin_angle"]<-S.data[S.data$ID==850,11]
Birds.comp[13, "Bird"]<-"6"
Birds.comp[13, "Frame"]<-1

##Bird 6b ####
Birds.comp[14, "Side"] <-unique(C1$side)

#Getting the paired images for Bird 6b
C1.img6b<- "Bresil_1_CAM1/2020-11-26/2020-11-26_15-27-25_Bresil-1_Cam1_record 1Det_27.avi/image_000054.png" # time stamp = 15:27:35
C2.img6b<- "Bresil_2_CAM2/2020-11-26/2020-11-26_15-28-14_Bresil-2_Cam2_record_1Det_5.avi/image_000012.png" # time stamp = 15:28:17
Birds.comp[14, "File1"] <-paste(basename(dirname(C1.img6b)), basename(C1.img6b), sep="/")
Birds.comp[14, "File2"] <-paste(basename(dirname(C2.img6b)), basename(C2.img6b), sep="/")

# Calculating altitude and distance for the bird 6 using paired images####
B6b.C1.pix<-590
B6b.C2.pix<-1012

# Calculating the angle between the bird 6b and the top of the pylon
B6b.C1.angle<-C1$P_degree * B6b.C1.pix
B6b.C2.angle<-C2$P_degree * B6b.C2.pix

#complementary angle between the bird 6b and C1 at horizon
B6b.C1a <- B6b.C1.angle + C1$C_degree
B6b.C2a <- B6b.C2.angle + C2$C_degree

# Converting degrees into radians
B6b.C1r<-B6b.C1a * pi /180
B6b.C2r<-B6b.C2a * pi /180

# Calculating altitude of the bird 6b
Z.B6b<- (x.C2 * tan(B6b.C1r) * tan(B6b.C2r) - z.C2 * tan(B6b.C1r)) / (tan(B6b.C2r) - tan(B6b.C1r))

# Calculating distance of the bird 6b
X.B6b<- (x.C2 * tan(B6b.C2r) - z.C2) / (tan(B6b.C2r) - tan(B6b.C1r))

Birds.comp[14, "Altitude"] <-Z.B6b
Birds.comp[14, "Distance"] <-X.B6b

# Completing the data frame
Birds.comp[14, "Bin_ID"] <-S.data[S.data$ID==850,21]
Birds.comp[14, "Bin_altitude"] <-S.data[S.data$ID==850,20]
Birds.comp[14, "Bin_distance"] <-S.data[S.data$ID==850,19]
Birds.comp[14, "Bin_time"] <-S.data[S.data$ID==850,18] # time stamp = 15:28:23
Birds.comp[14, "Bin_angle"]<-S.data[S.data$ID==850,11]
Birds.comp[14, "Bird"]<-"6b"
Birds.comp[14, "Frame"]<-2

##Bird 6a ####
Birds.comp[15, "Side"] <-unique(C1$side)

#Getting the paired images for Bird 6a
C1.img6a<- "Bresil_1_CAM1/2020-11-26/2020-11-26_15-27-25_Bresil-1_Cam1_record 1Det_27.avi/image_000054.png" # time stamp = 15:27:35
C2.img6a<- "Bresil_2_CAM2/2020-11-26/2020-11-26_15-28-14_Bresil-2_Cam2_record_1Det_5.avi/image_000014.png" # time stamp = 15:28:17
Birds.comp[15, "File1"] <-paste(basename(dirname(C1.img6a)), basename(C1.img6a), sep="/")
Birds.comp[15, "File2"] <-paste(basename(dirname(C2.img6a)), basename(C2.img6a), sep="/")

# Calculating altitude and distance for the bird 6 using paired images####
B6a.C1.pix<-590
B6a.C2.pix<-1041

# Calculating the angle between the bird 6a and the top of the pylon
B6a.C1.angle<-C1$P_degree * B6a.C1.pix
B6a.C2.angle<-C2$P_degree * B6a.C2.pix

#complementary angle between the bird 6a and C1 at horizon
B6a.C1a <- B6a.C1.angle + C1$C_degree
B6a.C2a <- B6a.C2.angle + C2$C_degree

# Converting degrees into radians
B6a.C1r<-B6a.C1a * pi /180
B6a.C2r<-B6a.C2a * pi /180

# Calculating altitude of the bird 6a
Z.B6a<- (x.C2 * tan(B6a.C1r) * tan(B6a.C2r) - z.C2 * tan(B6a.C1r)) / (tan(B6a.C2r) - tan(B6a.C1r))

# Calculating distance of the bird 6a
X.B6a<- (x.C2 * tan(B6a.C2r) - z.C2) / (tan(B6a.C2r) - tan(B6a.C1r))

Birds.comp[15, "Altitude"] <-Z.B6a
Birds.comp[15, "Distance"] <-X.B6a

# Completing the data frame
Birds.comp[15, "Bin_ID"] <-S.data[S.data$ID==850,21]
Birds.comp[15, "Bin_altitude"] <-S.data[S.data$ID==850,20]
Birds.comp[15, "Bin_distance"] <-S.data[S.data$ID==850,19]
Birds.comp[15, "Bin_time"] <-S.data[S.data$ID==850,18] # time stamp = 15:28:23
Birds.comp[15, "Bin_angle"]<-S.data[S.data$ID==850,11]
Birds.comp[15, "Bird"]<-"6a"
Birds.comp[15, "Frame"]<-3

##Bird 7 ####
Birds.comp[16, "Side"] <-unique(C1$side)

#Getting the paired images for Bird 7
C1.img7<- "Bresil_1_CAM1/2020-11-26/2020-11-26_15-28-10_Bresil-1_Cam1_record 2Det_28.avi/image_000054.png" # time stamp = 15:28:20
C2.img7<- "Bresil_2_CAM2/2020-11-26/2020-11-26_15-28-52_Bresil-2_Cam2_record_1Det_6.avi/image_000049.png" # time stamp = 15:29:02
Birds.comp[16, "File1"] <-paste(basename(dirname(C1.img7)), basename(C1.img7), sep="/")
Birds.comp[16, "File2"] <-paste(basename(dirname(C2.img7)), basename(C2.img7), sep="/")

# Calculating altitude and distance for the bird 7 using paired images####
B7.C1.pix<-302
B7.C2.pix<-418

# Calculating the angle between the bird 7 and the top of the pylon
B7.C1.angle<-C1$P_degree * B7.C1.pix
B7.C2.angle<-C2$P_degree * B7.C2.pix

#complementary angle between the bird 7 and C1 at horizon
B7.C1a <- B7.C1.angle + C1$C_degree
B7.C2a <- B7.C2.angle + C2$C_degree

# Converting degrees into radians
B7.C1r<-B7.C1a * pi /180
B7.C2r<-B7.C2a * pi /180

# Calculating altitude of the bird 7
Z.B7<- (x.C2 * tan(B7.C1r) * tan(B7.C2r) - z.C2 * tan(B7.C1r)) / (tan(B7.C2r) - tan(B7.C1r))

# Calculating distance of the bird 7
X.B7<- (x.C2 * tan(B7.C2r) - z.C2) / (tan(B7.C2r) - tan(B7.C1r))

Birds.comp[16, "Altitude"] <-Z.B7
Birds.comp[16, "Distance"] <-X.B7

# Completing the data frame
Birds.comp[16, "Bin_ID"] <-S.data[S.data$ID==851,21]
Birds.comp[16, "Bin_distance"] <-S.data[S.data$ID==851,19]
Birds.comp[16, "Bin_altitude"] <-S.data[S.data$ID==851,20]
Birds.comp[16, "Bin_time"] <-S.data[S.data$ID==851,18] # time stamp = 15:29:46
Birds.comp[16, "Bin_angle"]<-S.data[S.data$ID==851,11]
Birds.comp[16, "Bird"]<-"7"
Birds.comp[16, "Frame"]<-1

##Bird 7b ####
Birds.comp[17, "Side"] <-unique(C1$side)

#Getting the paired images for Bird 7b
C1.img7b<- "Bresil_1_CAM1/2020-11-26/2020-11-26_15-28-10_Bresil-1_Cam1_record 2Det_28.avi/image_000054.png" # time stamp = 15:28:20
C2.img7b<- "Bresil_2_CAM2/2020-11-26/2020-11-26_15-28-52_Bresil-2_Cam2_record_1Det_6.avi/image_000048.png" # time stamp = 15:29:02
Birds.comp[17, "File1"] <-paste(basename(dirname(C1.img7b)), basename(C1.img7b), sep="/")
Birds.comp[17, "File2"] <-paste(basename(dirname(C2.img7b)), basename(C2.img7b), sep="/")

# Calculating altitude and distance for the bird 7 using paired images####
B7b.C1.pix<-302
B7b.C2.pix<-423

# Calculating the angle between the bird 7b and the top of the pylon
B7b.C1.angle<-C1$P_degree * B7b.C1.pix
B7b.C2.angle<-C2$P_degree * B7b.C2.pix

#complementary angle between the bird 7b and C1 at horizon
B7b.C1a <- B7b.C1.angle + C1$C_degree
B7b.C2a <- B7b.C2.angle + C2$C_degree

# Converting degrees into radians
B7b.C1r<-B7b.C1a * pi /180
B7b.C2r<-B7b.C2a * pi /180

# Calculating altitude of the bird 7b
Z.B7b<- (x.C2 * tan(B7b.C1r) * tan(B7b.C2r) - z.C2 * tan(B7b.C1r)) / (tan(B7b.C2r) - tan(B7b.C1r))

# Calculating distance of the bird 7b
X.B7b<- (x.C2 * tan(B7b.C2r) - z.C2) / (tan(B7b.C2r) - tan(B7b.C1r))

Birds.comp[17, "Altitude"] <-Z.B7b
Birds.comp[17, "Distance"] <-X.B7b

# Completing the data frame
Birds.comp[17, "Bin_ID"] <-S.data[S.data$ID==851,21]
Birds.comp[17, "Bin_distance"] <-S.data[S.data$ID==851,19]
Birds.comp[17, "Bin_altitude"] <-S.data[S.data$ID==851,20]
Birds.comp[17, "Bin_time"] <-S.data[S.data$ID==851,18] # time stamp = 15:29:46
Birds.comp[17, "Bin_angle"]<-S.data[S.data$ID==851,11]
Birds.comp[17, "Bird"]<-"7b"
Birds.comp[17, "Frame"]<-2

##Bird 7a ####
Birds.comp[18, "Side"] <-unique(C1$side)

#Getting the paired images for Bird 7a
C1.img7a<- "Bresil_1_CAM1/2020-11-26/2020-11-26_15-28-10_Bresil-1_Cam1_record 2Det_28.avi/image_000054.png" # time stamp = 15:28:20
C2.img7a<- "Bresil_2_CAM2/2020-11-26/2020-11-26_15-28-52_Bresil-2_Cam2_record_1Det_6.avi/image_000050.png" # time stamp = 15:29:02
Birds.comp[18, "File1"] <-paste(basename(dirname(C1.img7a)), basename(C1.img7a), sep="/")
Birds.comp[18, "File2"] <-paste(basename(dirname(C2.img7a)), basename(C2.img7a), sep="/")

# Calculating altitude and distance for the bird 7 using paired images####
B7a.C1.pix<-302
B7a.C2.pix<-424

# Calculating the angle between the bird 7a and the top of the pylon
B7a.C1.angle<-C1$P_degree * B7a.C1.pix
B7a.C2.angle<-C2$P_degree * B7a.C2.pix

#complementary angle between the bird 7a and C1 at horizon
B7a.C1a <- B7a.C1.angle + C1$C_degree
B7a.C2a <- B7a.C2.angle + C2$C_degree

# Converting degrees into radians
B7a.C1r<-B7a.C1a * pi /180
B7a.C2r<-B7a.C2a * pi /180

# Calculating altitude of the bird 7a
Z.B7a<- (x.C2 * tan(B7a.C1r) * tan(B7a.C2r) - z.C2 * tan(B7a.C1r)) / (tan(B7a.C2r) - tan(B7a.C1r))

# Calculating distance of the bird 7a
X.B7a<- (x.C2 * tan(B7a.C2r) - z.C2) / (tan(B7a.C2r) - tan(B7a.C1r))

Birds.comp[18, "Altitude"] <-Z.B7a
Birds.comp[18, "Distance"] <-X.B7a

# Completing the data frame
Birds.comp[18, "Bin_ID"] <-S.data[S.data$ID==851,21]
Birds.comp[18, "Bin_distance"] <-S.data[S.data$ID==851,19]
Birds.comp[18, "Bin_altitude"] <-S.data[S.data$ID==851,20]
Birds.comp[18, "Bin_time"] <-S.data[S.data$ID==851,18] # time stamp = 15:29:46
Birds.comp[18, "Bin_angle"]<-S.data[S.data$ID==851,11]
Birds.comp[18, "Bird"]<-"7a"
Birds.comp[18, "Frame"]<-3

##Bird 8 ####
Birds.comp[19, "Side"] <-unique(C1$side)

#Getting the paired images for Bird 8
C1.img8<- "Bresil_1_CAM1/2020-11-26/2020-11-26_16-03-26_Bresil-1_Cam1_record 5Det_38.avi/image_000011.png" # time stamp = 16:03:28
C2.img8<- "Bresil_2_CAM2/2020-11-26/2020-11-26_16-04-03_Bresil-2_Cam2_record_1Det_13.avi/image_000032.png" # time stamp = 16:04:10
Birds.comp[19, "File1"] <-paste(basename(dirname(C1.img8)), basename(C1.img8), sep="/")
Birds.comp[19, "File2"] <-paste(basename(dirname(C2.img8)), basename(C2.img8), sep="/")

# Calculating altitude and distance for the bird 8 using paired images####
B8.C1.pix<-907
B8.C2.pix<-1096

# Calculating the angle between the bird 8 and the top of the pylon
B8.C1.angle<-C1$P_degree * B8.C1.pix
B8.C2.angle<-C2$P_degree * B8.C2.pix

#complementary angle between the bird 8 and C1 at horizon
B8.C1a <- B8.C1.angle + C1$C_degree
B8.C2a <- B8.C2.angle + C2$C_degree

# Converting degrees into radians
B8.C1r<-B8.C1a * pi /180
B8.C2r<-B8.C2a * pi /180

# Calculating altitude of the bird 8
Z.B8<- (x.C2 * tan(B8.C1r) * tan(B8.C2r) - z.C2 * tan(B8.C1r)) / (tan(B8.C2r) - tan(B8.C1r))

# Calculating distance of the bird 8
X.B8<- (x.C2 * tan(B8.C2r) - z.C2) / (tan(B8.C2r) - tan(B8.C1r))

Birds.comp[19, "Altitude"] <-Z.B8
Birds.comp[19, "Distance"] <-X.B8

# Completing the data frame
Birds.comp[19, "Bin_ID"] <-S.data[S.data$ID==852,21]
Birds.comp[19, "Bin_altitude"] <-S.data[S.data$ID==852,20]
Birds.comp[19, "Bin_distance"] <-S.data[S.data$ID==852,19]
Birds.comp[19, "Bin_time"] <-S.data[S.data$ID==852,18] # time stamp = 16:02:44
Birds.comp[19, "Bin_angle"]<-S.data[S.data$ID==852,11]
Birds.comp[19, "Bird"]<-"8"
Birds.comp[19, "Frame"]<-1

##Bird 8b ####
Birds.comp[20, "Side"] <-unique(C1$side)

#Getting the paired images for Bird 8b
C1.img8b<- "Bresil_1_CAM1/2020-11-26/2020-11-26_16-03-26_Bresil-1_Cam1_record 5Det_38.avi/image_000011.png" # time stamp = 16:03:28
C2.img8b<- "Bresil_2_CAM2/2020-11-26/2020-11-26_16-04-03_Bresil-2_Cam2_record_1Det_13.avi/image_000031.png" # time stamp = 16:04:10
Birds.comp[20, "File1"] <-paste(basename(dirname(C1.img8b)), basename(C1.img8b), sep="/")
Birds.comp[20, "File2"] <-paste(basename(dirname(C2.img8b)), basename(C2.img8b), sep="/")

# Calculating altitude and distance for the bird 8 using paired images####
B8b.C1.pix<-907
B8b.C2.pix<-1110

# Calculating the angle between the bird 8b and the top of the pylon
B8b.C1.angle<-C1$P_degree * B8b.C1.pix
B8b.C2.angle<-C2$P_degree * B8b.C2.pix

#complementary angle between the bird 8b and C1 at horizon
B8b.C1a <- B8b.C1.angle + C1$C_degree
B8b.C2a <- B8b.C2.angle + C2$C_degree

# Converting degrees into radians
B8b.C1r<-B8b.C1a * pi /180
B8b.C2r<-B8b.C2a * pi /180

# Calculating altitude of the bird 8b
Z.B8b<- (x.C2 * tan(B8b.C1r) * tan(B8b.C2r) - z.C2 * tan(B8b.C1r)) / (tan(B8b.C2r) - tan(B8b.C1r))

# Calculating distance of the bird 8b
X.B8b<- (x.C2 * tan(B8b.C2r) - z.C2) / (tan(B8b.C2r) - tan(B8b.C1r))

Birds.comp[20, "Altitude"] <-Z.B8b
Birds.comp[20, "Distance"] <-X.B8b

# Completing the data frame
Birds.comp[20, "Bin_ID"] <-S.data[S.data$ID==852,21]
Birds.comp[20, "Bin_altitude"] <-S.data[S.data$ID==852,20]
Birds.comp[20, "Bin_distance"] <-S.data[S.data$ID==852,19]
Birds.comp[20, "Bin_time"] <-S.data[S.data$ID==852,18] # time stamp = 16:02:44
Birds.comp[20, "Bin_angle"]<-S.data[S.data$ID==852,11]
Birds.comp[20, "Bird"]<-"8b"
Birds.comp[20, "Frame"]<-2

##Bird 8a ####
Birds.comp[21, "Side"] <-unique(C1$side)

#Getting the paired images for Bird 8a
C1.img8a<- "Bresil_1_CAM1/2020-11-26/2020-11-26_16-03-26_Bresil-1_Cam1_record 5Det_38.avi/image_000011.png" # time stamp = 16:03:28
C2.img8a<- "Bresil_2_CAM2/2020-11-26/2020-11-26_16-04-03_Bresil-2_Cam2_record_1Det_13.avi/image_000033.png" # time stamp = 16:04:10
Birds.comp[21, "File1"] <-paste(basename(dirname(C1.img8a)), basename(C1.img8a), sep="/")
Birds.comp[21, "File2"] <-paste(basename(dirname(C2.img8a)), basename(C2.img8a), sep="/")

# Calculating altitude and distance for the bird 8 using paired images####
B8a.C1.pix<-907
B8a.C2.pix<-1112

# Calculating the angle between the bird 8a and the top of the pylon
B8a.C1.angle<-C1$P_degree * B8a.C1.pix
B8a.C2.angle<-C2$P_degree * B8a.C2.pix

#complementary angle between the bird 8a and C1 at horizon
B8a.C1a <- B8a.C1.angle + C1$C_degree
B8a.C2a <- B8a.C2.angle + C2$C_degree

# Converting degrees into radians
B8a.C1r<-B8a.C1a * pi /180
B8a.C2r<-B8a.C2a * pi /180

# Calculating altitude of the bird 8a
Z.B8a<- (x.C2 * tan(B8a.C1r) * tan(B8a.C2r) - z.C2 * tan(B8a.C1r)) / (tan(B8a.C2r) - tan(B8a.C1r))

# Calculating distance of the bird 8a
X.B8a<- (x.C2 * tan(B8a.C2r) - z.C2) / (tan(B8a.C2r) - tan(B8a.C1r))

Birds.comp[21, "Altitude"] <-Z.B8a
Birds.comp[21, "Distance"] <-X.B8a

# Completing the data frame
Birds.comp[21, "Bin_ID"] <-S.data[S.data$ID==852,1]
Birds.comp[21, "Bin_altitude"] <-S.data[S.data$ID==852,20]
Birds.comp[21, "Bin_distance"] <-S.data[S.data$ID==852,19]
Birds.comp[21, "Bin_time"] <-S.data[S.data$ID==852,18] # time stamp = 16:02:44
Birds.comp[21, "Bin_angle"]<-S.data[S.data$ID==852,11]
Birds.comp[21, "Bird"]<-"8a"
Birds.comp[21, "Frame"]<-3

write.csv(Birds.comp, "BirdsB1C1_B2C2_comp.csv")

plot(Birds.comp$Distance, Birds.comp$Bin_distance)

library(ggplot2)

#Calculating the difference between CAM altitude and Bin altitude
Birds.comp$Dif<- Birds.comp$Altitude - Birds.comp$Bin_altitude

#different colors for each frame, size of the difference between methods
ggplot(Birds.comp, aes(x= Altitude , y= Bin_altitude)) + 
   geom_point(aes(colour= factor(Frame), size= Dif)) +
   scale_size(breaks = c(1, 10, 20, 30, 40, 50),  range = c(0,50)) +
   ylim(0, 300) +
   xlim(0, 300) +
   geom_abline(intercept= 0, slope= 1)

Birds.comp.ed<-Birds.comp[c(1:8, 10:21),]

#different colours and shape for each frame
ggplot(Birds.comp.ed, aes(x= Altitude , y= Bin_altitude)) + 
  geom_point(aes(colour= factor(Frame), shape=factor(Frame)),size=4) +
  ylim(0, 300) +
  xlim(0, 300) +
  geom_abline(intercept= 0, slope= 1)

#same shape but different colours set manually for each frame
ggplot(Birds.comp.ed, aes(x= Altitude , y= Bin_altitude)) + 
  geom_point(colour="white", shape=21, size = 5,
             aes(fill=factor(Frame))) +
  scale_shape(solid = FALSE) +
  scale_fill_manual(values=c("blue", "cyan4", "magenta")) +
  ylim(0, 300) +
  xlim(0, 300) +
  geom_abline(intercept= 0, slope= 1)

#same shape, no fill, different colours set manually for each frame
ggplot(Birds.comp.ed, aes(x= Altitude , y= Bin_altitude)) + 
  geom_point(aes(colour=factor(Frame), 
                 ), shape=24, size = 6) + 
  scale_colour_manual(values=c("orange", "blue", "magenta")) +
  ylim(0, 300) +
  xlim(0, 300) +
  geom_abline(intercept= 0, slope= 1)

#Preferable#
#different shapes, no fill, different colours set manually for each frame
ggplot(Birds.comp.ed, aes(x= Altitude , y= Bin_altitude)) + 
  geom_point(aes(colour=factor(Frame), shape=factor(Frame)), size = 6) + 
  scale_colour_manual(values=c("magenta", "blue", "yellow3")) +
  scale_shape_manual(values=c(22, 23, 24))+
  ylim(0, 300) +
  xlim(0, 300) +
  geom_abline(intercept= 0, slope= 1) +
  labs(y="Binoculars altitude (m)", x = "Cameras altitude (m)")
  
#Plotting the parametric space and the points calculated for the birds position
#Cam 1 and 2 = 103 degrees of field of view
alt.prec_4<- function(x.cam2= 28.9, z.cam2= -1.92, x.target, z.target, d.theta1= 0.05, d.theta2= 0.05, nsim= 10000){
  ld<- length(x.target)
  
  theta1.TRUE.rad<- atan(z.target/x.target)
  theta2.TRUE.rad<- atan((z.target-z.cam2)/(x.target-x.cam2))
  theta1.TRUE.deg<- theta1.TRUE.rad * 180 / pi
  theta2.TRUE.deg<- theta2.TRUE.rad * 180 / pi
  
  d.z<- matrix(NA, ld, nsim)
  d.x<- matrix(NA, ld, nsim)
  
  for(i in 1:nsim){
    theta1.OBS.rad<- (theta1.TRUE.deg + runif(ld, -d.theta1, d.theta1)) * pi / 180
    theta2.OBS.rad<- (theta2.TRUE.deg + runif(ld, -d.theta2, d.theta2)) * pi / 180
    
    d.z[, i]<- z.target - (x.cam2 * tan(theta1.OBS.rad) * tan(theta2.OBS.rad) - z.cam2 * tan(theta1.OBS.rad)) / (tan(theta2.OBS.rad) - tan(theta1.OBS.rad))
    d.x[, i]<- x.target - (x.cam2 * tan(theta2.OBS.rad) - z.cam2) / (tan(theta2.OBS.rad) - tan(theta1.OBS.rad))
  }
  
  data.frame(x.target, z.target, d.z= apply(d.z, 1, sd), d.x= apply(d.x, 1, sd))
}

# create study area 200m x 350m
Nz<- 200
Nx<- 350
area.grid<- expand.grid(list(x= 0:Nx, z= 0:Nz))#0:Nz

# run the simulation with x.cam2= 20, z.cam2= 0, and measurement error of 0.05 decimal degree
# test_54<- alt.prec_2(x.target= area.grid$x, z.target= area.grid$z, d.theta1= 0.05, d.theta2= 0.05, x.cam2= 50, z.cam2= 0)

# run the simulation with x.cam2= 28.9, z.cam2= -1.92 and measurement error of 0.05 decimal degree
test_103L<- alt.prec_4(x.target= area.grid$x, z.target= area.grid$z, d.theta1= 0.05, d.theta2= 0.05, x.cam2= 28.9, z.cam2= -1.92, nsim= 1000)
test_103L$ln.d.x<- round(log(abs(test_103L$d.x)), 2)
test_103L$ln.d.z<- round(log(abs(test_103L$d.z)), 2)

test_103L$ln.d.z[is.na(test_103L$ln.d.z)]<- 0
test_103L$ln.d.x[is.na(test_103L$ln.d.x)]<- 0

gz.1<- ggplot(test_103L, aes(x.target, z.target)) +
  geom_raster(aes(fill = ln.d.z)) +
  geom_contour(colour = "white", aes(z= ln.d.z), bins= 6, alpha= 0.5) +
  ggtitle("Camera 1 and 2 = 103 degrees")

gx.1<- ggplot(test_103L, aes(x.target, z.target)) +
  geom_raster(aes(fill = ln.d.x)) +
  geom_contour(colour = "white", aes(z= ln.d.x), bins= 18, alpha= 0.5) #+

Birds.triang<-Birds.comp[,c(1, 4, 5, 6, 11, 12)]
Birds.triang$x.target<-Birds.triang$Distance
Birds.triang$z.target<-Birds.triang$Altitude

#Removing the outlier in line 9
Birds.triang<-Birds.triang[c(1:8, 10:21),]

# Adding the altitude and distance for each frame
Fr<-gz.1 + geom_point(data= Birds.triang, aes(x.target, z.target, colour=factor(Frame), shape=factor(Frame)), size=4) +#, fill=factor(Frame), size=3)) #+
    scale_shape_manual(values=c(21,22,24, 25))+
  scale_colour_manual(values=c("magenta", "blue", "yellow3", "green3")) +
  geom_line(data= Birds.triang, aes(group=Bin_ID))+
 #geom_segment(aes(x=Altitude, y=Distance , xend=x.target, yend=z.target, group=Bin_ID), data=Birds.triang) + 
 labs(y="Altitude (m)", x = "Distance (m)")

#Adding the binocular data
Birds.bin<-Birds.comp[,c(1, 6, 7, 8, 11, 12)]
Birds.bin$x.target<-Birds.bin$Bin_distance
Birds.bin$z.target<-Birds.bin$Bin_altitude

Fr + geom_point(data= Birds.bin, aes(x.target, z.target, colour="Binoculars", 
                                     shape="Binoculars"), size=4) #+ #, colour="red", shape=25, size=3)) 
     #geom_line(data= Birds.bin, aes(group=Bin_ID))
     # geom_segment(data= Birds.bin, aes(x =Bin_distance , y = Bin_altitude ,
     #                                    xend = z.target, yend =x.target , group=Bin_ID))

#Combining Bin and camera data # using this graph
library("plyr")
Birds.bin$Frame<-"Binoculars"
CB_data<-rbind.fill(Birds.triang[,c(1,4:8)], Birds.bin[,c(1,2, 5:8)])

gz.1 + geom_point(data= CB_data, aes(x.target, z.target, colour=factor(Frame), shape=factor(Frame)), size=4) +#, fill=factor(Frame), size=3)) #+
  scale_shape_manual(values=c(21,22,24, 25))+
  scale_colour_manual(values=c("magenta", "blue", "yellow3", "green3")) +
  geom_line(data= CB_data, aes(group=Bin_ID))+
  #geom_text(label=rownames(CB_data)) +
  #geom_label(data=CB_data, aes(label=rownames(CB_data))) +
  labs(y="Altitude (m)", x = "Distance (m)")
  






