library(ggplot2)
library(gridExtra)
library(colorspace)

# Stochastic version of the error calculation, assuming uniform error between min and max, and returning SD of error
#For two cameras with 103 degrees of field of view - 2.8mm
#Left side - Cam 1 is higher than Cam 2
alt.prec_1<- function(x.cam2= 28.9, z.cam2= -1.92, x.target, z.target, d.theta1= 0.05, d.theta2= 0.05, nsim= 10000){
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

# sensor type 1080 p
1920*1080 # pixels

# angle of view per pixel ("measurement error")
103/1920 # [1] 0.05364583
54/1920 # [1] 0.028125

Nz<- 150
Nx<- 300

# create study area 150m x 300m
area.grid<- expand.grid(list(x= 0:Nx, z= 0:Nz))#0:Nz

# run the simulation with x.cam2= 20, z.cam2= 0, and measurement error of 0.05 decimal degree
# test_54<- alt.prec_2(x.target= area.grid$x, z.target= area.grid$z, d.theta1= 0.05, d.theta2= 0.05, x.cam2= 50, z.cam2= 0)

# run the simulation with x.cam2= 28.9, z.cam2= -1.92 and measurement error of 0.05 decimal degree
test_103L<- alt.prec_1(x.target= area.grid$x, z.target= area.grid$z, d.theta1= 0.05, d.theta2= 0.05, x.cam2= 28.9, z.cam2= -1.92, nsim= 1000)
test_103L$ln.d.x<- round(log(abs(test_103L$d.x)), 2)
test_103L$ln.d.z<- round(log(abs(test_103L$d.z)), 2)

test_103L$ln.d.z[is.na(test_103L$ln.d.z)]<- 0
test_103L$ln.d.x[is.na(test_103L$ln.d.x)]<- 0

#Plotting the result on a grid
gz.1<- ggplot(test_103L, aes(x.target, z.target)) +
  geom_raster(aes(fill = ln.d.z)) +
  geom_contour(colour = "white", aes(z= ln.d.z), bins= 6, alpha= 0.5) +
  ggtitle("Camera 1 and 2 = 103 degrees")

gx.1<- ggplot(test_103L, aes(x.target, z.target)) +
  geom_raster(aes(fill = ln.d.x)) +
  geom_contour(colour = "white", aes(z= ln.d.x), bins= 18, alpha= 0.5) +
  ggtitle("Camera 1 and 2 = 103 degrees")

gzx.1<- grid.arrange(gz.1, gx.1, nrow= 2)
gzx.1

#ggsave("C:/Users/nhy577/Documents/_toma/TEACHING/PhD/VivianneEilers/FieldWork/dist50-elevminus10.pdf", plot= gzx)


#####
#For two cameras with 54 degrees of field of view - 6mm
#Left side - Cam 1 is higher than Cam 2
alt.prec_2<- function(x.cam2= 28.9, z.cam2= -1.92, x.target, z.target, d.theta1= 0.03, d.theta2= 0.03, nsim= 10000){
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
# run the simulation with x.cam2= 28.9, z.cam2= -1.92 and measurement error of 0.05 decimal degree
test_54L<- alt.prec_2(x.target= area.grid$x, z.target= area.grid$z, d.theta1= 0.03, d.theta2= 0.03, x.cam2= 28.9, z.cam2= -1.92, nsim= 1000)
test_54L$ln.d.x<- round(log(abs(test_54L$d.x)), 2)
test_54L$ln.d.z<- round(log(abs(test_54L$d.z)), 2)

test_54L$ln.d.z[is.na(test_54L$ln.d.z)]<- 0
test_54L$ln.d.x[is.na(test_54L$ln.d.x)]<- 0

gz.2<- ggplot(test_54L, aes(x.target, z.target)) +
  geom_raster(aes(fill = ln.d.z)) +
  geom_contour(colour = "white", aes(z= ln.d.z), bins= 6, alpha= 0.5) +
  ggtitle("Camera 1 and 2 = 54 degrees")


gx.2<- ggplot(test_54L, aes(x.target, z.target)) +
  geom_raster(aes(fill = ln.d.x)) +
  geom_contour(colour = "white", aes(z= ln.d.x), bins= 18, alpha= 0.5) +
  ggtitle("Camera 1 and 2 = 54 degrees")

gzx.2<- grid.arrange(gz.2, gx.2, nrow= 2)
gzx.2

#####
#For the camera 1 with 54 degrees of field of view - 6mm and Camera 2 with 103 degrees of field of view - 2.8mm
#Left side - Cam 1 is higher than Cam 2
alt.prec_3<- function(x.cam2= 28.9, z.cam2= -1.92, x.target, z.target, d.theta1= 0.03, d.theta2= 0.05, nsim= 10000){
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
# run the simulation with x.cam2= 28.9, z.cam2= -1.92 and measurement error of 0.05 decimal degree
test_54.103L<- alt.prec_3(x.target= area.grid$x, z.target= area.grid$z, d.theta1= 0.03, d.theta2= 0.05, x.cam2= 28.9, z.cam2= -1.92, nsim= 1000)
test_54.103L$ln.d.x<- round(log(abs(test_54.103L$d.x)), 2)
test_54.103L$ln.d.z<- round(log(abs(test_54.103L$d.z)), 2)

test_54.103L$ln.d.z[is.na(test_54.103L$ln.d.z)]<- 0
test_54.103L$ln.d.x[is.na(test_54.103L$ln.d.x)]<- 0

gz.3<- ggplot(test_54.103L, aes(x.target, z.target)) +
  geom_raster(aes(fill = ln.d.z)) +
  geom_contour(colour = "white", aes(z= ln.d.z), bins= 6, alpha= 0.5) +
  ggtitle("Camera 1 = 54 degrees and Camera 2 = 103 degrees")


gx.3<- ggplot(test_54.103L, aes(x.target, z.target)) +
  geom_raster(aes(fill = ln.d.x)) +
  geom_contour(colour = "white", aes(z= ln.d.x), bins= 18, alpha= 0.5) +
  ggtitle("Camera 1 = 54 degrees and Camera 2 = 103 degrees")

gzx.3<- grid.arrange(gz.3, gx.3, nrow= 2)
gzx.3

#####
#For camera 1 with 103 degrees of field of view - 2.8mm and camera 2 with 54 degrees of field of view - 6mm
#Left side - Cam 1 is higher than Cam 2
alt.prec_4<- function(x.cam2= 28.9, z.cam2= -1.92, x.target, z.target, d.theta1= 0.05, d.theta2= 0.03, nsim= 10000){
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
# run the simulation with x.cam2= 28.9, z.cam2= -1.92 and measurement error of 0.05 decimal degree
test_103.54L<- alt.prec_4(x.target= area.grid$x, z.target= area.grid$z, d.theta1= 0.05, d.theta2= 0.03, x.cam2= 28.9, z.cam2= -1.92, nsim= 1000)
test_103.54L$ln.d.x<- round(log(abs(test_103.54L$d.x)), 2)
test_103.54L$ln.d.z<- round(log(abs(test_103.54L$d.z)), 2)

test_103.54L$ln.d.z[is.na(test_103.54L$ln.d.z)]<- 0
test_103.54L$ln.d.x[is.na(test_103.54L$ln.d.x)]<- 0

gz.4<- ggplot(test_103.54L, aes(x.target, z.target)) +
  geom_raster(aes(fill = ln.d.z)) +
  geom_contour(colour = "white", aes(z= ln.d.z), bins= 6, alpha= 0.5) +
  ggtitle("Camera 1 = 103 degrees and Camera 2 = 54 degrees")


gx.4<- ggplot(test_103.54L, aes(x.target, z.target)) +
  geom_raster(aes(fill = ln.d.x)) +
  geom_contour(colour = "white", aes(z= ln.d.x), bins= 18, alpha= 0.5) +
  ggtitle("Camera 1 = 103 degrees and Camera 2 = 54 degrees")

gzx.4<- grid.arrange(gz.4, gx.4, nrow= 2)
gzx.4

#Altitude
grid.arrange(gz.1, gz.2, gz.3, gz.4, nrow= 2)

#Distance
grid.arrange(gx.1, gx.2, gx.3, gx.4, nrow= 2)

##################
####################
#################

# Stochastic version of the error calculation, assuming uniform error between min and max, and returning SD of error
#For two cameras with 103 degrees of field of view - 2.8mm
#Right side - Cam 1 is lower than Cam 2
alt.prec_5<- function(x.cam2= 28.9, z.cam2= 1.92, x.target, z.target, d.theta1= 0.05, d.theta2= 0.05, nsim= 10000){
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

# run the simulation with x.cam2= 28.9, z.cam2= 1.92 and measurement error of 0.05 decimal degree
test_103R<- alt.prec_5(x.target= area.grid$x, z.target= area.grid$z, d.theta1= 0.05, d.theta2= 0.05, x.cam2= 28.9, z.cam2= 1.92, nsim= 1000)
test_103R$ln.d.x<- round(log(abs(test_103R$d.x)), 2)
test_103R$ln.d.z<- round(log(abs(test_103R$d.z)), 2)

test_103R$ln.d.z[is.na(test_103R$ln.d.z)]<- 0
test_103R$ln.d.x[is.na(test_103R$ln.d.x)]<- 0

#Plotting the result on a grid
gz.5<- ggplot(test_103R, aes(x.target, z.target)) +
  geom_raster(aes(fill = ln.d.z)) +
  geom_contour(colour = "white", aes(z= ln.d.z), bins= 6, alpha= 0.5) +
  ggtitle("Camera 1 and 2 = 103 degrees")

gx.5<- ggplot(test_103R, aes(x.target, z.target)) +
  geom_raster(aes(fill = ln.d.x)) +
  geom_contour(colour = "white", aes(z= ln.d.x), bins= 18, alpha= 0.5) +
  ggtitle("Camera 1 and 2 = 103 degrees")

gzx.5<- grid.arrange(gz.5, gx.5, nrow= 2)
gzx.5

#ggsave("C:/Users/nhy577/Documents/_toma/TEACHING/PhD/VivianneEilers/FieldWork/dist50-elevminus10.pdf", plot= gzx)


#####
#For two cameras with 54 degrees of field of view - 6mm
#Right side - Cam 1 is lower than Cam 2
alt.prec_6<- function(x.cam2= 28.9, z.cam2= 1.92, x.target, z.target, d.theta1= 0.03, d.theta2= 0.03, nsim= 10000){
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
# run the simulation with x.cam2= 28.9, z.cam2= -1.92 and measurement error of 0.05 decimal degree
test_54R<- alt.prec_6(x.target= area.grid$x, z.target= area.grid$z, d.theta1= 0.03, d.theta2= 0.03, x.cam2= 28.9, z.cam2= 1.92, nsim= 1000)
test_54R$ln.d.x<- round(log(abs(test_54R$d.x)), 2)
test_54R$ln.d.z<- round(log(abs(test_54R$d.z)), 2)

test_54R$ln.d.z[is.na(test_54R$ln.d.z)]<- 0
test_54R$ln.d.x[is.na(test_54R$ln.d.x)]<- 0

gz.6<- ggplot(test_54R, aes(x.target, z.target)) +
  geom_raster(aes(fill = ln.d.z)) +
  geom_contour(colour = "white", aes(z= ln.d.z), bins= 6, alpha= 0.5) +
  ggtitle("Camera 1 and 2 = 54 degrees")


gx.6<- ggplot(test_54R, aes(x.target, z.target)) +
  geom_raster(aes(fill = ln.d.x)) +
  geom_contour(colour = "white", aes(z= ln.d.x), bins= 18, alpha= 0.5) +
  ggtitle("Camera 1 and 2 = 54 degrees")

gzx.6<- grid.arrange(gz.6, gx.6, nrow= 2)
gzx.6

#####
#For the camera 1 with 54 degrees of field of view - 6mm and Camera 2 with 103 degrees of field of view - 2.8mm
#Right side - Cam 1 is lower than Cam 2
alt.prec_7<- function(x.cam2= 28.9, z.cam2= 1.92, x.target, z.target, d.theta1= 0.03, d.theta2= 0.05, nsim= 10000){
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
# run the simulation with x.cam2= 28.9, z.cam2= -1.92 and measurement error of 0.05 decimal degree
test_54.103R<- alt.prec_7(x.target= area.grid$x, z.target= area.grid$z, d.theta1= 0.03, d.theta2= 0.05, x.cam2= 28.9, z.cam2= 1.92, nsim= 1000)
test_54.103R$ln.d.x<- round(log(abs(test_54.103R$d.x)), 2)

test_54.103R$ln.d.z<- round(log(abs(test_54.103R$d.z)), 2)

test_54.103R$ln.d.z[is.na(test_54.103R$ln.d.z)]<- 0
test_54.103R$ln.d.x[is.na(test_54.103R$ln.d.x)]<- 0

gz.7<- ggplot(test_54.103R, aes(x.target, z.target)) +
  geom_raster(aes(fill = ln.d.z)) +
  geom_contour(colour = "white", aes(z= ln.d.z), bins= 6, alpha= 0.5) +
  ggtitle("Camera 1 = 54 degrees and Camera 2 = 103 degrees")

gx.7<- ggplot(test_54.103R, aes(x.target, z.target)) +
  geom_raster(aes(fill = ln.d.x)) +
  geom_contour(colour = "white", aes(z= ln.d.x), bins= 18, alpha= 0.5) +
  ggtitle("Camera 1 = 54 degrees and Camera 2 = 103 degrees")

gzx.7<- grid.arrange(gz.7, gx.7, nrow= 2)
gzx.7

#####
#For camera 1 with 103 degrees of field of view - 2.8mm and camera 2 with 54 degrees of field of view - 6mm
#Right side - Cam 1 is lower than Cam 2
alt.prec_8<- function(x.cam2= 28.9, z.cam2= 1.92, x.target, z.target, d.theta1= 0.05, d.theta2= 0.03, nsim= 10000){
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
# run the simulation with x.cam2= 28.9, z.cam2= -1.92 and measurement error of 0.05 decimal degree
test_103.54R<- alt.prec_8(x.target= area.grid$x, z.target= area.grid$z, d.theta1= 0.05, d.theta2= 0.03, x.cam2= 28.9, z.cam2= 1.92, nsim= 1000)
test_103.54R$ln.d.x<- round(log(abs(test_103.54R$d.x)), 2)
test_103.54R$ln.d.z<- round(log(abs(test_103.54R$d.z)), 2)

test_103.54R$ln.d.z[is.na(test_103.54R$ln.d.z)]<- 0
test_103.54R$ln.d.x[is.na(test_103.54R$ln.d.x)]<- 0

gz.8<- ggplot(test_103.54R, aes(x.target, z.target)) +
  geom_raster(aes(fill = ln.d.z)) +
  geom_contour(colour = "white", aes(z= ln.d.z), bins= 6, alpha= 0.5) +
  ggtitle("Camera 1 = 103 degrees and Camera 2 = 54 degrees")

gx.8<- ggplot(test_103.54R, aes(x.target, z.target)) +
  geom_raster(aes(fill = ln.d.x)) +
  geom_contour(colour = "white", aes(z= ln.d.x), bins= 18, alpha= 0.5) +
  ggtitle("Camera 1 = 103 degrees and Camera 2 = 54 degrees")

gzx.8<- grid.arrange(gz.8, gx.8, nrow= 2)
gzx.8

#Altitude
grid.arrange(gz.5, gz.6, gz.7, gz.8, nrow= 2)

#Distance
grid.arrange(gx.5, gx.6, gx.7, gx.8, nrow= 2)

#########
###########
##########

#OBTUSE ANGLE
#For camera 1 with 103 degrees of field of view - 2.8mm (obtuse angle) and camera 2 with 54 degrees of field of view - 6mm (small angle)
#Left side - Cam 1 is higher than Cam 2
alt.prec_9<- function(x.cam2= 28.9, z.cam2= -1.92, x.target, z.target, d.theta1= 0.05, d.theta2= 0.03, nsim= 10000){
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
# run the simulation with x.cam2= 28.9, z.cam2= -1.92 and measurement error of 0.05 decimal degree
test_103.54R<- alt.prec_8(x.target= area.grid$x, z.target= area.grid$z, d.theta1= 0.05, d.theta2= 0.03, x.cam2= 28.9, z.cam2= 1.92, nsim= 1000)
test_103.54R$ln.d.x<- round(log(abs(test_103.54R$d.x)), 2)
test_103.54R$ln.d.z<- round(log(abs(test_103.54R$d.z)), 2)

test_103.54R$ln.d.z[is.na(test_103.54R$ln.d.z)]<- 0
test_103.54R$ln.d.x[is.na(test_103.54R$ln.d.x)]<- 0

gz.8<- ggplot(test_103.54R, aes(x.target, z.target)) +
  geom_raster(aes(fill = ln.d.z)) +
  geom_contour(colour = "white", aes(z= ln.d.z), bins= 6, alpha= 0.5) +
  ggtitle("Camera 1 = 103 degrees and Camera 2 = 54 degrees")

gx.8<- ggplot(test_103.54R, aes(x.target, z.target)) +
  geom_raster(aes(fill = ln.d.x)) +
  geom_contour(colour = "white", aes(z= ln.d.x), bins= 18, alpha= 0.5) +
  ggtitle("Camera 1 = 103 degrees and Camera 2 = 54 degrees")

gzx.8<- grid.arrange(gz.8, gx.8, nrow= 2)
gzx.8

# Another way of doing, without ggplot2:
par(mfrow= c(2, 1))
image(x= 0:Nx+1, y= 0:Nz+1, z= matrix(test_103L$ln.d.z, Nx+1, 151), col= hcl.colors(24, "ag_GrnYl"), main= "ln(dz)", xlab= "Distance (m)", ylab= "Altitude (m)")
iso.z<- pretty(range(test_103L$ln.d.z, finite = TRUE), 30)
lab.z<- round(exp(iso.z), 1)
# contour(x= 0:Nx, y= 0:Nz, z= matrix(test_54$ln.d.z, Nx+1, 151), add= T, rgb(1, 1, 1, 0.5), nlevels= 6, zlim = range(test_54$ln.d.z, finite = TRUE), labels= exp(pretty(range(test_54$ln.d.z, finite = TRUE), 6)))
contour(x= 0:Nx, y= 0:Nz, z= matrix(test_103L$ln.d.z, Nx+1, 151), add= T, levels= iso.z, labels= lab.z)
# contour(x= 0:Nx, y= 0:Nz, z= matrix(test_54$d.z, Nx+1, 151), add= T, nlevels= 50)
image(x= 0:Nx+1, y= 0:Nz+1, z= matrix(test_103L$ln.d.x, Nx+1, 151), col= hcl.colors(24, "ag_GrnYl"), main= "ln(dx)")
contour(x= 0:Nx, y= 0:Nz, z= matrix(test_103L$ln.d.x, Nx+1, 151), add= T, nlevels= 30)


# package the whole thing (simulation + plotting) into a single function "test.func.gg"
test.func.gg<- function(Nx= 500, Nz= 200, x.cam2= 28.9, z.cam2= -1.92, d.theta1= 0.05, d.theta2= 0.05, print= T){
	# create study area Nx m x Nz m
	area.grid<- expand.grid(list(x= seq(0, Nx, l= 101), z= seq(0, Nz, l= 101)))#0:Nz

	# run the simulation with x.cam2= 20, z.cam2= 0, and measurement error of 0.05 decimal degree
	test_54<- alt.prec_3(x.target= area.grid$x, z.target= area.grid$z, d.theta1= d.theta1, d.theta2= d.theta2, x.cam2= x.cam2, z.cam2= z.cam2)
	test_54$ln.d.x<- log(abs(test_54$d.x))

	if(print){
		gz<- ggplot(test_54, aes(x.target, z.target)) +
		geom_raster(aes(fill = d.z)) +
		geom_contour(colour = "white", aes(z= d.z), bins= 20, alpha= 0.5)
		print(gz)
	}

	test_54
}

#test1<- test.func.gg(Nx= 500, Nz= 150, x.cam2= 50, z.cam2= -3, d.theta1= 0.05, d.theta2= 0.05)

# a function for plotting the field of view as well as expected precision
# FOV<- function(fov= 120, Nx= 500, Nz= 150, x.cam2= 50, z.cam2= -3, d.theta1= 0.05, d.theta2= 0.05){
# 	dat<- test.func.gg(Nx= Nx, Nz= Nz, x.cam2= x.cam2, z.cam2= z.cam2, d.theta1= d.theta1, d.theta2= d.theta2, print= F)
# 	targ.angle1<- atan(dat$z.target / dat$x.target) * 180 / pi
# 	dat$out.1<- ifelse(targ.angle1<0, (180 + targ.angle1)>fov, targ.angle1>fov)
# 	targ.angle2<- atan((dat$z.target - z.cam2) / (dat$x.target - x.cam2)) * 180 / pi
# 	dat$out.2<- ifelse(targ.angle2<0, (180 + targ.angle2)>fov, targ.angle2>fov)
# 	dat$out<- dat$out.1 | dat$out.2
# 	dat$d.z[dat$out]<- NA
# 
# 	gz<- ggplot(dat, aes(x.target, z.target)) +
# 	geom_raster(aes(fill = d.z)) +
# 	geom_contour(colour = "white", aes(z= d.z), bins= 6, alpha= 0.5)
# 	print(gz)
# 
# 	dat
# 
# }
# 
# bla<- FOV(fov= 103, Nx= 500, Nz= 150, x.cam2= 50, z.cam2= 0, d.theta1= 0.05, d.theta2= 0.05)
# not working! Not had time to find where the bug(s) is(are)





