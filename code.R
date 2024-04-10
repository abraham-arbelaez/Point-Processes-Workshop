#########################################
#
# POINT PROCESSES WORKSHOP
#
#########################################

#########################################
### BASE EXAMPLES
#########################################

#########################################
## Homogeneous Poisson point process
#########################################

set.seed(2024)
x <- runif(50, 0, 1)
y <- runif(50, 0, 1)

plot(x,y)

#########################################
## Inhomogeneous Poisson Point Process
#########################################

set.seed(2024)
lambda <- function(x,y){
  (x*y)^2
}


points <- matrix(NA, ncol = 2, n = 50)
for(i in 1:50){
  repeat{
    x <- runif(1, 0, 1)
    y <- runif(1, 0, 1)
    
    if(runif(1)<lambda(x,y)){
      points[i,] <- c(x,y)
      break
    }
  }
}

plot(points[,1],points[,2])


# creating grid
x <- seq(from = 0.001, to = 1, by = 0.001)
y <- seq(from = 0.001, to = 1, by = 0.001)

#outer(lambda(x,1), lambda(1,y))

newp <- outer(lambda(x,1), lambda(1,y))

newp <- apply(newp, 2, rev)
newp

r <- raster(nrows = 1000, ncols = 1000, xmn = 0, xmx = 1, ymn = 0, ymx = 1)
vals <- newp
r <- setValues(r, vals)

plot(r)
points(points[,1],points[,2], type = "p", pch = 20)





#########################################
### ACTUAL SIMULATION
#########################################

# Parameters first
#set.seed(2024)
beta0 = 0.4
beta1 = 0.3
beta2 = 0.02
 
# Cool lambda function
lambda <- function(x,y){
  exp(beta0 + beta1*sin(x^2+y^3) - beta2*y^3 )
}
 
## Creating plot of intensity
# loading raster library
library(raster)
 
# creating grid
x <- seq(from = 0.001, to = 1, by = 0.001)
y <- seq(from = 0.001, to = 1, by = 0.001)
 
# creating matrix of intensity
newp <- outer(lambda(x,1), lambda(1,y))
newp <- apply(newp, 2, rev)
newp
 
# creating raster
r <- raster(nrows = 1000, ncols = 1000, xmn = 0, xmx = 1, ymn = 0, ymx = 1)
vals <- newp
r <- setValues(r, vals)
 
# plotting intensity
plot(r)
 
 
## Simulating points
#set.seed(2024)
lambda.max <- maxValue(r) # Should be exp(beta0+beta1)
n <- rpois(1, lambda.max)
x <- runif(n,0,1) # all samples
y <- runif(n,0,1)
 
# the OG points are our original samples
ogpoints <- cbind(x, y)
 
# plot with OG points
plot(r)
points(ogpoints[,1], ogpoints[,2], pch = 16)
 
# lambda function
lambda(x,y)
 
# lambda
lambda(x,y)/lambda.max
 
 
## Rejection
#set.seed(2024)
m <- rbinom(n, 1, lambda(x,y)/lambda.max)
 
# New points
sx <- ifelse(m==1,x,"NA") # real ones
sx <- as.numeric(sx[-which(sx=="NA")])
sy <- ifelse(m==1,y,"NA") # real ones
sy <- as.numeric(sy[-which(sy=="NA")])
 
# the OG points are our original samples
points <- cbind(sx, sy)
 
# plot of OG points with points accepted
plot(r)
points(ogpoints[,1], ogpoints[,2])
points(points[,1], points[,2], col = "red", pch = 16)
 
# Final plot with points
plot(r)
points(points[,1], points[,2], pch = 16)


#########################################
### REAL LIFE DATA
#########################################

library(sp)
library(leaflet)
library(oce) # utm to lonlat
library(tidyverse)

# GRASSHOPPER SPARROW!
CBS011 <- read.csv("https://raw.githubusercontent.com/abraham-arbelaez/Point-Processes-Workshop/main/CBS011.csv")

# changing coordinate system
locations <- utm2lonlat(CBS011$PositionX, CBS011$PositionY, zone = 14, hemisphere = "N", km = FALSE)

# just dataset with points
locations <- as.data.frame(locations)

# getting rid of some birds that are very far
which.min(locations$latitude)
locations <- locations[-241,]

which.max(locations$latitude)
locations <- locations[-1655,]
locations <- locations[-1656,]

# Plotting GRSP
leaflet(locations) %>% 
  addCircleMarkers(radius = 1,
                   color = "#512888") %>% 
  addTiles()


## INTENSITY FUNCTION WITH ONLY KERNEL DENSITY ESTIMATORS

# heat map
ggplot() +
  stat_density_2d(data = GRSP, 
                  mapping = aes(x = purrr::map_dbl(geometry, ~.[1]),
                                y = purrr::map_dbl(geometry, ~.[2]),
                                fill = stat(density)),
                  geom = 'tile',
                  contour = FALSE,
                  alpha = 0.8) +
  scale_fill_viridis_c(option = 'magma', direction = -1) +
  xlab("Longitude") +
  ylab("Latitude")+
  theme_test()

# heat map with points
ggplot() +
  stat_density_2d(data = GRSP, 
                  mapping = aes(x = purrr::map_dbl(geometry, ~.[1]),
                                y = purrr::map_dbl(geometry, ~.[2]),
                                fill = stat(density)),
                  geom = 'tile',
                  contour = FALSE,
                  alpha = 0.8) +
  geom_sf(data = GRSP, color = 'red', size = 0.1) + 
  scale_fill_viridis_c(option = 'magma', direction = -1) +
  xlab("Longitude") +
  ylab("Latitude")+
  theme_test()

