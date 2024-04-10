###############################
#
# POINT PROCESSES WORKSHOP
#
###############################


## Homogeneous Poisson point process

set.seed(2024)
x <- runif(50, 0, 1)
y <- runif(50, 0, 1)

plot(x,y)


## Inhomogeneous Poisson Point Process

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
