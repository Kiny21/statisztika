rec <- function(x) (abs(x) < 1) * 0.5
tri <- function(x) (abs(x) < 1) * (1 - abs(x))
gauss <- function(x) 1/sqrt(2*pi) * exp(-(x^2)/2)
x <- seq(from = -3, to = 3, by = 0.001)
plot(x, rec(x), type = "l", ylim = c(0,1), lty = 1,
          ylab = expression(K(x)))
lines(x, tri(x), lty = 2)
lines(x, gauss(x), lty = 3)
legend(-3, 0.8, legend = c("Rectangular", "Triangular",
                                "Gaussian"), lty = 1:3, title = "kernel functions", bty = "n")
###
x <- c(0, 1, 1.1, 1.5, 1.9, 2.8, 2.9, 3.5)
n <- length(x)
xgrid <- seq(from = min(x) - 1, to = max(x) + 1, by = 0.01)
h <- 0.4
bumps <- sapply(x, function(a) gauss((xgrid - a)/h)/(n * h))
plot(xgrid, rowSums(bumps), ylab = expression(hat(f)(x)), type = "l", xlab = "x", lwd = 2)
rug(x, lwd = 2)
out <- apply(bumps, 2, function(b) lines(xgrid, b))
###
epa <- function(x, y)  ((x^2 + y^2) < 1) * 2/pi * (1 - x^2 - y^2)
x <- seq(from = -1.1, to = 1.1, by = 0.05)
epavals <- sapply(x, function(a) epa(a, x))
persp(x = x, y = x, z = epavals, xlab = "x", ylab = "y",
           zlab = expression(K(x, y)), theta = -35, axes = TRUE,
           box = TRUE)
###
logL = function(param, x){
  d1 = dnorm(x, mean = param[2], sd = param[3])
  d2 = dnrom(x, mean = param[4], sd = param[5])
  -sum(log(param[1] * d1 + (1-param[1] * d2)))
}

data("faithful", package = "datasets")
x <- faithful$waiting
layout(matrix(1:3, ncol = 3))
hist(x, xlab = "Waiting times (in min.)", ylab = "Frequency",
          probability = TRUE, main = "Gaussian kernel",
          border = "gray")
lines(density(x, width = 12), lwd = 2) #sz?less?g n?vel?s?vel laposabb g?rbe ker?l r?
rug(x)
hist(x, xlab = "Waiting times (in min.)", ylab = "Frequency",
          probability = TRUE, main = "Rectangular kernel",
          border = "gray")
lines(density(x, width = 12, window = "rectangular"), lwd = 2)
rug(x)
hist(x, xlab = "Waiting times (in min.)", ylab = "Frequency",
          probability = TRUE, main = "Triangular kernel",
          border = "gray")
lines(density(x, width = 12, window = "triangular"), lwd = 2)
rug(x)

#freq=F ?s prob=T ugyanaz: hist ?sszter?let?vel oszt, ter?let 1
###
library("KernSmooth")
data("CYGOB1", package = "HSAUR2")
CYGOB1d <- bkde2D(CYGOB1, bandwidth = sapply(CYGOB1, dpik))
contour(x = CYGOB1d$x1, y = CYGOB1d$x2, z = CYGOB1d$fhat,
             xlab = "log surface temperature",
             ylab = "log light intensity") #2D-re s?r?s?gfv illeszt?s

persp(x = CYGOB1d$x1, y = CYGOB1d$x2, z = CYGOB1d$fhat,
        xlab = "log surface temperature",
        ylab = "log light intensity",
        zlab = "estimated density",
        theta = -35, axes = TRUE, box = TRUE) #3D-re uez

