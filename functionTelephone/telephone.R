# telephone.R
rm(list=ls())

funs <- list()

# (variation on) logistic function
funs$f_1 <- function(x,a,b){
  1/(1+exp(-(a*x+b)))
}

# normal (Gaussian) probability density function
funs$f_2 <- function(x,a,b){
  exp(-((x-a)/b)^2/2)/(b*sqrt(2*pi))
}

# Weibull cumulative density function (a,b >0)
# defined for x >=0 (alt: piecewise)
funs$f_3 <- function(x,a,b){
  1-exp(-(x/b)^a)
}

# Weibull probability density function (a,b >0)
# defined for x >=0 (alt: piecewise)
funs$f_4 <- function(x,a,b){
  (a/b)*(x/b)^(a-1)*exp(-(x/b)^a)
}

# Kumaraswamy probability density function (a,b >0)
# continuously defined on the interval from 0 to 1
funs$f_5 <- function(x,a,b){
 a*b*x^(a-1)*(1-x^a)^(b-1) 
}

# Kumaraswamy cumulative density function (a,b >0)
# continuously defined on the interval from 0 to 1
funs$f_6 <- function(x,a,b){
  1-(1-x^a)^b
}

# U-quadratic distribution (a > 0)
# * only defined on interval a to b
funs$f_7 <- function(x,a,b){
  a*(x-b)^2
}

# Gompertz distribution pdf
# defined for x >=0 (alt: piecewise)
funs$f_8 <- function(x,a,b){
  a*b*exp(a*x)*exp(b)*exp(-b*exp(a*x))
}

# Beverton Holt like
funs$f_9 <- function(x,a,b){
  a*x/(1+x/b)
}

# Ricker like
funs$f_10 <- function(x,a,b){
  x*exp(a*(1-x/b))
}

# ... (0 <= a <= 1; b >= 0); 
funs$f_11 <- function(x,a,b){
  a*x/(a*x - (1-a)*b)
}

# logistic growth (a,b > 0)
funs$f_12 <- function(x,a,b){
  a*x*(1-x/b)
}

par(bty="n",ann=F,mar=c(4,2,1,1),mfrow=c(2,ceiling(length(funs)/2)))
for(nn in 1:length(funs)){
  curve(funs[[nn]](x,a=.10,b=2),-10,10,lwd=2)
}

