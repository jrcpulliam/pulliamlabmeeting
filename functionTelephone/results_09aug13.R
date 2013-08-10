###-- Eqn 1 --###
# lab [FIXED]
F_1 <- function(a,b,x){
  1/(1+exp(-(a*x+b)))
}
# DH
Fun1<-function (x, a, b)
{
  y=1/(1+exp(-(a*x+b)))
  return(y)
}
# Original function at x=0, for a=.1,b=2: 0.8807971
# lab [FIXED]
F_1(x=0,a=.1,b=2)
# DH
Fun1(x=0,a=.1,b=2)
# Original function at x=10, for a=.1,b=2: .9525741
# lab [FIXED]
F_1(x=10,a=.1,b=2)
# DH
Fun1(x=10,a=.1,b=2)

###-- Eqn 2 --###
# lab
F_X <- function(x,a,b){
  ((1/b)*(2*pi^(-1/2))*exp(-(x-a)^2))/((2b)^2)
}
# Original function at x=0, for a=.1,b=2: 0.199222
# lab:
F_X(x=0,a=.1,b=2)
# Original function at x=10, for a=.1,b=2: 9.533005e-07
# lab:
F_X(x=10,a=.1,b=2)

###-- Eqn 3 --###
# lab
f_4 <- function(x,a,b){
  1-exp(-a*x/b)
}
# DH
Fun3<-function (x, a, b)
{
  y= 1-exp(-(x/b)^a)
  Return(y)
}
# Original function at x=0, for a=.1,b=2: 0
# lab
f_4(x=0,a=.1,b=2) # agrees at this value of x,
# BUT:
# Original function at x=10, for a=.1,b=2: 0.6910633
f_4(x=10,a=.1,b=2) # not at this one!!!
# DH:
Fun3(x=0,a=.1,b=2)
Fun3(x=10,a=.1,b=2)

###-- Eqn 4 ---###
# lab
f_4 = function(x,a,b){
  (a/x^(a-1))*e^((-x^a)/b)
}
# Original function at x=0, for a=.1,b=2: Inf
# lab:
f_4(x=0,a=.1,b=2)
# Original function at x=10, for a=.1,b=2: 0.003628829
# lab:
f_4(x=10,a=.1,b=2)

###-- Eqn 5 --###
f5 <- function(x,a,b){
  (ab*x^(a-1))(1-x^a)^(b-1)
}
# Original function at x=0, for a=.1,b=2: Inf
# lab:
f5(x=0,a=.1,b=2)
# Original function at x=10, for a=.1,b=2: -0.006519356
# lab:
f5(x=10,a=.1,b=2)

###-- Eqn 6 --###
# lab:
f_6 <- function(x,a,b){
  1-(1-x^(a))^(b)
}
# Original function at x=0, for a=.1,b=2: 0
# lab:
f_6(x=0,a=.1,b=2)
# Original function at x=10, for a=.1,b=2: 0.9329576
# lab:
f_6(x=10,a=.1,b=2)

