# Generation of sample data, packaged in a data.frame
#
#####################################################

# Linear Function

LinearFunc <- function(x, a = 1, b = 0){
  y = a * x + b
}

x <- seq(0, 5, length.out = 11)
y <- LinearFunc(x)
lin.data <- data.frame(x = x, y = y)
plot(lin.data)

#####################################################

# Oscillating Function

OscillationFunc <- function(x, a = pi, b = 1){
  y = sin(a * x) + b * x
}

x <- seq(0, 5, length.out = 21)
y <- OscillationFunc(x)
osz.data <- data.frame(x = x, y = y)
plot(osz.data)

#####################################################

# Polynomial Function (3rd deg)

Poly3Func <- function(x, a = 1, b = -3, c = 1, d = 1){
  y = a * x^3 + b * x^2 + c * x + d
}

x <- seq(-1, 3, length.out = 11)
y <- Poly3Func(x)
pl3.data <- data.frame(x = x, y = y)
plot(pl3.data)

#####################################################

# Polynomial Function (5rd deg)

Poly5Func <- function(x, a = 1, b = -3, c = 0.3, d = 1, e = 1, f = 1){
  y = a * x^5 + b * x^4 + c * x^3 + d * x^2 + e * x + f 
}

x <- seq(-1, 3, length.out = 21)
y <- Poly5Func(x)
pl5.data <- data.frame(x = x, y = y)
plot(pl5.data)

#####################################################

# Linear Function with a jump

LinJumpFunc <- function(x, a1 = -1, b1 = 5, a2 = 0.5, b2 = 0){
  y <- 0.5*abs((sign(x)-1))*(a1 * x + b1) + 0.5*(sign(x)+1)*(a2 * x + b2)
}

x <- seq(-5, 5, length.out = 10)
y <- LinJumpFunc(x)
jmp.data <- data.frame(x = x, y = y)
plot(jmp.data)

#####################################################

# Linear Function with an outlier

x <- seq(-5, 5, length.out = 11)
y <- LinearFunc(x, a = 0.5)
y[7] <- y[7] + 7
out.data <- data.frame(x = x, y = y)
plot(out.data)

#####################################################

# Clean up
rm(x, y)
