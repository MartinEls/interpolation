# Steffen interpolation method

# sample data
sample.data <- data.frame(x = c(0,1,2,5,7), y = c(-3,-1,-2,-1,-2))
#sample.data <- data.frame(x = c(0,1,2,5,7), y = c(0,1,2,1,1))
#sample.data <- data.frame(x = c(0,1,2,5,7), y = 0.5*c(0,1,2,5,7))

sample.data
plot(sample.data)

SteffenPolynom <- function(x, x.ref, a, b, c, d){
  y <- a * (x - x.ref)^3 + b * (x - x.ref)^2 + c * (x - x.ref) + d
}

PlotSpline <- splinefun(sample.data[,1], sample.data[,2], method = "natural")
x <- seq(0, 7, length.out = 701)
y <- PlotSpline(x)
lines(x, y, col = "red")
rm(x, y)

SteffenCoef <- function(df){
  v  <- rep(NaN, times = nrow(df))
  df <- data.frame(x = df[,1], y = df[,2],
                   a = v, b = v, c = v, d = v,
                   h = v, p = v, s = v, 
                   y1 = v)
  for(i in 1:(nrow(df)-1)){
    df$h[i] <- df$x[(i+1)] - df$x[i]
    df$s[i] <- (df$y[(i+1)] - df$y[i])/(df$h[i])
  }
  
  for(i in 2:(nrow(df)-1)){
    df$p[i]  <- (df$s[i-1] * df$h[i] + df$s[i] * df$h[i-1])/(df$h[i-1] + df$h[i])
    df$y1[i] <- (sign(df$s[i-1]) + sign(df$s[i])) *
                min(abs(df$s[i-1]), abs(df$s[i]), (0.5*abs(df$p[i])))
  }
  
  N <- nrow(df)
  df$p[1] <- df$s[1] * (1 + df$h[1]/(df$h[1]+df$h[2])) - 
             df$s[2] * (df$h[1]/((df$h[1])+df$h[2]))
  df$p[N] <- df$s[N-1] * (1 + df$h[N-1]/(df$h[N-1] + df$h[N-2])) -
             df$s[N-2] * (df$h[N-1]/(df$h[N-1] + df$h[N-2]))
  
  if((df$p[1] * df$s[1]) <= 0)          df$y1[1] <- 0
  if(abs(df$p[1]) > (2 * abs(df$s[1]))) df$y1[1] <- 2 * df$s[1]
  else                                  df$y1[1] <- df$p[1]
  
  if((df$p[N] * df$s[N-1]) <= 0)          df$y1[N] <- 0
  if(abs(df$p[N]) > (2 * abs(df$s[N-1]))) df$y1[N] <- 2 * df$s[N-1]
  else                                    df$y1[N] <- df$p[N]
  
  for(i in 1:(nrow(df)-1)){
    df$a[i] <- (df$y1[i] + df$y1[i+1] - 2 * df$s[i]) / (df$h[i])^2
    df$b[i] <- (3 * df$s[i] - 2 * df$y1[i] - df$y1[i+1]) / df$h[i]
    df$c[i] <- df$y1[i]
    df$d[i] <- df$y[i]
  }
    
  return(df)
}

steffen.sample.coef <- SteffenCoef(sample.data)

x.int1 <- seq(0, 1, length.out = 101)
y.int1 <- SteffenPolynom(x.int1, steffen$x[1], steffen$a[1], steffen$b[1], steffen$c[1], steffen$d[1])
lines(x.int1, y.int1, col = "blue")

x.int2 <- seq(1, 2, length.out = 101)
y.int2 <- SteffenPolynom(x.int2, steffen$x[2], steffen$a[2], steffen$b[2], steffen$c[2], steffen$d[2])
lines(x.int2, y.int2, col = "blue")

x.int3 <- seq(2, 5, length.out = 301)
y.int3 <- SteffenPolynom(x.int3, steffen$x[3], steffen$a[3], steffen$b[3], steffen$c[3], steffen$d[3])
lines(x.int3, y.int3, col = "blue")

x.int4 <- seq(5, 7, length.out = 201)
y.int4 <- SteffenPolynom(x.int4, steffen$x[4], steffen$a[4], steffen$b[4], steffen$c[4], steffen$d[4])
lines(x.int4, y.int4, col = "blue")
