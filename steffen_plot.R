Steffen <- function(coef, n = 10){
  number.of.segments <- nrow(coef) - 1
  number.of.points   <- (number.of.segments * n) + 1
  data.empty         <- vector(mode = "numeric", length = number.of.points)
  
  rtn.value          <- data.frame(x = data.empty, y = data.empty)
  
  current.line       <- 1
  for(i in 1:number.of.segments){
    rtn.value$x[current.line] <- coef$x[i]
    rtn.value$y[current.line] <- coef$y[i]
    current.line <- current.line + 1
    
    current.x.st.sz <- coef$h[i] / n
    for(j in 1:(n-1)){
      x.pos <- coef$x[i] + (j * current.x.st.sz)
      rtn.value$x[current.line] <- x.pos
      
      rtn.value$y[current.line] <- SteffenPolynom(x.pos, coef$x[i], coef$a[i], coef$b[i], coef$c[i], coef$d[i])
      
      current.line <- current.line + 1
    }
  
  rtn.value$x[current.line] <- coef$x[nrow(coef)]
  rtn.value$y[current.line] <- coef$y[nrow(coef)]
  }
  return(rtn.value)
}

steffen.points <- Steffen(steffen.sample.coef, 10)
