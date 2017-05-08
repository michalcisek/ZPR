# Proba w S3 --------------------------------------------------------------
rm(list=ls())

sts <- function(time, value){
  #sprawdzenie poprawnosci argumentow
  if(!is.numeric(time) | !is.numeric(value)){
    "At least one of the vector is not numeric!"
  } else {
    sts <- structure(data.frame(time=time, value=value), class=c("sts", "data.frame"))
  }
  
  return(sts)
}


plot.sts <- function(x, ...){
  plot(x=x$time, y=x$value, ...)
}

lines.sts <- function(x, ...){
  lines(x=x$time, y=x$value, ...)
}

points.sts <- function(x, ...){
  points(x=x$time, y=x$value, ...)
}

"[.sts" <- function(x, i, drop="missing"){
  .time <- x$time[i]
  .value <- x$value[i]  
  
  sts(time = .time, value = .value)
}

window.sts <- function(x, start, end){
  if(missing(end)) end <- max(x$time)
  ind <- which(x$time >= start & x$time <= end)
  .time <- x$time[ind]
  .value <- x$value[ind]
  
  sts(time = .time, value = .value)
}

select <- function(x, cond){
  attach(x)
  cond <- deparse(substitute(cond))
  ind <- which(eval(parse(text = cond)))
  detach(x)
  
  .time <- x$time[ind]
  .value <- x$value[ind]
  
  sts(time = .time, value = .value)
}


simplify <- function(x){
  rows <- length(x$time)
  
  elements <- rep(NA, rows)
  elements[1:2] <- T
  for(i in 3:rows){
    if( (x$value[i-1] > x$value[i-2] & x$value[i] < x$value[i-1]) |
        (x$value[i-1] < x$value[i-2] & x$value[i] > x$value[i-1])){
      elements[i-1] <- T
    } else{
      elements[i-1] <- F 
    }
  }
  
  .time <- x$time[elements]
  .value <- x$value[elements]
  
  sts(time = .time, value = .value)
}

#sprawdzenie dzialania
n <- 100
s <- sts(time = sort(rnorm(n)), value = cumsum(rnorm(n)))

pdf(file="fig1.pdf")
plot(s, type="o", pch=20, cex=1.2, col=rgb(0,0,1,.5))
grid()
dev.off()


pdf(file="fig2.pdf")
plot(s, type="o", pch=20, cex=1.2, col=rgb(0,0,1,.5))

s1 <- s[1:20]
lines(s1, col="red", lwd=2)
points(s1, col="red", pch=20, cex=1.3)

s2 <- window(s, start=.5)
lines(s2, col="magenta", lwd=2)
points(s2, col="magenta", pch=20, cex=1.3)

s3 <- select(s, value > -12 & value < -5)
points(s3, col="green", pch=20, cex=1.3)

s4 <- simplify(s)
lines(s4, col="black", lty="dashed")
dev.off()



# # Proba w S4 --------------------------------------------------------------
# rm(list=ls())
# library(methods)
# 
# 
# setOldClass("data.frame")
# sts <- setClass("sts",
#                 slots = list(time = "numeric", value = "numeric"),
#                 contains = "data.frame",
#                 validity = function(object){
#                   if (length(object@time) != length(object@value)){
#                     return("Liczba podanych czasów jest różna od liczby podanych wartości!")
#                   } else{
#                     TRUE
#                   }
#                 })
# 
# sts <- function(time, value){
#   pr <- new("sts", time=time, value=value)
#   data.frame(time = pr@time, value = pr@value)
# }
# 
# pr<-sts(time=1:10, value=rnorm(10))
# 
# proba <- new("sts", time=1:10, value=rnorm(10))
# proba@time
# class(proba)
# typeof(proba)
# mode(proba)
# otype(proba)
# 
# #sprawdzenie czy zwroci blad
# new("sts", time=1:10, value=rnorm(9))
# 
# 
# #przeciazenie metody plot
# setMethod("plot",
#           signature = c(x = "sts"),
#           function(x, ...){
#             plot(x=x@time, y=x@value, ...)
#           })
# 
# 
# setMethod("lines",
#           signature = c(x = "sts"),
#           function(x, ...){
#             lines(x=x@time, y=x@value, ...)
#           })
# 
# setMethod("points",
#           signature = c(x = "sts"),
#           function(x, ...){
#             points(x=x@time, y=x@value, ...)
#           })
# 
# 
# 
# n <- 100
# s <- sts(time = sort(rnorm(n)), value = cumsum(rnorm(n)))
# s@.Data
# s1 <- data.frame(s@time, s@value)
# 
# plot(s,type="l")
# lines(s)
# points(s)
# 
# print(s)
