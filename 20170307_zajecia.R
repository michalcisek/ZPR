#przyklad motywujacy
x<-cumsum(rnorm(100))
y<-ts(x,start(2000,1),freq=12)
data(cars)
z<-lm(dist~+1+speed,data=cars)

plot(x)
plot(y)
plot(cars)
par(mfrow=c(2,2))
plot(z)
# plot <- generic plot.lm plot.ts; plot robi dispatching metod
methods(plot)
plot.ts
plot.lm #ukryte, z gwiazdką; oznacza obecnosc w namespacie


# Typy bazowe
typeof() #zwraca typ bazowy; opisuje strukture C

x<-1:5
typeof(x)
x<-list(x=x)
x
typeof(x)

f<-function(x) {print(x)}
f(1)
typeof(f)

x <-quote(x<-66)
x
typeof(x)
rm(x)

makeActiveBinding("x",function(){rnorm(1)},env=parent.frame())
x
x
typeof(x)

typeof(sum)

# Rozpoznawanie, szukanie i jak to dziala
# nie mozna prosto sprawdzic czy dany obiekt jest klasy S3

#przyklad
data <- data.frame(x=1:10,y=letters[1:10])
data

is.object(data)
isS4(data)

is.object(data) & isS4(data) # przyblizenie sprawdzenia czy S3

library(pryr)
otype(data)
otype(data$x) #typ bazowy
otype(data$y)

letters
typeof(letters) #character ale...
data$y #factor jest obiektem klasy S3

data1 <- list(x=1:10,y=letters[1:10])
otype(data1) #data.frame - S3, a lista - typ bazowy

#przyklad
plot #to jest generic
#jesli w kodyie jest funkcja UseMethod to generic
data.frame # tu nie ma UseMethod wiec to nie jest generic

sum #niewiadomo z takiego napisu czy to jest generic
list #tu tez niewiadomo - pisane w C

ftype(plot)
ftype(data.frame)
ftype(sum)
ftype(list)

# generic.class() - kazda metoda w S3 tak wyglada
# np. plot.lm() 
# data.frame - taka nazwa jest niekonsekwentna bo oznaczaloby ze klasa jest frame a generic data...

#przyklad
q<-data.frame(time=1:100,values=rnorm(100))
str(q)
par(mfrow=c(1,1))
plot(q)
plot.data.frame(q) # fail bo jest zdefiniowane ponizej (w pakiecie graphics)
graphics::plot.data.frame(q) #tez fail
graphics:::plot.data.frame(q) 

where("plot.data.frame",env=asNamespace("graphics"))

eval(quote(plot.data.frame(q)),envir = asNamespace("graphics")) 
q31313<-data.frame(time=1:100,values=rnorm(100))
eval(quote(plot.data.frame(q31313)),envir = asNamespace("graphics")) 

#przyklad
methods(plot)
methods(mean)
getS3method("plot","data.frame")


# Definiowanie klasy i tworzenie obiektow

#przyklad / tworzenie obiektu klasy matrix
rm(list=ls())
x<-structure(1:4,dim=c(2,2))
x
class(x)
attributes(x)

y<-list(x=1:4)
attr(y,"class")<-"myClass"
y #obiekt klasy myClass
inherits(y,"myClass") #czy dany obiekt dziedziczy z klasy myClass

#przyklad
utils::data(anorexia,package="MASS")
model<-glm(Postwt ~ Prewt + Treat + offset(Prewt), family=gaussian,data=anorexia)
class(model) #najpierw bedzie szukac dla metody glm, a jak nie znajdzie do lm

x<-data.frame(x=1:4, y=rnorm(4))
x
attributes(x)
typeof(x)
attr(x,"class") <- NULL
attr(x,"row.names") <- NULL
x
class(x)
attr(x,"class") <- "data.frame"
attr(x,"row.names") <- 1:5
class(x)
x

#przyklad
graph<-function(nodes,edges){
  #sprawdzanie poprawnosci argumentow
  a <- T
  a <-a & is.numeric(nodes)
  a <-a & (class(edges)=="matrix")
  
  #testowanie spojnosci
  temp <-sort(unique(as.vector(edges)))
  for (n in temp){
    a <- a & (n %in% nodes)
  }
  
  if(a){
    ret<-structure(list(nodes=nodes,edges=edges),class="graph")
  } else {
    warning("Edges not consistent with nodes")
  }
  ret
}

nodes <-1:4
edges<-rbind(c(1,2),c(1,3),c(2,4))
g <- graph(nodes,edges) #tworzy klase

#przyklad / Tworzenie generica
showGraph<-function(g) UseMethod("showGraph") #zdefiniowanie generica

#definiowanie defaulta
showGraph.default<-function(g){
  print("--- nie wiem co to jest ---")
  print(g)
}
#definiowanie metody dla klasy graph
showGraph.graph<-function(g){
  print("--- obiekt klasy graph ---")
  print(g$edges) #tutaj wiem, ze g ma slot edges
}

methods(showGraph)

nodes
edges
g
showGraph(edges)
showGraph(g)

plot.graph<-function(g){
  ell <- length(g$nodes)
  px <- rnorm(ell)
  py <- rnorm(ell)
  
  xrange <- range(px,py)
  yrange <- xrange
  plot(0,0,col="white",xlim=xrange,ylim=yrange,main="Graph")
  points(px,py,pch=20,cex=2)
  
  ell<-dim(g$edges)[1]
  for(k in 1:ell){
    t1<-match((g$edges[k,1]),g$nodes)
    t2<-match((g$edges[k,2]),g$nodes)
    lines(px[c(t1,t2)],py[c(t1,t2)])
  }
}
methods(plot)
plot(g)

nodes <-1:5
edges<-rbind(c(1,2),c(1,3),c(2,4),c(3,5),c(3,2),c(4,5))
g <- graph(nodes,edges) #tworzy klase
showGraph(g)
plot(g)



?groupGeneric


