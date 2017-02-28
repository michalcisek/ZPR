#enclosing - zawsze jest jednoznaczny, srodowisko w ktorym f zzostala stworzona; okresla gdzie funkcja 
#zaczyna przeszukiwanie
#binding - srodowisko wiazania; po to zeby f mozna bylo znalezc
#execution
#calling

install.packages("pryr")
library(pryr)
where("where")
environment(where)

#przyklad
rm(list=ls())
ne <- new.env()
attr(ne,"name")<-"ne"
environmentName(ne)
ne$ff<-function(x){x^2}
ff(2)
ne$ff(2)
eval(quote(ff(2)),envir=ne)
ls.str(envir=ne)
eval(quote(environment(ff)),envir = ne)
eval(quote(where("ff")),envir = ne)
rm(ne)

#przyklad
ne <- new.env()
attr(ne,"name")<-"ne"

x<-0
ne$x<-10

eval(quote(f<-function(){print(x)}),envir=ne)
ne$f()

rm("x",envir=ne)
ne$f()
#funkcja zaczyna przeszukiwac od srodowiska enclosing a nie od tego w ktorym jest wywolywana
rm(ne)

#przyklad
ne <- new.env()
attr(ne,"name")<-"ne"

ne$x<-10
x<-5
f<-function(){
  print(x)
}

where("f")#binding srodowiska
environment(f) #enclosing 
f() #szuka w global
environment(f)<-ne
environment(f)
where("f")
f()
rm("x",envir=ne)
f() #inne wywolanie niz na poczaktu, to szuka najpierw w ne
rm(ne)


#execution - srodowisko ulotne, tworzone wtedy kiedy funkcja jest wywolana
#przyklad
g<-function(x){
  if(!exists("a",inherits = T)){
    a<-1
  } else {
    a<-a+1
  }
  a
}
ls()
g(43543)
rm(a)
a<-3
g(2)


#przyklad
ne <-new.env()
attr(ne,"name")<-"ne"

g<-function(){
  exe<-environment()
  exeParent<-parent.env(exe)
  list(exe,exeParent)
}
g()

environment(g)<-ne
g()
where("g")

ne$g2<-g#zmieniamy binding
ne$g2()
g()
eval(quote(where("g2")),envir=ne)
rm(g)
rm(ne)
#execution jest zawsze podpiete pod enclosing

#przyklad - co pamieta funkcja
ne<-new.env()
attr(ne,"name")<-"ne"

ne$x<-123
g<-function(){print(x)}
g()
environment(g)<-ne
g()
rm(ne)
g()
mmm<-environment(g) 
mmm$x<-555
g()
typeof(g) #closure - obiekt + srodowisko enclosing


#przyklad
createMultiply<-function(x){
  y<-"adada100"
  function(y){x*y}
}
#fabryka funkcji,tworzy funkcje
f1<-createMultiply(2)
f1(5)
f2<-createMultiply(5)
f2(5)
q1<-environment(f1)
q1$y
q1$y<-500
q2<-environment(f2)
q1$y
q2$y


#przyklad
createFunction<-function(){
  x<-2
  function(y){x*y}
}
f1<-createFunction()
f1(4)
q1<-environment(f1)
q1$x<-5
f1(4)

# parent.frame zwraca srodowisko calling
#przyklad
g<-function(){
  print(parent.frame())
}
g()
environment(g)
where("g")

#przyklad
ne1<-new.env()
attr(ne1,"name")<-"ne1"
ne2<-new.env(parent=ne1)
attr(ne2,"name")<-"ne2"

ne1$f<-function(){
  e<-environment()
  list(
    `execution`=e,
    `enclosing`=parent.env(e),
    `calling`= parent.frame(),
    `binding`=where("f",env=ne2)
  )
}

environment(ne1$f)<-ne2
ne1$f()

#pd - funkcja ktora pamieta ile razy jest wywolywana. zeby mozna bylo zapamietac stan zmiennych pomiedzy wywolaniami

