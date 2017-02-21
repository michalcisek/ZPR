getwd()
install.packages("pryr")
library("pryr")

# 4 podstawowe srodowiska: 
emptyenv()
baseenv() # zawsze podpiete pod emptyenv; definiowane np typy bazowe
# tu są pakiety
globalenv() # srodowisko w ktorym pracujemy
# tu są namespace'y
environment() # bez argumentu zwraca nam biezace srodowisko

search() # do przeszukiwania srodowisk; 

# przyklad:
envs <- search()
x <- 123
as.environment(envs[1])
as.environment(envs[1])$x

#jak wgrywamy biblioteke to jest dolaczana zaraz po .GlobalEnv

parent.env() #zwraca rodzica, w argumencie srodowisko
# przyklad:
envs<-search()
parent.env(parent.env(environment()))

# przyklad:
envs<- list(environment())
while( !identical(emptyenv(),envs[[length(envs)]])){
  envs <- c(envs,list(parent.env(envs[[length(envs)]])))
}
envs

# zadanie: zaimplementowac powyzsza funkcje jako funkcje rekurencyjna
listEnvs <- function(e = list(globalenv())){
  env <- parent.env(e[[length(e)]])
  
  if( identical(emptyenv(), env)){
    ret <- c(e, list(env))
  } else {
    ret <- c(e, list(env))
    ret <- listEnvs(ret)
  }
  return(ret)
}


# Tworzenie srodowisk -----------------------------------------------------

new.env()

# przyklad:
ne <- new.env()
attr(ne, "name") <- "ne"
environmentName(ne)
eval(quote(environment()),ne)
parent.env(ne)

x <-123
eval(quote(x <-321),ne)
ls(env=ne)
ls.str(env=ne)
rm(ne)

# przyklad:
ne <- new.env()
attr(ne,"name")<- "ne"
ne$x<-1
ne$y<-2
ne$.a<-555
ls(ne) # nie ma .a
ls(ne, all.names = T) # jest

ne$x
ne[["x"]]
get("x",env=ne)

x<-123
get("x",env=ne, inherits = T)
rm("x", envir=ne)
get("x",env=ne, inherits = T)

exists("x",envir = ne, inherits = F)
ne$x

where("x")
where("where")

ne <- new.env()
attr(ne,"name")<-"ne"
ne$x<-1
x
where("x")

eval(quote(where("x")),envir = ne)

rm("x",envir=ne)
eval(quote(where("x")),envir = ne)
rm(ne)

rm(list=ls())
ls()
q<-quote(x<-123)
ls()
typeof(q)
eval(q)
ls()
x

#zadanie: zaimplementuj wlasna funkcje where().
myWhere<-function(var, env=parent.frame()){
  if(identical(var,emptyenv())){
    stop("Nie ma takiej zmiennej", var, call. = F)
  } else {
    if(exists(var, envir = env, inherit=F)){
      env
    } else {
      myWhere(var, parent.env(env))
    }
  }
}
myWhere("where")

#zadanie: zaimplementuj where ktore zwraca wszystkie wystapienia danej zmiennej. 
#Implementacja ma być rekurencyjna

#zadanie: zaimplementuj where ktore zwraca wszystkie wystapienia danej zmiennej. 
#Implementacja ma nie być rekurencyjna

#zadanie: zaimplementuj where ktore zwraca wszystkie wystapienia danej zmiennej. 
#zwraca wynik jedynie wtedy kiedy zmienna jest lista

#zadanie: pomysl o super where(); bierze nazwe, srodowisko, inherit i ma zwracac
#zmienna tylko o okreslonym typie