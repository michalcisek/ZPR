# funkcja ma:
# -cialo - body()
# - formals - formals()
# - srodowisko - enclosing

#pairlist - czysty list()

f <- function(x="FOO"){
  print(x)
}

formals(f)
body(f)
typeof(f)
alist() #do tworzenia pairlist

attributes(f)

f<- function(){}
typeof(closure)

formals(f) <- alist(x="Mary")
body(f) <- quote({print(x)})
body(f)
formals(f)
attributes(f)
f()
f("This id")

# jesli funkcja jest primitive to nie zwroci ani body ani formals (bo nie ma w niej jezyka R tylko C)
c
formals(c)
body(c)

#tworzenie listy wszystkich funkcji w pakiecie base
obs <- mget(ls("package:base"), inherit = T)
obs
fs <- Filter(f=is.function, x=obs)
length(fs)

#tworzenie wszystkich funkcji primitive
fsPrimitive <- Filter(f=function(x) is.primitive(x) & is.function(x), x=obs)
length(fsPrimitive)


library(pryr)
ast(2+2) #abstract syntax tree
ast(mean(rnorm(100)))
ast((2+2))


#przedefiniowanie funkcji `(`
'(' <- function(x) x+1
(2+1) #evil
rm('(')
(2+1)


ast(for(k in 1:10) print(k))

#proste wykorzystanie przy programowaniu funkcyjnym
sapply(1:10, `+`, 3)

x <- list(1:3, 4:9, 5:13)
sapply(x, `[`, 2)


#wywolanie funkcji na argumentach - do.call

#lazy computations
f1 <- function(a,b){
  print(a)
  print(nchar(b))
}
f1('ala', print("john")) #tutaj wyrazenie jest obliczone

f2 <- function(a,b){
  print(a)
}

f2('ala', print('john')) #tutaj drugi argument nie jest obliczany bo nie jest wykorzystany

#dziwne rzeczy
f1 <- function(a = 1, b=2*a){
  c(a,b)
}

f1(2)
f1(2,5)

f2 <- function(a=1, b=2*x){
  x <- 2*a
  c(a,x,b)
}
f2(3)


#wymuszanie obliczen/ forcing
f <- function(x=stop("Evaluated")){
  123
}

f() #argument nie zostal obliczony

###wymuszanie obliczen
f <- function(x = stop("Evaluated")){
  force(x)
  123
}

f() #argument zostal obliczony

f<-function(x){
  function(y) x+y
}

g2<- f(2)
g4<- f(4)

g2(10)
g4(10)

#proba stworzenia listy funkcji
q1 <- lapply(1:10, f)
q1[[10]](10)
q1[[1]](10)
for(k in 1:length(q1)) print(q1[[k]](10))


#gdzie sa obliczane argumenty
# domyslnie wewnatrz srodowiska execution. Jesli podawana jest wartos explicite to moze ona byc inna nic wartosc domyslna

f <- function(x = ls()){
  a <- 123
  x
}
rm(x)
x
f() #wartosc domyslna obliczona jest w exevution env
f(ls()) #wartosc podana z calling env


#arumenty podawane w spodob  bezposredni sa obliczane w srodowisku enclosing. 
ne <- new.env(parent = globalenv())

x<- 10
ne$x <- 1

f<- function(q){
  print(q)
}
environment(f) <- ne

#binding env dla f to global env()
f(x)
eval(quote(f(x)), envir = ne)

#Argumenty podane jawnie sa obliczane w srodowisku calling

rm(ne)


#promises, promises ...
# nie obliczone wyrazenie nazywa sie "obietnica" promise. Zawiera dwa elementy
# - wyrazenie ktore bedzie obliczne jezeli dojdzie do obliczenia / mozna sie od niego dostac przez funkcje substitute()
# - srodowisko gdzie wyrazenie zostalo stworznie i gdzie powinno byc obliczone jezeli dojdzie do obliczenia

#po pierwszym obliczeniu obietnica nie jest powtorznie obliczna.  wiecej info: pryr::promise_info()

x<- NULL
x >0

#tu sprawdza tylko pierwszy warunek, on nie spełniony wiec nie idzie dalej
if(!is.null(x) && x>0){
  print("Works?")
}


#tutaj blad bo sprawdza cale wyrazenie
if(is.null(x) && x>0){
  print("doesnt work")
}

#wykorzystanie nie podanych argumentow
#przy pomocy funkcji missing() mozna wykryc czy dany argument byl podany

f <- function(a,b){
  c(missing(a), missing( b))
}
f(a=1, b=1)
f(a=1)
f()


#typowe wykorzystanie to zapewnienie wartosci domyuslnyucj jezeli wymaga to skomplikowanych obliczen
f <- function(x){
  if(missing(x)){
    cat("Value was not provided. Default: ", 0, "\n")
  } else {
    cat("provided value: ", x,"\n")
  }
}
f()


#argument ... i co z nim zrobic
f <- function(...){
  q <- list(...)
  cat("there are: ",length(q),"\n")
  if(!identical(names(q), NULL)) names(q)
}

f(a=10)


#specjalne wywoalania
#R wspiera dwa typy specjalnych wywolan: infix operators oraz replacement functions

x<-1:5
address(x) #hash gdzie to w pamieci siedzi


#on.exit / wygodne sprzatanie
#warto przeczytac help do tej funkcji



#Obliczenia na jezyku
#podstawowa funkcja ktora pozwala na wykonywanie oobliczen na jezyku to substiute(). Funkcja ta
#zamiast zbierac wartosci argumentow, zbiera kod ktory je oblicza
f <- function(x){
  substitute(x)
}

f(1:10)
typeof(f(1:10))

x<-10
f(x)
eval(f(x))

g <- function(x){
  print(typeof(substitute(x)))
}
g(2)
g(x<-2)


#druga przydatna funkcja to deparse(). Funkcja bierze obiekt typu language i zamienia go na string

q <- quote(x<-1:5)
q
deparse(q)

f <- function(s) deparse(substitute(s))
f(x <- 1:10)
f(function(s) s^2)

#polaczenie quote i eval
#Drugi argument eval to albo srodowisko albo lista czy ramka danych
eval(quote(x), list(x=100))
x

#obliczenia w ramce danych
eval(quote(x), data.frame(x=1:10))

#obliczenia w srodowisku
q <- new.env(parent = globalenv())
q$x <- rnorm(10)
eval(quote(mean(x)), q)
rm(q)



subset2 <- function(data, cond){
  condCall <- substitute(cond)
  ind <- eval(condCall, data)
  data[ind,]
}

data <- data.frame(pos = 1:10, val= rnorm(10))
data
subset2(data, cond = val>0)
subset2(data, cond = val>0 & val<1)


#Rdz 13, 4 - Advanced R