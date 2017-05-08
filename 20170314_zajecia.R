#setClass() do stworzenia klasy

#definicja struktury klasy
Graph <- setClass("Graph", slots=c(nodes="numeric",edges="matrix"))
Graph
str(Graph)

#spraw co jest przypisane do zmiennej graph
typeof(Graph)
library(pryr)
ftype(Graph)

#Tworzenie poprawnego pustego obiektu
g1<-Graph()
g1
attributes(g1)
typeof(g1)
otype(g1)
# S4 to jest tak naprawde jesten z base typów

#tworzenie poprawnego obiektu z zawartoscia
nodes<-1:10
edges<-cbind(sample(nodes,10),sample(nodes,10))
g2<-Graph(nodes=nodes,edges=edges)
g2

#proba stworzenia niepoprawnego obiektu
g3<-Graph(nodes="ala",edges=matrix(1:4,2,2))
g3<-Graph(nodes=1:4,edges=list(a=3,b=4))

#przyklad
g3<-new("Graph",nodes=nodes,edges=edges)
typeof(g3)
otype(g3)
g3

#proba stworzenia niepoprawnego obiektu
g4<-new("Graph",nodes="ala",edges=edges)

#Niezlaeznie od wykorzystywanej metody zawsze sprawdzana jest poprawnosc wprowadzonych danych w sensie zgodnosci z typami danych w deklaracji struktury klasy

# dostep do slotow w klasie S4 --------------------------------------------
#przez operator '@'. Odpowiednik operatora '$' dla list i środowisk
#mozna tez przez funkcje slot(). Jest odpowiednikiem operatora '[[' dla list

g2
g2@nodes
g2@edges
typeof(g2@nodes)
typeof(g2@edges)

slot(g2,"nodes")
slot(g2,"edges")

#Typ s4 nie jest indeksowany, nie ma pierwszego slotu, drugiego...
#generalnie projektujac klasy s4 nie chcemy aby uzytkownik odwolywal sie bezposrednio do slotow
#poprzez operator @ bo to oznacza ze musi znac wewnetrzna konstrukcje klasy co nie jest korzystne

#Zamiast kazac uzytkownikowi odwoluwac sie bezposrednio do slotow nalzey zdefiniowac getters i setters aby umozliwic
#enkapsulacje. Do tego konieczne sa metody


# Metody ------------------------------------------------------------------
#podobnie jak w przypadku s3, dispatchowanie metod jest roboione przez tzw generics. Generics w zaleznosci od argumentu beda dobieraly odpowiednia metode
#Metoda jest dopinana przez funkcje setMethod()
#Nowy generic jest tworzony przez funkcje setGeneric()


#przyklad - sprawdzenie czy dany obiekt jest generics
show
ftype(show)
isGeneric("show")

plot #useMethod - czyli s3
ftype(plot)
isGeneric("plot")

hasMethod("show") #TRUE jezeli jest podpieta co najmniej jedna metoda
methods("show")

#przyklad - dopinanie metod do istniejeacego generica
showTemp <- function(object){
  cat("Objekt klasy",class(object),"\n",sep="")
  cat("Graph with ", length(object@nodes),"nodes. \n",sep="")
  ell<-dim(object@edges)[1]
  if(ell>0){
    cat("Edges ---- \n",sep="")
    ell<-dim(object@edges)[1]
    for(k in 1:(dim(object@edges)[1])){
      cat(as.character(object@edges[k,1])," -> ", as.character(object@edges[k,2]), "\n",sep="")
    }
    cat("---------- \n")
  }
  invisible()
}

#dopinanie metody do generica
setMethod("show",
          signature="Graph",
          definition=showTemp
          )
#wykorzystanie nowej metody
methods(show)
show(g1)
show(g2)
g2


#przyklad tworzenie nowego generica i dopinanie nowej metody
#tworzenie generica
setGeneric("nodes", function(object,...){standardGeneric("nodes")})
#dopinanie moetudy/getter
setMethod("nodes","Graph", function(object){object@nodes})
#wykorzystanie gettera
nodes(g1)
nodes(g2)

#tworzenie generica, gettera i wykorzystanie
setGeneric("edges", function(object,...){object@edges})
setMethod("edges","Graph",function(object){object@edges})

#przyklad subsetting, promocja do generica
#typowe wywolanie indeksowania
letters[1:3]

#funkcja dzialajaca pod spodem
'['(letters,1:3)

#dodanieawnie do funkcji '[' metody dla klasy graph
temp<-function(x,i,drop="missing"){
  .nodes<-x@nodes[i]
  ell<-dim(x@edges)[1]
  ind<-rep(T,ell)
  for (k in 1:ell){
    ind[k] <- prod(as.vector(x@edges[k,]) %in% .nodes)
  }
  as.logical(ind) -> ind
  .edges <- x@edges[ind,]
  Graph(nodes=.nodes, edges=.edges)
}

#dopinanie metody
setMethod("[","Graph",temp)

#wywowlanie subsettera/ automatycznie jest wolana metoda show!
g2
nodes(g2)
g2[1:8]

#tutaj nie bedzie wolana metoda show
g5<- g2[1:8]
nodes(g5)
edges(g5)

#jezeli chcemy wykonac subsetting dla dwoch wymiarow to wywolanie powinno byc postacji function(x,i,j,drop=)



# Poprawnosc/ walidacja ---------------------------------------------------

#chcemy dodac mechanizm sprawdzajacy czy przy tworzeniu graphu nie pojawi sie sytuacja gdy nie bedzie krawedzi
#setValidity()
#def metody
temp <- function(object){
  val<-T
  warn<-NULL
  if( !nrow(edges(object))>0){
    val<-F
    warn <- "Brak zdefiniowanych krawedzi"
  }
  if ( nrow(edges(object))>0){
    if(prod(as.vector(edges(object)) %in% nodes(object)) != 1){
      val <- F
      warn<- "Krawedzie pomiedzy nieistniejacych wierzcholkami"
    }
  }
  
  if(val){
    TRUE
  } else {
    warn
  }
}

#przypiecie metody
setValidity("Graph",temp)

#sprawdzenie czy istniejace grafy sa dobre
validObject(g2)
validObject(g1)

#proba stworznia grafu, ktory nie jest poprawny
g6<-Graph(nodes=1:4, edges=rbind(c(1,2),c(2,5)))


# Mutacja / setters ------------------------------------------------------
#setters
#przyklad = dlaczego nie chcemy bezposrednio uzywac @
#tworzenie popraewnego obiektu
tnodes<-1:10
tedges<-cbind(sample(tnodes,10),sample(tnodes,10))
g7<-Graph(nodes=tnodes,edges=tedges)
g7

# proba zmiany wartosci slotow bezposrednio
g7@nodes<-"ala"
g7@edges<-1:6

#delikatniejsza zmiana
g7@edges<-matrix(20:23,2,2) #blednie stworzony obiekt
validObject(g7)

#dlatego chce stworzyc settery
#settery to specjalna rodzina metod dla danego obiektu, z tego powody podleaja pewnym regulom
#kazdy setter jest postaci slot(object) <-, gdzie slot to nazwa slotu a object jest obiektem odpowiedniej klasy.
#Przykladowo setter dla slotu nodes bedzie postaci nodes<-.

#przyklad budujacy intuicje
x<-1:5
x[1]
x[1] <- 10
x
'[<-'(x,1,50)
x
'[<-'(x,1,50) ->x
x

#kazdy  setter zawsze przyjmuje co najmjnije dwa argumenty

#def generica
setGeneric("nodes<-", function(object,value){standardGeneric("nodes<-")})

#def metody.w metodzie zakladamy ze mozna jedynie zmienoic wezly ktore nie maja krawdzi lub dodac wezly
temp<- function(object,value){
  object@nodes <- value
  if(validObject(object)){
    return(object)
  }
}

#przypiecie metody do generica
setMethod("nodes<-","Graph",temp)
g3<-graph(nodes=1:10,edges=cbind(sample(10),sample(10)))

nodes(g3)
edges(g3)




slotNames(g3)
class(g3)
getClass("Graph")
showMethods(classes="Graph")

#kod dla konkretnej metody
findMethod(f="nodes", signature="Graph")
showMethods("nodes")
getMethod("nodes","Graph")

findMethod(f="nodes<-", signature="Graph")

isS4(g3)
otype(g3)
is(g3) # z czego dziedziczy obiekt
ftype(edges)
ftype(Graph)

#wszystkie generics dla modelu s4
getGenerics()
getClasses(where=globalenv())
getClasses(where=globalenv(), inherits=T)


# Dziedziczenie -----------------------------------------------------------
#w modelu s4 klasa moze dziedziczyc z modelu s4, s3 oraz base type. W dwoch ostatnich przypdkach klasa zawiera slot .Data, ktory
#zawiera wartosc w base type dla klasy s3 albo ten, z ktorego bezprosednio dziedziczy klasa

#z czego dziedzicyz klasa wskazuje sie przez argument contains, ktory jest wektorem stringow okreslajacych z czego dziedziczy
#klasa. Wektor ten jest co najmniej 1 elementowy ale moze zawierac wiecej niz jeden element.

#przyklad
DataStructure<- setClass("DataStructure",
                         slots= list(data="data.frame")
)
getClass("DataStructure")

DataStructureWithDesc <- setClass("DataStructureWithDesc",
                                  contains="DataStructure",
                                  slots=list(desc="character"))

getClass("DataStructureWithDesc")

#tworzenie obiektow
a<- DataStructure(data=data.frame(a=1:10, b=rnorm(10)))
b<-DataStructureWithDesc(data=data.frame(a=1:10, b=rnorm(10)),desc=" data from some source...")

a
b

#dodawanie metody dla klasy datastructure
temp<-function(object){
  cat("DataStructure object\n")
  cat("number of variables: ", dim(object@data)[2],"\n",sep="")
  cat()
  ...
}



# wiecej info -------------------------------------------------------------

?Classes
?setOldClass
logi.sysbio.com.ac.uk/teaching/advancedR/slides.pdf
stackoverflow.com/questions/12709933/adding-s4-dusoatch-to-base-r-s3-generic
bioConductior biblioteka - jesli chce sie programowac w S4

Rzeczy pominiete:
  -dispatchowanie na podstawie wielu argumentow
- dziedziczenie z wielu klas



library(pryr)

#tworzenie prostej klasy s4
DataStruct <- setClass("DataStruct", slots = list(values="data.frame", info = "character"))

#jak wyglada konstruktor
ftype(DataStruct)
typeof(DataStruct)
environment(DataStruct)
where("DataStruct")

DataStruct

temp <- function(object){
  cat("object of class: ", class(object),"\n")
  cat("no of obs: ", as.character(dim(object@values)[1]),"\n")
  cat("no of feature: ", as.character(dim(object@values)[2]),"\n")
  cat("info: ", object@info,"\n")
  e<-environment()
  print(list("execution"))
}


setGeneric("values", function(object,...){standardGeneric("values")})

temp<- function(object,...){
  e<-environment()
  eParent <-parent.env(e)
  eCalling<-parent.frame()
  list("execution"=e, "enclosing"=eParent, "calling"=eCalling)
}
setMethod("values","DataStruct",temp)

#tworzenie obiektu
x<-DataStruct(values=data.frame(pos=1:3,val=rnorm(3)), info="Exemplary data")

ftype("values")
isGeneric("values")
typeof(values)
values

#gdzie jest generic w sensie srodowiska
where("show")
temp<-list(environment(show))
while(!identical(emptyenv(), temp[[length(temp)]])){
  temp <- c(temp, list(parent.env(temp[[length(temp)]])))
}
temp[1:5]  


#jak widac standardowy generic show ma enclosing env, ktory jest losowym stodowiskiem. Jest to stodowisko
#execution dla funkcji setGeneric, ktora tworzy generica. jej enclosing env jest srodowisko namespace::methods
environment(setGeneric)
identical(environment(setGeneric), temp[[2]])

#gdzie jest metoda show podpieta pod tego generica
x


#jak widac jej enclosing enc to global (tam zostala zbudowana) ale jej caling env to juz losowe srodwisko, ktore jest execution
#env stworzone przez standardGeneric(). Sciezke przeszukiwania dla metody show dispatchowanej przez generica mozna sprawidzic
#modyfikujac genrica

temp<-function(object){
  cat("object of class: ", class(object),"\n")
  cat("no of obs: ")
}