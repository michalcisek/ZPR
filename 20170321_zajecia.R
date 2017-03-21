# model R5


#definiowanie klasy

a<-setRefClass("student")
otype(a)
ftype(a)
typeof(a)
str(a)

#definicja zawerajaca pola, bez metod, zwraca konstruktor

student<-setRefClass("student",
                     fields=list(name="character", age="numeric",id="numeric")
)

#jak wyglada generator?
student

#przykladowe wykorzystanie predefiniowanej metody getrefclass()
student$getRefClass()

#tworzenie przykladowego obiektu
s1 <- student(name="Mike", age=20, id=52034)
otype(s1)
typeof(s1) #typ bazowy to s4
attributes(s1)

#jak widac mamy atrybut .xData, ktorego wart jest srodowisko. Co zawiera to srodowisko?
ls(envir=attributes(s1)$.xData)

#jak widac to srodowisko zawiera dwa nasze pola oraz dwie dodatkowe info. spr czy rzeczywiscie jest to to co zdefiniowalismy
e <- attributes(s1)$.xData

e$name
e$age
e$id

e$getClass #check~!

#co tak naprawde zawiera srodowisko e?
ls(envir=e, all.names=T)

#jak widac tutaj zapisane sa metody settery/gettery poza tym pojawia sie zmiana refClassDef
e$.refClassDef
typeof(e$.refClassDef)
attributes(e$.refClassDef)
class(e$.refClassDef)

#metoda getclass ma enclosing enc ustawione na srodowisko zapisane w atrybucie klasy i dlatego widzi zmienna .refClassDef
identical(environment(e$getClass),e)

parent.env(e)

s1$getClass()


#przyklad gettery/settery

s1$name
s1$age

#wykorzystywanie setter
s1$name <- "John"
s1$name
sr(s1)

#podstawowa walidacja typow
s1$id<-"wrong type"



#obiekty r5 sa mutowalne
#wszystkie elemtny sa przechowywane w srodowisku

a<-list("x"=1, "y"=2)
b <- a

b$x <-100

identical(a,b)
str(a)
str(b)

#w tej chwili istnieje juz obiekt s1 klasy student
str(s1)
s2 <- s1 #tutaj nie ma kopii, przekzazuwanie przez referencje
identical(s1,s2)

s1$name
s2$name
s2$name<-"Mary"
s2$name
s1$name


#tworzenie kopii obiektu R5
#automatycznie jest tworzona metody copy() dla kazdej klasy R5

s2

s3 <- s2$copy()

s2$name
s2$name<-"anne"
s2$name
s3$name


#metody (internal)

#jak wygladaja dziedziczone metody
student

student$copy
student$field
student$show

s2$show()

#podstawowe ingo o klasie
getRefClass("student")
student

#pozyskiwanie info
student$help()
student$help("field")
student$help("usingMethods")

?setRefClass


# Tworzenie wlasnych metod
student<-setRefClass("student",
                     fields=list(name="character", age="numeric", id="numeric"))
student$methods(
  ageInc = function(x=1){age<<-age+x},
  ageDec = function(x=1){age<<-age-x}
)

s1 <-student(name="John",age=20)
s1$ageInc()
s1$ageDec()


#metody (external)

#dziewdziczenie

#przklad
