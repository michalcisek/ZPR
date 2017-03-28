# model r6


install.packages("R6")
library(R6)

# nie jest oparty o model s4

#tworzenie klasy
Person <- R6Class("Person",
                  public = list(
                    name = NULL,
                    hair = NULL,
                    initialize = function(name= NA, hair=NA){
                      self$name <- name
                      self$hair <- hair
                      self$greet()
                  
                    },
                    set_hair = function(val){
                      self$hair <- val
                    },
                    greet = function(){
                      cat(paste0("My name is ", self$name, "\n"))
                    }
                  ))


ann <- Person$new(name="Ann", hair="red")
ann

# elementy public/ dostep i ustawianie

ann$hair
ann$name
ann$greet()
ann$set_hair("purple")
ann$hair

typeof(ann)
ls(envir=ann)
parent.env(ann)

## zdjecie

Person <- R6Class("Person",
                  public = list(
                    name = NULL,
                    hair = NULL,
                    initialize = function(name= NA, hair=NA){
                      self$name <- name
                      self$hair <- hair
                      self$greet()
                      
                    },
                    set_hair = function(val){
                      self$hair <- val
                      envs <- list(environment())
                      while(!identical(emptyenv(), envs[[length(envs)]])){
                        envs <- c(envs, list(parent.env(envs[[length(envs)]])))
                        envs
                      }
                    },
                    greet = function(){
                      cat(paste0("My name is ", self$name, "\n"))
                    }
                  ))

john <- Person$new(name="John", hair = "blue")

q1<-john$set_hair("red")
q2<-john$greet()

q1[1:3]
q2[1:3]


ls(envir=q1[[2]])

eval(quote(typeof(self)), envir = q1[[2]])


eval(quote(ls(envir = self)), envir = q1[[2]])
ls(envir=john)


# Elementy private

Queue <- R6Class("Queue",
                 public = list(
                   initialize = function(...){
                     for(item in list(...)){
                       self$add(item)
                     }
                   },
                   add = function(x){
                     private$queue <- c(private$queue, list(x))
                     invisible(self)
                   },
                   remove = function(){
                     if(private$length() ==0) return(NULL)
                     head <- private$queue[[1]]
                     private$queue <- private$queue[-1]
                     head # zwraca usunitey element
                   },
                   show =function(){
                     private$queue
                   }
                 ),
                 private = list(
                   queue=list(),
                   length= function() base::length(private$queue)
                 )
                 )

q <- Queue$new(1,2,T,"John")
q
# elementy publiczne wywoiluje sie zgfodnie ze skladnia self$foo()

q$show()
q$add(1+01)
q$show()
q$remove()
q$show()



#wykorzystanie prywatnych elementow - fail
q$queue
q$length()

#wykorzystanie chainingu
q$show()
q$remove()
q$show()
q$add(1)$add("last")$add("day")
q$show()


#Elementy active /active binding

CrazyString <- R6Class("CrazyString",
                       public = list(
                         x="CrazyString"
                       ),
                       active = list(
                         xActive = function(val){
                           if(missing(val)){
                             return(self$x)
                           } else {
                             self$x <- val
                           }
                         },
                         randomCharacter = function(){
                           ell <- nchar(self$x)
                           ell <- sort(sample(1:ell, 2))
                           substr(self$x, ell[1], ell[2])
                         }
                       )
                       )


q <-CrazyString$new()
q$x
q$xActive
q$xActive <- "Here comes johnny!"
q$xActive
q$randomCharacter


# Dziedziczenie
#klasy r6 moga dziedziczyc z klas r6


Queue <- R6Class("Queue",
                 public = list(
                   initialize = function(...){
                     for(item in list(...)){
                       self$add(item)
                     }
                   },
                   add = function(x){
                     private$queue <- c(private$queue, list(x))
                     invisible(self)
                   },
                   remove = function(){
                     if(private$length() ==0) return(NULL)
                     head <- private$queue[[1]]
                     private$queue <- private$queue[-1]
                     head # zwraca usunitey element
                   },
                   show =function(){
                     private$queue
                   }
                 ),
                 private = list(
                   queue=list(),
                   length= function() base::length(private$queue)
                 )
)


HistoryQueue <- R6Class("HistoryQueue",
                        inherit = Queue,
                        public = list(
                          show=function(){
                            cat("Next item")
                            ............
                          }
                        ))


# portable and non-portable class