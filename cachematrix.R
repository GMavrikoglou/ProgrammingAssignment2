## The two functions makeCacheMAtrix and cacheSolve are functons that store  retrieve 
## and calculate the inverse matrix of a matrix.

## makeCacheMatrix is a function that contains four functions(set ,get,setsolve ,getsolve)
##  for geting and storing  an object .The input of this function is a matrix

makeCacheMatrix <- function(x = matrix()) {
        s<-NULL
        set<-function(y) {
                x<<-y
                s<<-NULL
        } 
get<-function() x
setsolve<-function(solve) s <<-solve
getsolve<- function() s
list( set=set,get=get,
      setsolve=setsolve,
      getsolve=getsolve)
}

## cacheSolve function retrieves the cached inverse matrix of x if any -printing  
## a message that already exists- , otherwise it calculates the input(makeCacheMatrix) and
## stores its value . 

cacheSolve <- function(x, ...) {
        s<-x$getsolve()
        if(!is.null(s)){
                message("getting cached data")
                return(s)
        }
        data<-x$get()
        s<-solve(data,...)
        x$setsolve(s)
        s
}

