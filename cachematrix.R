## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Week3 Assignment

makeCacheMatrix <- function(x = matrix()) {
  ins<- NULL
  set<- function(y){
    x<<- y
    ins<<-NULL
  }
get<-function()x

setinverse<- function(inverse)ins<<-inverse
getinverser<- function()ins
list(set= set, get= get, setinverse=setinverse, getinverse=getinverse)

}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##if the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ins<-x$getinverse()
  if(!is.null(ins)){
    message("getting cached data")
    return(ins)
    
  }
  data<-x$get
  ins<-solve(data, ...)
  x$setinverse(ins)
  ins
}