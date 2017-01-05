## Put comments here that give an overall description of what your 
## functions do:
## The overall purpose of this pair of functions is to cache the inverse of a 
## matrix rather than compute it repeatedly if the matrix argument is not
## changing.  


## Write a short comment describing this function:
## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x=matrix()) {
  i <- NULL
  set <- function(y) {
    x<<-y
    i<<-NULL
  }
  get <- function()x
  setinverse <- function(inverse) i <<-inverse
  getinverse <- function()i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function:
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve retrieves the inverse 
## from the cache.


cacheSolve <- function(x,...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}




