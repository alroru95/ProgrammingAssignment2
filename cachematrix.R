###RProgramming: Assignment 2

##This function creates a special "matrix" object that can cache its inverse.
##The <<- operator is used to assign a value to an object in an environment different from the current environment.
##It has been assumed that our matrix supplied is always invertible. That's why the inv has been associated to NULL

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(solveMatrix) (inv <<- solveMatrix)
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix function. 
##If the inversed matrix has already been calculated and didn't change, the result is retrieved from cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv 
}
