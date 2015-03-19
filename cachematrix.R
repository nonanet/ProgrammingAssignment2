## Two functions creating a "matrix" object that can cache the matrix' 
## inverse, by means of four getter/setter functions for the matrix and the 
## means value, respectively

## makeCacheMatrix creates a list of the four functions required to 
## get/set the matrix itself, as well as the inverse. This does not 
## do any actual calculations, but 

makeCacheMatrix <- function(x = matrix()) {
  
  ## inverse value, initialized to NULL
  inv <- NULL
  
  ## setter function for the matrix, re-set cached inverse to NULL on assignment
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## getter function for the matrix itself 
  get <- function() {
    x
  }

  ## setter function for the inverse value
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## getter function for the inverse value
  getinverse <- function() {
    inv
  }
  
  ## list of functions as return value
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Caches the inverse (Solve) of a matrix using the makeCacheMatrix object
## x is an instance of such a cacheable matrix object (created by the 
## makeCacheMatrix function, see above)
cacheSolve <- function(x, ...) {
  ## attempt fetching a cached value
  i <- x$getinverse()
  if (!is.null(i)) {
    message("cache hit")
    i
  }
  ## otherwise solve the matrix, cache and return result
  data <- x$get()
  i <- Solve(data, ...)
  x$setinverse(i)
  i
}

