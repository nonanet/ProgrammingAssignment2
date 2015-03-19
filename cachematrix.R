## Two functions creating a "matrix" object that can cache the matrix' 
## inverse. The "object" itself implements a set of getter/setter 
## methods for the matrix as well as the inverse, while the actual 
## calculation is performed in a second function making use of that object

## makeCacheMatrix creates a list of the four functions required to 
## get/set the matrix itself, as well as the inverse. This does not 
## do any actual calculations
makeCacheMatrix <- function(x = matrix()) {
  
  ## inv: cached inverse value, initialized to NULL
  inv <- NULL
  
  ## setter function for the matrix, re-set cached inverse to NULL on assignment
  ## since cached value is invalid when matrix changes
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## getter function for the matrix itself 
  get <- function() {
    x
  }

  ## setter function for the inverse value, does not do calculations
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## getter function for the inverse value, does not to calculations
  getinverse <- function() {
    inv
  }
  
  ## return list of functions/methods 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Caches the inverse (Solve) of a matrix using the makeCacheMatrix object
## x is an instance of such a cacheable matrix object (created by the 
## makeCacheMatrix function, see above)
cacheSolve <- function(x, ...) {
  ## attempt fetching a cached value, and return it if it exists
  i <- x$getinverse()
  if (!is.null(i)) {
    message("cache hit")
    i
  }
  ## otherwise, calculate inverse, cache result, and return it
  data <- x$get()
  i <- Solve(data, ...)
  x$setinverse(i)
  i
}

