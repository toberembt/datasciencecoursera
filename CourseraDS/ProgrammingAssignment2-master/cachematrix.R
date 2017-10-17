## A pair of functions that cache the inverse of a matrix
## the below function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x<<-y
    i<<-NULL
  }
  get <- function() {
    x
  }
  setInverse <- function(inverse) {
    i <<- inverse
  }
  getInverse <- function() {
    i
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The below function computes the inverse of the special "matrix" returned by the makeCacheMatrix above.
##  If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
##  retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if( !is.null(i) ) {
    message("getting the cached data")
    return(i)
  }
  data <- x$get()
  i<- solve(data) %*% data
  x$setInverse(i)
  i
}
