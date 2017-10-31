## MakeCacheMatrix generates a special "matrix", a list that both sets 
## the value of the matrix and its inverse, as well as retrieving those values

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(m) {
    x <<- m
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" of 
## makeCacheMatrix above. In case the inverse would bve calculated, 
## the function should get the inverse from the cache

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

