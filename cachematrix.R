## These functions cache the inverse of a matrix to save computing
## time and resources.
##
## makeCacheMatrix creates a matrix and contains funcitons to: 
##    1. set the value of the matrix
##    2. get the value of the matrix
##    3. set the value of the inverse of the matrix
##    4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setmatrixinverse <- function(inverse) inv <<- inverse
  getmatrixinverse <- function() inv
  list(set = set, get = get, 
       setmatrixinverse = setmatrixinverse, 
       getmatrixinverse = getmatrixinverse)
}

## Next, the function cacheSolve returns the inverse of makeCacheMatrix.
## It will grab any cached results if available. If unavailable, it will
## compute the inverse of the matrix and cache it.cacheSolve assumes 
## that the matrix can be inverted.

cacheSolve <- function(x, ...) {
  inv <- x$getmatrixinverse()
  if(!is.null(inv)) {
    message("Good news! I've got that cached for you.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setmatrixinverse(inv)
  if(!is.null(inv)) {
    message("Let me cache that for you...")
    return(inv)
  }
  inv
}

## Here's an example:
## > x = rbind(c(1, 4/10), c(4/10, 1))
## > m = makeCacheMatrix(x)
## > m$get()
## [,1] [,2]
## [1,]  1.0  0.4
## [2,]  0.4  1.0
## 
## The following first run isn't cached so the results will be computed.
## > cacheSolve(m)
## Let me cache that for you...
## [,1]       [,2]
## [1,]  1.1904762 -0.4761905
## [2,] -0.4761905  1.1904762
##
## This second run will grab the cached results from the previous call.
## > cacheSolve(m)
## Good news! I've got that cached for you.
##            [,1]       [,2]
## [1,]  1.1904762 -0.4761905
## [2,] -0.4761905  1.1904762
## >
