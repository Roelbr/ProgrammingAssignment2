
## These two functions enable the caching of an inverse matrix.

## This function creates a matrix that can cache the inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinv <- function(inverse) inv <<- inverse
     getinv <- function() inv
     list(set=set, get=get,
          setinv=setinv,
          getinv=getinv)
}


## This function computes the inverse of the matrix returned by the makeCacheMatrix() function. 
## If the inverse has been calculated already the matrix is retrieved from the cache.

cacheSolve <- function(x, ...) {
inv <- x$getinv()
     if(!is.null(inv)) {
          message("Fetching cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data,...)
     x$setinv(inv)
     return(inv)
 }
