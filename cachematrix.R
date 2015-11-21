## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     
     set <- function(y){
          x <<- y
          inv <<- NULL
     }
     
     get <- function() { x }
     
     setInverse <- function(matrixInverse) { inv <<- matrixInverse }
     getInverse <- function() { inv }
     
     cachingFunctions <- list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(cachingFunctions, ...) {
     inv <- cachingFunctions$getInverse()
     
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     
     data <- cachingFunctions$get()
     inv <- solve(data, ...)
     
     cachingFunctions$setInverse(inv)
     inv
        ## Return a matrix that is the inverse of 'x'
}
