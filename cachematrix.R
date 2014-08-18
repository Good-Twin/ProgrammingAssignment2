## makeCacheMatrix creates matrix that saves its inverse in another Cache. THe cached value can be retrieved 
## by cacheSolve() which gets the inverse from the cache or calculates if the inverse does not exist in the cache.

## Creates a matrix which can cache its inverse. Return value is a list of functions which can be used getting 
## and setting the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns the inverse of a cached matrix or calculates the inverse if the inverse is Null.
cacheSolve <- function(x, ...) {
   i <- x$getinverse()
   if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
} 
