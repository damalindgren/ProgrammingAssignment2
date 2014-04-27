## These functions work together to cache an inverse of a matrix to speed up
## computing

## This function creates the object and the list of functions that is 
## connected to it
makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
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


## This function checks to see if the inverse is already cached
## otherwise it computes it
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
