##Prepares a cached matrix for use with cacheSolve.

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
}

##Returns a matrix that is the inverse of 'x'.  If the inverse has already been calculated and cached
##cacheSolve will read the cache in place of re-running the solve function

cacheSolve <- function(x, ...) {

    m <- x$getinverse()
    if(!is.null(m)) {
        message("Getting cached data...")
        return(m)
    }
  
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
  
}
