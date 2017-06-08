## The following set of functions creates a matrix along with a set of functions and objects that are 
## used to calculate the inverse and store it in cache. This way it can be stored for use in subsequent 
## functions or calculations, rather than calculating the inverse each time. Note that the matrix must be 
## square in order to calculate an inverse.

## The function makeCacheMatrix results in a matrix and contains 4 functions (set, get, setinverse, getinverse)
## and 2 data objects (x, m)


makeCacheMatrix <- function(x = matrix(), nrow= 0, ncol=0) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function cacheSolve calculates the inverse of the matrix created by makeCacheMatrix (and then caches it),
## or retrieves the inverse if already cached

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
