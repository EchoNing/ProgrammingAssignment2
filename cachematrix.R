## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	cachedMatrix <- NULL
    set <- function(y) {
        x <<- y
        cachedMatrix <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) cachedMatrix <<- solve
    getinverse <- function() cachedMatrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    cachedMatrix <- x$getinverse()
    if(!is.null(cachedMatrix)) {
        message("getting cached data")
        return(cachedMatrix)
    }
    data <- x$get()
    cachedMatrix <- solve(data, ...)
    x$setinverse(cachedMatrix)
    cachedMatrix
}
