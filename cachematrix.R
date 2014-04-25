## Caching the Inverse of a Matrix

## makeCacheMatrix creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	cachedMatrix <- NULL
    set <- function(y) {
        x <<- y
        cachedMatrix <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) cachedMatrix <<- solve
    getinverse <- function() cachedMatrix
    list(set = set, get = get,				#a list of 4 functions
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been cached, it just 
## returns the cache. Otherwise, it will compute the inverse using
## solve()

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    cachedMatrix <- x$getinverse()			#query the x vector's cache
    if(!is.null(cachedMatrix)) {
        message("getting cached data")		#check if a cache exists
        return(cachedMatrix)				#if yes, return the cache
    }
    data <- x$get()							
    cachedMatrix <- solve(data, ...)		#if not, compute the inverse
    x$setinverse(cachedMatrix)				#save the result
    cachedMatrix							#return the result
}
