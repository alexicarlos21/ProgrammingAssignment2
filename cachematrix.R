## Below is a pair of functions that are used to create a special object that 	
## stores a matrix and cache its inverse

## makeCacheMatrix creates a special matrix that caches its inverse

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


## cachesolve computes or retrieves 
## the inverse of the special matrix in makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- inverse(data, ...)
        x$setinverse(i)
        i

}
