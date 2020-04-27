## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#this function creates an object "matrix" that can cache its inverse (solve(matrix))

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function(solve) m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## Return a matrix that is the inverse of 'x'

cachesolve <- function(x, ...) {
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
