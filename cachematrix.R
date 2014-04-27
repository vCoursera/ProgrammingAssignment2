## makeCacheMatrix uses a matrix to initialize the input matrix through the first call of makeCacheMatrix 
##  cacheSolve returns the inverse of the matrix from cache if already calculated or else fresh

## This method creates the input for the inverse and exposes 4 functions get , set , getInverse and setInverse based on whether on not these have been set in the cache

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) m <<- inv
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This method looks for a cached version of matirx inverse for the object passed as input param , if found returns the same or calculates the same and put a copy in cache as well.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
	if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <<- data %*% solve(data)
        x$setInverse(inv)
}
