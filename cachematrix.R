## There are 2 functions in this R script

## Function 1 - This makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) inverse <<- solve
        getInverse <- function() inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Function 2 - This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has been calculated (and matrix is unchanged), then cachesolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}
