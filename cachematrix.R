## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix is a function which creates a list of functions set, get, setInverse and getInverse.
## Set stores the matrix in an object defined outside of the function
## Get retrieves the object
## setInverse calculates the inverse of an invertible matrix and caches it in inverse object
## getInverse retrieves the cached value
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL

    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }

    get <- function() x

    setInverse <- function(solve) {
        inverse <<- solve
    }

    getInverse <- function() {
        inverse
    }

    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

} 


## Write a short comment describing this function
##cacheSolve looks for an object inverse not having a NULL value, if found it displays it along with a message
## If not found,  it will calculate the inverse of the matrix defined earlier
## To use these two functions, first use makeCacheMatrix to store a matrix in an object
## Then using this new object as an argument for cacheSolve, retrieve either the cached value of inverse or calculate it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()

    if (!is.null(inverse)) {
        message("Getting cached data")
        return(inverse)
    }

    matrix <- x$get()
    inverse <- solve(matrix, ...)

    x$setInverse(inverse)
    inverse
}
