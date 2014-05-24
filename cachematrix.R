## These functions allow for the creation of a special "matrix"
## which caches its inverse after computing it, allowing us to
## save compute cycles by using the cached value in situations
## where the inverse would normally be computed multiple times

## This function creates a special "matrix", which is really
## just a list of functions, allowing us to:
##   set the value of the matrix
##   get the value of the matrix
##   set the value of the inverse
##   get the value of the inverse
makeCacheMatrix <- function(mtx = matrix()) {
    inverse <- NULL

    # sets the value of the matrix (mtx),
    # sets the inverse to NULL so it will be re-computed
    set <- function(new_mtx) {
        mtx <<- new_mtx
        inverse <<- NULL
    }

    # returns the current value of the matrix
    get <- function() {
        mtx
    }

    # sets the value of the inverse
    setInverse <- function(new_inverse) {
        inverse <<- new_inverse
    }

    # returns the current value of the inverse
    getInverse <- function() {
        inverse
    }

    # return the container for these functions,
    # which is really just a list
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function creates a special "matrix", which is really
## just a list of functions, allowing us to:
cacheSolve <- function(cache_mtx, ...) {
    # get the current value of inverse
    inverse <- cache_mtx$getInverse()

    # if inverse is NULL, compute and set its value
    if(is.null(inverse)) {
        mtx <- cache_mtx$get()
        inverse <- mean(mtx, ...)
        cache_mtx$setInverse(inverse)
    }
    # else, inverse has a value; let the caller know
    # that we're going to return that cached value
    else {
        message("getting cached data")
    }
    
    # either way, return the value of inverse
    inverse
}
