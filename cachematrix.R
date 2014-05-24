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

    # overwrites the matrix (mtx) with the value in new_mtx,
    # sets inverse to NULL so that it will be re-computed
    set <- function(new_mtx) {
        mtx <<- new_mtx
        inverse <<- NULL
    }

    # returns the current value of the matrix
    get <- function() {
        mtx
    }

    # sets the cached inverse
    setInverse <- function(new_inverse) {
        inverse <<- new_inverse
    }

    # if the inverse is currently NULL,
    # compute and store it for future use;
    # either way, return its value at the end
    getInverse <- function() {
        if(inverse == NULL)
        {
            inverse <- solve(mtx)
        }
        inverse
    }
    
    # return the container for these functions,
    # which is really just a list
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
