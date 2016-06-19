## 
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


# accepts a mtrix and saves the matrix in a list along with functions to cache/save the inverse
# of a matrix, ou can also change the matrix that is saved
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverseMatrix) inverse <<- inverseMatrix
        getInverse <- function() inverse
        list(set = set,
                get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}


## Write a short comment describing this function

# accepts a makeCacheMatrix type and computes the inverse if it already computed
# if a inverse is already computed for the matrix in question
# it returns the inverse that was cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("Getting inverse from cached data!")
                return(inverse)
        }
        theMatrix <- x$get()
        inverse <- solve(theMatrix, ...)
        x$setInverse(inverse)
        inverse
}
