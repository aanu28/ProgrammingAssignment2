## Put comments here that give an overall description of what your
## functions do

## These functions create a special matrix object that can cache 
## its inverse. This avoids recomputing the inverse repeatedly 
## and improves performance.

## Write a short comment describing this function

## This function, makeCacheMatrix() creates a special "matrix" object that can
## store its inverse for later retrieval.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inverse) inv <<- inverse
    
    getInverse <- function() inv
    
    list(
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
    
}


## Write a short comment describing this function

## This function computes the inverse of the special matrix
## returned by makeCacheMatrix(). If the inverse is already 
## cached and the matrix has not changed, it retrieves the 
## inverse from the cache instead of recomputing it.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    
    mat <- x$get()
    inv <- solve(mat, ...)
    
    x$setInverse(inv)
    inv
}

m <- matrix(c(1, 2, 3, 4), nrow = 2)
cm <- makeCacheMatrix(m)
cacheSolve(cm)
cacheSolve(cm)
cm$set(matrix(c(2, 1, 1, 2), nrow = 2))
cacheSolve(cm)