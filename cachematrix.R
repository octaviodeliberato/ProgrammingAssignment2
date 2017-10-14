## makeCacheMatrix creates a special "matrix" whereas cacheSolve
## calculates, if needed, the inverse of this special "matrix".

## If the inverse has already been calculated and the matrix hasn't
## changed, cacheSolve will skip the computation.

## makeCacheMatrix Creates a special "matrix" (actually a list)
## whose functions ("methods") do the following tasks:
##    - set the matrix
##    - get the values of the matrix
##    - set the inverse of the matrix
##    - get the values of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y = matrix()) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(xinv) m <<- xinv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve will calculate, if needed, the inverse of the special "matrix"
## created by makeCacheMatrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x', if needed.
    ## x is a special "matrix" previously created by makeCacheMatrix.
    
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    m <- x$get()
    inv <- solve(m, ...)
    x$setinv(inv)
    inv
}