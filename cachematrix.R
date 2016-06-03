## Matrix inversion is often a costly computation and it usually is beneficial 
## to cache the inverse of a matrix rather than computing it repeatedly. This
## pair of functions work together to cache the inverse of a matrix.
## The following sequence of statements may be used to illustrate the operation
## of the two functions:
## 
## m <- makeCacheMatrix()
## a <- matrix(c(1,2,3,4), nrow = 2, ncol = 2)
## m$set(a)
## cacheSolve(m)
## b <- cacheSolve(m)
## m$set(b)
## m$get()
## cacheSolve(m)
## c <- cacheSolve(m)
## a == c

## This function provides an encapsulated structure (similar to an object in 
## object-oriented programming). The structure contains the following:
##    DATA:
##        argument x:         a square matrix
##        local variable inv: the inverse of x (once calculated)
##    BEHAVIOR:
##        set: set the value of x
##        get: get the value of x
##        setinverse: set the inverse of x
##        getinverse: get the inverse of x
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve will retrieve the inverse
## from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv    
}
