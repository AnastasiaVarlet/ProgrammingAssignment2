## Here we write a pair of functions that cache the inverse of a matrix.
## However, note that the matrix should be inversible.
## Below are the two functions!!!

## Function1 ---
## The makeCacheMatrix creates a special "matrix" object that can cache its inverse:

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)    
}

## Function2 ---
## The cacheSolve function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated, then the cacheSolve
## retrieves the inverse from the cache:

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinv(inv)
    inv
}
