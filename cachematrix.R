## makeCacheMatrix contains the functions to set and get the original matrix and
## its inverse.
## cachesolve verifies the cache for the inverse matrix, if it is already cached
## the function returns the cached matrix. if the cache is clear the function
## calculates the inverse, caches it and returns it to console.


## makeCacheMatrix receives a matrix as argument. Calling this function will
## cache the matrix in variable x and clear inv, which is the cache variable
## for the inverse of x.
## At the end of the function a list containing the four functions to get and
## set the matrix and its inverse is returned.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(aux) inv <<- aux
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cachesolve receives the "list of 4" (returned by makeCacheMatrix) as argument
## and returns the inverse of the matrix that was passed initially as argument
## to makeCacheMatrix!
## This is the function that calculates the inverse matrix. First it gets the
## inverse with "x$getinverse" and verifies if it is already cached.
## If "!is.null(inv)" is TRUE the cached inverse matrix is returned and the
## function ends there. if the conditional expression is FALSE the rest of the
## function is executed. The inverse is calculated with "solve(x$get(),...)"
## and cached with "x$setinverse(inv)" and the inverse matrix "inv" is returned.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached matrix")
        return(inv)
    }
    inv <- solve(x$get(), ...)
    x$setinverse(inv)
    inv
}
