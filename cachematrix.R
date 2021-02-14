## There are two functions that are used to create a special
## object that stores a matrix and cache its inverse.

## This function will create a "special matrix", that will actually
## be a list with a function to set & get the value of the matrix, 
## and set & get the value of this matrix's inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) inv <<- solve
    getsolve <- function() inv
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function computes the inverse of the "special matrix" returned 
## by the makeCacheMatrix function, or retrives the inverse that has
## already been calculated if the matrix has not changed.

cacheSolve <- function(x, ...) {
    inv <- x$getsolve()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setsolve(inv)
    inv
}
