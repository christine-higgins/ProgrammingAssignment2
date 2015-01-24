## Pair of functions makeCacheMatrix and cacheSolve to handle the following:
##  - compute the inverse of a matrix
##  - cache the inverse of a matrix
##  - access cached results when available

## makeCacheMatrix
##  creates a special "matrix" object that can cache its inverse.
##  Includes:
##    1.  set the value of the matrix
##    2.  get the value of the matrix
##    3.  set the value of the inverse
##    4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    ## Function to define the matrix data and clear previously cached value
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## Function returning matrix data
    get <- function() x
    
    ## Function to cache the found inverse
    set_inverse <- function(inverse_value) inv <<- inverse_value
    
    ## Function to return the cached inverse value if available
    get_inverse <- function() inv
    
    ## Return the special "matrix" definition
    list(set = set, get = get,
        set_inverse = set_inverse,
        get_inverse = get_inverse)
}


## This function computes the inverse of the special
##  "matrix" returned by `makeCacheMatrix` above. If the inverse has
##  already been calculated (and the matrix has not changed), then
##  `cacheSolve` retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## attempt to get the cached inverse
    inverse <- x$get_inverse()
    if(!is.null(inverse)) {
        ##return the cached inverse found
        return(inverse)
    }
    
    ## cached inverse not found; use solve to find matrix inverse
    data <- x$get()
    inverse <- solve(data)
    
    #cache the matrix inverse
    x$set_inverse(inverse)
    
    ## Return the matrix that is the inverse of 'x'
    inverse
}
