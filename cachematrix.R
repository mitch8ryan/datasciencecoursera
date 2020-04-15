## Mitch Ryan Cache Matrix Assignment
## Together, these 2 functions cache the inverse of the matrix. This can be a lot faster
## than having to compute it over and over. 

## makeCacheMatrix creates a special 'matrix' which is just list containing functions
## to get and set both x (the matrix) and the inverse of x. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
    x <<- y
    m <<- NULL
    }
    get <- function() x
    set_inv <- function(solve) m <<- mean
    get_inv <- function() m
    list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
  
}


## cacheSolve calculats the inverse using the above function, but also checks
## to see if the inverse has already been calculated. If it has been calculated, the function skips
## that step and takes the mean from the cache. Otherwise it does the calculation. 

cacheSolve <- function(x, ...) {
    m <- x$get_inv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$set_inv(m)
    m
}
