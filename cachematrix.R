## Set of function for cached inversion of matrix.
## Matrix inversion is done with usage of solve() function

## Creates matrix structure with access code and caches matrix data
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Inversing matrix with usage of cache mechanism.
## Inversion is done with solve() function
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
