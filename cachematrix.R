## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## inner function that set the value of the matrix
    set <- function(y) {
        ## operator <<- assign a value to an object in an environment
        ## that is different from the current environment
        x <<- y
        m <<- NULL
    }
    ## inner function that get the value of the matrix
    get <- function() x
    
    ## inner function that set the inverse of the matrix
    setinverse <- function(inverse) m <<- inverse
    ## inner function that get the inverse of the matrix
    getinverse <- function() m
    ## R list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ## for x call function that get the inverse of the matrix
    m <- x$getinverse()
    ## check if data is cached
    if (!is.null(m)) {
        message("getting cached data")
        ## return cached data
        return(m)
    }
    ## if no data in cache then
    data <- x$get()
    ## invers data using solve function
    m <- solve(data, ...)
    ## call function that set the inverse of the matrix
    x$setinverse(m)
    ## return data
    m
}
