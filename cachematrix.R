## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a cache for the inverse result

makeCacheMatrix <- function(x = matrix()) {
    ## the result of matrix inverse
    m <- NULL
    
    ## function to set the data (the matrix)
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## function to get the data
    get <- function() x
    ## function to set the cached inverse result
    setinverse <- function(inverse) m <<- inverse
    ## function to retrieve the cached inverse result
    getinverse <- function() m
    
    ## return a new result with all the required functions
    ## to manage get/set data and cache
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Write a short comment describing this function
## cacheSolve returns the inverse of the matrix
## it requires an object created by makeCacheMatrix
## and will retrieve the cached result if available,
## otherwise it will calculate the inverse and cache the result

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## check if the inverse has already been calculated
    m <- x$getinverse()
    if(!is.null(m)) {
        ## inverse cached, so return the result
        message("getting cached data")
        return(m)
    }
    
    ## inverse not calculated, so get the matrix, calculate and cache
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    
    ## return the inverse
    m
}
