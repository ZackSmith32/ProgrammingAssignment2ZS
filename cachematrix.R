## makeCacheMatrix creates a list of functions.  Those functions are utilized in
## cacheSolve to output inverse from cache, and if not in cache then it computes the
## inverse.  The intent is to not have to compute the inverse of the same matrix twice

## makeCacheMatrix creates a list of functions.  Those functions are set, get, setinverse
## and get inverse.  This list will be utilized by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function outputs the inverse of the matrix input into makeCacheMatrix
## The function checks if the value is stored in cache, and prints it if it is in cache
## If the inverse is not in cache, this function computes the inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## this section computes inverse
    data <- x$get()
    
    ## doing the calculation that will be stored in cache
    m <- solve(data)
    
    ## cache's m
    x$setinvers(m)
    m
}
