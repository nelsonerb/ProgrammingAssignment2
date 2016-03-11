## These functions help to store a cached version of a matrix inverse 
## so that the inverse does not need to be calculated mutliple times
## for the matrix

## Returns a list with functions and variables to access a matrix
## and set/get a matrix and its calculated inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(set = set, 
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## Function that returns a cached inverse if available, otherwise
## it calculates the inverse

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
