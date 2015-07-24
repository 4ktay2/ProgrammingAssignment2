## MakeCacheMatrix() and cacheSolve() take a matrix, create functions that can be used to calculate 
## the inverse and store it in cache where it is retrievable, and if not available, calculates it.

## makeCacheMatrix() returns list of 4 functions that can be used to set a matrix, get the matrix 
## and set and calculate the inverse. 
makeCacheMatrix <- function(x = matrix()) {
        
        mat <- NULL
        set <- function(y) {
                x <<- y
                mat <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) mat <<- inverse
        getinv <- function() mat
        list(set = set, get = get
             , setinv = setinv
             , getinv = getinv)
}


## cacheSolve() returns the inverse of the matrix either from cache, or calculates it.

cacheSolve <- function(x, ...) {
        
        mat <- x$getinv()
        
        if(!is.null(mat)) {
                message("getting cached data")
                return(mat)
        }
        data <- x$get()
        mat <- solve(data, ...)
        
        x$setinv(mat)
        return(mat)
}
