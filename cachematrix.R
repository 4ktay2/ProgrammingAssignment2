## makeCacheMatrix() creates a matrix object that can calculate and cache it's inverse
## cacheSolve() will calculate the inverse if not found in cache for the inverse of the
## matrix object returned by makeCacheMatrix()

## makeCacheMatrix creates matrix object, calculates/caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        minv = NULL
        set = function(y) {
                x <<- y
                minv <<- NULL
        }
        get = function() x
        setinv = function(inverse) minv <<- inverse
        getinv = function() minv
        list(set=set, get=get, setinv = setinv, getinv=getinv)
}


## Returns the inverse of the matrix passed to it if not found in cache.

cacheSolve <- function(x, ...) {
        
        inv = x$getinv()
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mdata = x$get()
        inv = solve(mdata, ...)
        
        x$setinv(inv)
        return(inv)
}
