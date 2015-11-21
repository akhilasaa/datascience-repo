## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
## x is a square invertible matrix
## return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##         this list is used as the input to cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
        inverse = NULL
        set = function(y) {
                # use to assign a value to an object in an environment 
                # different from the current environment. 
                x <<- y
                inverse <<- NULL
        }
        get = function() x
        setinverse = function(inverse) inverse <<- inverse 
        getinverse = function() inverse
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
        ## x would be the output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        
        inverse = x$getinverse()
        
        # if the inverse has already been calculated
        if (!is.null(inverse)){
                # get it from the cache and skips the computation. 
                message("getting cached matrix")
                return(inverse)
        }
        
        # otherwise, calculates the inverse 
        data = x$get()
        inverse = solve(data, ...)
        
        # sets the value of the inverse in the cache via the setinverse function.
        x$setinverse(inverse)
        
        return(inverse)
}
