## completed by Jamal Alikhani May 2017
## This function computes the inverse of a given matrix for the first introduction
## otherwise it cashes the previously calcuted inverted matrix 

## makeCacheMatrix: Creates a list of functions over input x

makeCacheMatrix <- function(x = matrix()) {
        xinv <- NULL
        set <- function(y) {
                x <<- y
                xinv <<- NULL
        }
        get <- function() x
        setinv <- function(x_inv) xinv <<- x_inv
        getinv <- function() xinv
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## cacheSolve: calculates the inv for the first tiem and 
## returns the inv that has already been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xinv <- x$getinv()
        if(!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
        }
        mat <- x$get()
        xinv <- solve(mat, ...)
        x$setinv(xinv)
        xinv
}
