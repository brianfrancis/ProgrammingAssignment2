## The following functions allow the inverse of a given matrix
## to be cached.  On a first call to get the inverse of the 
## matrix, the inverse is calculated and stored in variable i.
## Any subsequent call to get the inverse will return the value of i
## without calling the solve function to recalculate the inverse.


##------------------------------------------------------------------------------

## makeCacheMatrix creates list of 4 functions to:
##     1. set the value of the matrix
##     2. get the value of the matrix
##     3. set the value of the inverse
##     4. get the value of the inverse
## note that i & x will be part of the same environment as the list of functions
## returned by makeCacheMatrix.  Due to the lexical scoping of R, that means
## subsequent calls of those functions for an instance of makeCacheMatrix
## will access the values of i & x that were previously stored in the 
## environment.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve calculates and returns the inverse of a matrix if that matrix 
## has not been previously inverted.  If the matrix was already inverted
## then the function gets the value previously cached.  
## the parameter passed (x) would be the return of makeCacheMatrix
## e.g., m <- matrix(rnorm(100),10,10)  ##10 x 10 matrix
##       x <- makeCacheMatrix(m)        ##return the special "matrix"
##       inv <- cacheSolve(x)  ## calculates and cache's inverse of matrix m
##       inv1 <- cacheSolve(x) ## returns inverse of matrix m from the cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        ## check if i is null and return i if not and exit the function
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## invert the matrix m if not previously calcualted
        data <- x$get()
        i <- solve(data, ...)
        ## cache the inverse of matrix m and return it
        x$setinverse(i)
        i
}
