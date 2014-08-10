## makeCacheMatrix is a function that creates a special "matrix" object that
## can cache its inverse. 
## cacheSolve is a function that computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If the matrix has not been changed, the function
## will only calculte the inverse if it is not calculated already and 
## stored in cache. 

## The function allows the user to create a special matrix; reset and retrieve
## its value; storing and retrieving the value of its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <-NULL
        set <- function(y) {
                x<<-y
                inv<<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv=setinv, getinv = getinv)
}


## This function returns the inverse of a matrix that is created by the 
## makeCacheMatrix function. It also avoids recalculation of the matrix's
## inverse if it was calculated and stored previously in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
