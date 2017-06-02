## Program:     cachematrix_testing.R
## Date:        June 01, 2017
## Programmer:  Ryan Cleary
## Created for: Coursera R course Week 3 Assignment
##
## Purpose:     Create two functions, "makeCacheMatrix" and "cacheSolve" to 
##              cache the inverse of a matrix.
## 
## User input:  >cacheSolve(makeCacheMatrix(x))
## Example:     >cacheSolve(makeCacheMatrix(matrix(rnorm(16), nrow = 4, ncol =4)))
##
##
##

## "makeCacheMatrix" is a function to create a special matrix list
## to be used to create the inverse of the user defined matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {                                             
                x <<- y
                m <<- NULL
        }
        get <- function() x 
        setinverse <- function(solve) m <<- solve 
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## "cacheSolve" is a function to compute the inverse of the
## user defined matrix, based on the list created in "makeCacheMatrix" 
## If the inverse has been previously calculated, "cacheSolve" function.  
## will retrieve the inverse from the cache.  
## Else "cacheSolve" will compute the inverse.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <-- solve(data, ...)
        x$setinverse(m)
        m
}
##
## End cachematrix.R