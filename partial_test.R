## The makeVector function below works when entering at command line:
## >makeVector(68)
## returns the value 68 for x
## returns missing value for y

## >makeVector(c(1:5))
## returns the value 1 2 3 4 5 for x
##
##
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                                                     ## 
        set <- function(y) {                                          ## set up "special matrix list"
                x <<- y                                               ## cache y as x 
                m <<- NULL                                            ## cache NULL as m
        }
        get <- function() x                                           ## retrieve matrix from cache
        setinverse <- function(solve) m <<- solve                     ## call R "solve" function
        getinverse <- function() m
        list(set = set, get = get,                                    ## create "special matrix list"
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}