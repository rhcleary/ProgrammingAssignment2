## Coursera R course Week 3 Assignment example function 1:
## "makeVector" function creates a special "vector", which 
## is really a list containing a function to:
##
## 1. set the value of the vector - line 23
##    and caches the function value - lines 24-25
## 2. get the value of the vector - line 27
## 3. set the value of the mean   - line 28
## 4. get the value of the mean   - line 29
##
## "makeVector" is a list with 4 elements (in order):
## set, get, setmean, getmean (as defined in the "list" line below)
## running makeVector will print the list (as list is last line in function)

## want to use the "makeVector" function inside the "cachemean" function in
## section 2 to calculate the mean of whatever function is input by user.
## syntax is:
## >cachemean(makeVector(c(1:50)))
## will return the mean
## >25.5

makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

## Coursera R course Week 3 Assignment example function 2:
## "cachemean" function calculates the mean of "special vector" 
## from "meanVector" function above, based on:
##
## 1. if mean already calculated, get mean from cache -line 35-37
## 2. else calculate mean and set mean to cache       -line 40-41
##

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

