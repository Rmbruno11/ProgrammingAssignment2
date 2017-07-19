## The following function calculates the mean of the special "vector" created with the above function. 
## However, it first checks to see if the mean has already been calculated. If so, it gets the mean from 
## the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the value 
##of the mean in the cache via the setmean function.

## **************************************************
## HELP TO TEST THOSE FUNCTIONS IN BOTTOM OF THIS FILE
## **************************************************

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(minv) inv <<- minv
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

## ****************
## HELP FOR TESTING
## ****************

## Matrix Examples

## Example 1                    inverse 

##    1    1    0               1    0   -1
##    1    0    1               0    0    1
##    0    1    0               -1   1    1

## m1 <- matrix(c(1,1,0,1,0,1,0,1,0), nrow = 3, ncol = 3)


## Example 2                    inverse 
            
##    1    0    2               1    0   -2
##   -1    1    0               1    1   -2
##    0    0    1               0    0    1

## m2 <- matrix(c(1,-1,0,0,1,0,2,0,1), nrow = 3, ncol = 3)

## TEST

## > source("cachematrix.R")
## > mobj <- makeCacheMatrix()   ## Creates special matrix object
## > m1 <- matrix(c(1,1,0,1,0,1,0,1,0), nrow = 3, ncol = 3)   ## Creates example matrix
## > mobj$set(m1)      ## Sets the example matrix to special object
## > cacheSolve(mobj)   ## Creates inverse matrix and saves in cache
## [,1] [,2] [,3]
## [1,]    1    0   -1
## [2,]    0    0    1
## [3,]   -1    1    1
## > mobj$getinv()      ## Shows the cached inverse matrix
## [,1] [,2] [,3]
## [1,]    1    0   -1
## [2,]    0    0    1
## [3,]   -1    1    1
 
