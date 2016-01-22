## The functions 'makeCacheMatrix' and 'cacheSolve' allow for returning
## the inverse of a given (invertible) matrix: If the matrix has not been
## inverted already, it is calculated and then cached. Otherwise, it is 
## taken from the cache only, which saves computing time.

## The function 'makeCacheMatrix' creates a special 'matrix' object. 
## It is a list containing four functions:
## 'set' sets the value of the matrix
## 'get' gets the value of the matrix back
## 'setinverse' sets the value of the inverse of the matrix 
## 'getinverse' gets the value of the inverse of the matrix from cache

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

## The function 'cacheSolve' first checks, if the inverse has already been
## calculated (i.e., if the inverse has already been stored in the cache by
## setinverse()). If not, it calculates the inverse of the matrix and sets
## it to the cache (setinverse).
## Otherwise, 'cacheSolve' takes the inverse from the 
## cache (getinverse()) and just returns it without recalculating it.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

## Note: The two functions can be used as follows:
## source("ProgrammingAssignment2_HR.R")
## SpecialMatrix <- makeCacheMatrix(M)
## cacheSolve(SpecialMatrix) (first time)
## --> the matrix M (which must have been defined before) is calculated
## and returned.

## cacheSolve(SpecialMatrix) (second time)
## the message "getting cached data" and the inverse (now from cache) are
## returned.