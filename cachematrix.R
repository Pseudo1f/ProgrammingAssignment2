## The two functions below are intended to save time when calculating the
## inverse of a matrix. The first function creates a list with a series of
## functions and the second function calls upon these functions depending
## on whether the inverse has been calculated or not

## This first function takes as input an invertible matrix. The output is a 
## list of four functions. The first sets the value of the matrix, the second
## retrieve the value of the matrix, the third sets the inverse of the matrix
## and the fourth calculates the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setsolve <- function(solve) m <<- solve
     getsolve <- function() m
     list(set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
}


## the input of this function is the list (the output from the prior function)
## It first checks to see if the inverse has already been calculated
## that is what lines 33 and 34 do. If the inverse has already been calculates
## then it returns the inverse (which has already been calculated).
## if not then it calculates the inverse and sets it (by calling some of the 
## functions defined in makeCacheMatrix())

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m <- x$getsolve()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setsolve(m)
     m
}
