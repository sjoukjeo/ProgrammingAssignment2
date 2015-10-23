## These functions are able to cache potentially time-consuming computations.
## In particular, they are functions that create a special object that stores
## a matrix and cache's its inverse. 

## The function makeCacheMatrix creates a special "matrix", which is really a
## list containing a function to 1.set the value of the matrix; 2.get the value 
## of the matrix; 3.set the value of its inverse; get the value of its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## Create a special matrix object
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

## The function cacheSolve calculates the mean of the special "matrix" created with the
## above function. However, it first checks to see if its inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates
#  the inverse of the matrix and sets the value of the inverse in the cache via the setsolve function.

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
