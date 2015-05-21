# The cachematrix script creates two functions (one with four nested
# functions) that together present the inverse of a matrix in the most 
# efficient manner, i.e. cached if available.


makeCacheMatrix <- function(x = matrix()) {
# A series of functions to assign and store a matrix and its inverse.
#
# Args:
#	x: a matrix whose inverse is to be calculated
#
# Returns: a list with four functions
#	$set: sets x as the matrix to be inversed and sets i to NULL
#	$get: returns x
#	$setinverse: sets i as the inverse matrix of x
#	$getinverse: returns i
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
# Efficiently finds the inverse matrix of x from makeCacheMatrix()
#
# Args:
#	x: a variable that represents an indexed makeCacheMatrix function
#
# Returns: the inverse matrix of x from cache is available and correct.
# 	Otherwise calculates the inverse of x as i
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}