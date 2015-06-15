# makeCacheMatrix and cacheSolve R functions for the
# R programming course assignment

# I wrote the code following the supplied example
# for caching the mean of a vector

# From the assignment instructions:

# For this assignment, assume that the matrix supplied is always invertible.


# makeCacheMatrix: 
# This function creates a special "matrix" object that 
# can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL

	set <- function(y) {
	x <<- y
	inv <<- NULL
	}

	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


# cacheSolve: 
# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve 
# the inverse from the cache.

cacheSolve <- function(x, ...) {

	inv <- x$getinverse()

	if(!is.null(inv)) {
	message("getting cached data.")
	return(inv)
	}

	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
	inv

}