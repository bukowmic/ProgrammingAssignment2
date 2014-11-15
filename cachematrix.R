## Put comments here that give an overall description of what your
## functions do

## 'makeCacheMatrix':
## this function creates a matrix in the moment of calling or by using the method 'set'
## you can print the current set matrix by calling a method 'get'
## a variable 's' stores the inverse of 'x' 
## the methods setsolve and getsolve sets and prints the inverse of matrix 'x' respectively

makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) s <<- solve
	getsolve <- function() s
	list(set = set, get = get,
		setsolve = setsolve,
		getsolve = getsolve)
}

## 'cacheSolve':
## this function returns the inverse of given matrix 'x' made by function 'makeCacheMatrix'
## if inverse is already calculated and stores in variable 's' of 'makeCacheMatrix' function
## then 'cacheSolve' returns cached result
## otherwise it calculates the inverse of 'x' and stores it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	s <- x$getsolve()
	if(!is.null(s)) {
		message("getting cached data")
		return(s)
	}
	data <- x$get()
	s <- solve(data, ...)
	x$setsolve(s)
	s
}
