## 'makeCacheMatrix' is a list of 4 functions: set(), get(), setm(), getm()
## These 4 functions comprise Assignment 2 of the R Programming course on Coursera
## Both 'makeCacheMatrix' and 'cacheSolve' functions were written to demonstrate
## lexical scoping within the R environment.

## 'makeCacheMatrix' is a list of 4 functions that creates a matrix object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL	
	}
	get <- function() x
	setm <- function(solve) m <<- solve
	getm <- function() m
	list(set = set, get = get, setm = setm, getm = getm)
}

## 'cacheSolve' is a function that computes the inverse of the matrix object
## computed by 'makeCacheMatrix' (the function coded above).  If the inverse
## has already been calculated, then 'cacheSolve' should retrieve the inverse
## from the cache.  I

cacheSolve <- function(x = matrix(),  ...) {
	m <- x$getmatrix()
	if(!is.null(m)) {
		message("getting cached data"
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setmatrix(m)
	m
}
