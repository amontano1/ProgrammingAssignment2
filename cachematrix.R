## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
##************************************************
## makeCacheMatrix is a function that stores 4 functions
## set substitutes the value of the matrix x with the value y in the makeCacheMatrix function and it sets the value of mtx_inv to null	
## get simply returns the matrix stored in function makeCacheMatrix
## setinv stores the value of the function in mtx_inv
## getinv returns the value given to mtx_inv

makeCacheMatrix <- function(x = matrix()) {
	mtx_inv <- NULL
	set <- function(y) {
		x <<- y
		mtx_inv <<- NULL
	}
	get <- function() x
	setinv <- function(inv) mtx_inv <<- inv
	getinv <- function() mtx_inv
	list(set=set, get=get, setinv=setinv, getinv=getinv) 
}

## Write a short comment describing this function
##***********************************************
## the cachesolve function passes the value of function getinv to mtx_inv and checks that it's not null
## if the value is not null, they it returns the stores value of mtx_inv
## if the value is null, data is assigned the matrix from makeCacheMatrix and mtx_inv calculates the inverse of that matrix
## and setinv stores the value and makes it available to makeCacheMatrix

cacheSolve <- function(x, ...) {
	mtx_inv <<- x$getinv()
	if(!is.null(mtx_inv)) {
		message("getting cached data")
		return(mtx_inv)
	}
	data <- x$get()
	mtx_inv <- solve(data, ...)
	x$setinv(mtx_inv)
	mtx_inv
}
