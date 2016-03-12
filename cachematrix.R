## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function creates a special matrix object that can 
## cache its inverse

## The cacheSolve function computes the inverse of the special matrix made by
## the above function. If the inverse has already been calculated and the
## matrix has not changed then the function will retrieve the inverse from the cache

makeCacheMatrix <- function(x = matrix()) {
	z <- NULL
	set <- function(y){
		x <<-y
		z <<- NULL
	}
	get <- function() x
	setz = function(inverse) z <<- inverse
	getinv = function() z
	list(set=set, get=get, setinv=setinv, getinv=getinv)


}


cacheSolve <- function(x, ...) {
        z = x$getinv()
	if(!is.null(z)){
		message("getting cached data")
		return(z)
	}
	
	mat.data = x$get()
	z = solve(mat.data, ...)
	
	x$setinv(z)
	return(z)
	
}
