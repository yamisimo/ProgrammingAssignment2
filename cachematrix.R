## Put comments here that give an overall description of the functions 

## The following functions - 'makeCacheMatrix' and 'cacheSolve' - cache the inverse of a matrix. They use:
## 'get' function which returns the matrix x stored in the main function;
## 'set' function which changes the matrix stored in the main function;
## 'setinverse' and 'getinverse' functions which store the value of the input in a variable 'i'
## into the main function makeCacheMatrix (setinverse) and return it (getinverse).

## Write a short comment describing this function
## The function 'makeCacheMatrix'creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL 
	set <- function (y) {
		x <<- y
		i <<- NULL
	}
	get <- function () x
	setinverse <- function (inverse) i <<- inverse
	getinverse <- function () i 
	list (set=set, get=get,
	      setinverse =  setinverse,
	      getinverse = getinverse)
}

## Write a short comment describing this function
## The function 'cacheSolve' returns a matrix that is the inverse of 'x. It computes the inverse of the 
## special "matrix" created with makeCacheMatrix, but it first checks if the inverse has already been calculated: 
## If so (and the matrix has not changed), then 'cacheSolve' retrieves the inverse from the cache. 
## Otherwise, it calculated the inverse and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
	i <-x$getinverse()
	if(!is.null(i)){
		message ("getting cached data")
		return(i)
	}
	data <-x$get()
	i<-solve(data,...)
	x$setinverse(i)
	i
}
