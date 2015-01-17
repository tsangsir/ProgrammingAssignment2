## Put comments here that give an overall description of what your
## functions do
# Coursera R Programming Peer Accessment
# "Programming Assignment 2: Lexical Scoping **Please Note: No Grace Period**"
# 
# Quoted from assignment
# Matrix inversion is usually a costly computation and their may be 
# some benefit to caching the inverse of a matrix rather than compute 
# it repeatedly (there are also alternatives to matrix inversion that 
# we will not discuss here). This assignment is to write a pair of 
# functions that cache the inverse of a matrix.


## Write a short comment describing this function
# 
# This function creates a special "matrix" object that can cache its 
# inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve 
# the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if (!is.null(inv)) {
		message("getting cached data")
		return (inv)
	}
	m <- solve(x$get())
	x$setinv(m)
	m
}


