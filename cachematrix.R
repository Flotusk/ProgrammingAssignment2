## makeCacheMatrix creates a special "matrix", which contains a function that 
## does the following:

## sets the value of the matrix
## gets the value of the matrix
## sets the value of the inverse of the matrix
## gets the value of the inverse of the matrix

## "i" is the inverse matrix value

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}

## cacheSolve looks for the inverse value of a matrix from makeCacheMatrix first.
## if the inverse value is not available, cacheSolve then calculates
## the inverse value of a matrix.  "i" is the inverse value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getinverse()
		if(!is.null(i)) {
				message("getting cached data")
				return(i)
		}
		matrix <- x$get()
		i <- solve(matrix)
		x$setinverse(i)
		i
}
