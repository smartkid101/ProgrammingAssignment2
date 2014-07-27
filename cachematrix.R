# This program creates the inverse of a given matrix. If the matrix
# has already been solved, it returns a cached solution.


# This function makes the matrix, stores it, and gets the inverse
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# This function returns the cached solution if there is one,
cacheSolve <- function(x, ...) {
	m <- x$getinv()
	if(!is.null(m)) {
		message("getting cached data")
		return (m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinv(m)
	m
}