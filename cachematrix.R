## To reduce time-consuming computation these functions
## checks if the computation has been already calculated
## to return the cached result

## Creates a special "matrix" containing a list of four funcions
## set and get the matrix values
## set and get the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {

	i <- NULL

	set <- function(y) {
		x <<- y
		m <<- NULL
	}

	get <- function() x

	setInverse <- function(inv) i <<- inv
	getInverse <- function() i

	#Listing all the functions
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## cacheSolve take a matrix created from makeCacheMatrix
## returns the inverse matrix, checking first if the
## inverse has been already calculated

cacheSolve <- function(x, ...) {
	
	i <- x$getInverse()
	
	#Seraching for inverse
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}

	#Getting matrix data
	data <- x$get()
	
	i <- solve(data, ...)
	
	#Setting new inverse
	x$setInverse(i)
	
	i
}
