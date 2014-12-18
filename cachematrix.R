## TODO: Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL 			# cached matrix 
	set <- function(y) {            
		x <<- y
		m <<- NULL
	}
	get <- function() x             
	setinverse <- function(inverse) m <<- inverse  # anonymous function that sets m to inverse in other environment
	getinverse <- function() m    	      # returns inverse from other environment
	list(set = set, get = get,            # returns list containing 4 functions? an object?
		setinverse = setinverse,
		getinverse = getinverse)
}


## TODO: Write a short comment describing this function
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'

	m <- x$getinverse()              # gets inverse from this matrix
	if(!is.null(m)) {             # checks is inverse was already calculated 
		message("getting cached data")   # if it was print message
		return(m)		         # and return cached inverse	
	}
	data <- x$get()			     # data is given matrix (x)
	m <- solve(data)                 # compute inverse from the matrix (x)
	x$setinverse(m)                         # updates cached inverse
	m				     # returns inverse value
}
