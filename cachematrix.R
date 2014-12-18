# Function creates an object (list with a:setter for oryginal matrix that stores cached values of inverse matrix in 
# so called "other environment". It is used as a setter of matrix and it's inverse falue,
# and for a getter of matrix and it's inverse value.
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL 		  	 	       # cached matrix 
	set <- function(y) {            
		x <<- y
		m <<- NULL
	}
	get <- function() x             
	setinverse <- function(inverse) m <<- inverse  # anonymous function that sets m to inverse in other environment
	getinverse <- function() m    	   	       # returns inverse from other environment
	list(set = set, get = get,           	       # returns list containing 4 functions? an object?
		setinverse = setinverse,
		getinverse = getinverse)
}

# Computes inverse of matrix (x should be created by makeCacheMatrix),returns it and stores it in x.
# It can be retrieved by calling x$getinverse().
cacheSolve <- function(x, ...) {
	m <- x$getinverse()             	      # gets inverse from this matrix
	if(!is.null(m)) {       		      # checks is inverse was already calculated 
		message("getting cached data")        # if it was print message
 		return(m)		              # and return cached inverse	
	}
	data <- x$get()			              # matrix (x)
	m <- solve(data)                              # compute inverse of matrix (x)
	x$setinverse(m)                               # updates cache with inverse value
	m				              # returns inverse value
}
