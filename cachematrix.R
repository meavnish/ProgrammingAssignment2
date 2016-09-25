## The two functions shown here help in caching the inverse of a matrix.  
## To avoid recomputing the inverse and generating the same result repeatedly,
## we can simply compute the result once.  
## If we try to recompute the inverse again, we should just return this 
## pre-computed result.

## Write a short comment describing this function
## To facilitate caching we are discussing, we have to first create a special
## matrix that will help us with this by using the
## makeCacheMatrix function.  The input into this function
## is simply a variable of type matrix.

makeCacheMatrix <- function(x = matrix()) {

	# Initially set to NULL
	inv <- NULL

    # set function
    # Sets the matrix itself
    set <- function(y) {
            x <<- y
            inv <<- NULL
    }

    # get function
    # Gets the matrix itself
    get <- function(){ 
	x
	}

    # Manually set the inverse
	setinverse <- function(inverse){
		inv <<- inverse
	}

    # Get the inverse
	getinverse <- function() {
		inv
	}

    # Encapsulate into a list
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)	

}


## Write a short comment describing this function
## Once we create this matrix, we use the cacheSolve
## function to compute the inverse and cache the result


cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	# Get the current state of the inverse and see if it
    	# has been computed yet

	inv <- x$getinverse()

	# If its computed, then

    	if(!is.null(inv)) {
    	# Simply return the computed inverse		

        message("Getting cached matrix")
        return(inv)
    }

	# If its not computed yet
    	# Get the matrix itself
    	data <- x$get()

    	# Find the inverse
    	inv <- solve(data, ...)

    	# Cache this result in the object
    	x$setinverse(inv)

    	# Return this new result
    	inv    
}
