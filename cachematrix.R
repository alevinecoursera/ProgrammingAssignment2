## The following functions cache the inverse of a matrix in memory and retrieve the inverse from memory, rather than recalculating the inverse each time it needs to be computed.

## Creates a list of functions that allow us to cache the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL #inverse initialized to NULL
		#NOTE: we could put i <<-solve(y) and the inverse will be cached
		#when makeCacheMatrix is called. I will follow the makeVector
		#example and wait until cacheSolve is called to cache the inverse.
	}
	get <- function() x #returns matrix
	setinverse <- function(inverse) i <<- inverse #sets inverse
	getinverse <- function() i #returns inverse
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse) #creates a list of the functions definted above
					 #list is then returned
		

}


## Retrieves cached inverse. Calculates inverse if none cached.

cacheSolve <- function(x, ...) {
        i <- x$getinverse() #get cached inverse
	if(!is.null(i)) { #if inverse was computed
		message("getting cached inverse")
		return(i)
	}
	#if no inverse cached, compute inverse and cache it via the object x
	data <-x$get() #get matrix
	i <- solve(data, ...) #compute inverse
	x$setinverse(i) #cache inverse
	i#return inverse
}
