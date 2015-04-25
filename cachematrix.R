## Matrix object structure to cache the inverse function

## Builder for the cached inverse matrix
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
}


## Function to get the inverse through a cache so it doesnt get calculated every time
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    ## If operation is cached return the result from the cache
    if (!is.null(m)) {
    	message("getting cached data")
    	return(m)
    }
    ## If not, get the inverse, add it to the cache and returns the result
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
