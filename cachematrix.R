## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    	## Intialize matrix inverse
	ic <- NULL
	
	## Initialize matrix input from cache and matrix inverse
	setmat <- function(xc) {
		x <<- xc
            ic <<- NULL
      }
      
	## Retrieve cached matrix input
	getmat <- function() x
      
	## Put matrix inverse in cache
	setinv <- function(inv) ic <<- inv
      
	## Retrieve matrix inverse from cache
	getinv <- function() ic
      
	list(setmat = setmat, getmat = getmat,
             setinv = setinv,
             getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache.
cacheSolve <- function(x, ...) {
	## Check if matrix inverse has been cached
        inv <- x$getinv()
        if(!is.null(inv)) {
        	message("getting cached data")
            return(inv)
        }
        data <- x$getmat()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
