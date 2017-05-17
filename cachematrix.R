## Functions for creating a matrix with cacheable "solve" function

## Makes an object for caching of result of solve 
## function which contains a list of functions.
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(m) {
    x <<- m
    s <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) s <<- solve
  getSolve <- function() s
  list(set = set, 
       get = get, 
       setSolve = setSolve,
       getSolve = getSolve)
}


## Returns result of "solve" function from a cache 
## if it had been calculated or calculates result if it hadn't been calculated.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  	slv <- x$getSolve()
  	if(!is.null(slv)) {
   		message("getting cached data")
   		return(slv)
  	}
  	data <- x$get()
  	slv <- solve(data, ...)
  	x$setSolve(slv)
  	slv
	}
}
