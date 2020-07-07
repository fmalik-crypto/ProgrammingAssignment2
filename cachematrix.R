
## The function makeCacheMatrix() generates a "list" that contains following functions:

##     set() function takes the matrix to be inverted as argument
##     get() function returns the matrix to be inverted    
##     setsolve() function takes the matrix as argument to set it as inverse
##     getsolve() function returns the value of inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The function cachesolve() computes or retrieves the already computed inverted matrix for list passed in as its argument

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
