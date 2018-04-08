## MakeCacheMatrix function returns a vector of matrices based on the input matrix
##cacheSolve function returns the inverse of the created matrix vector after
##checking in the cache if the inverse is already calculated.

## This function creates a vector of matrices by defining four functions.
##The set functions sets m as a null matrix and x as the input vector using the
##lexical scoping operator <<
##The get function returns the X matrix set using the set function
##The setinv function sets the value of m as the matrix inv
##The getinv function returns m

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv=matrix()) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function uses the getinv function to test the value of m
## If the value of m is null, it calculates the inverse using solve()
## If the value of m is not null, it will return the value of m from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
