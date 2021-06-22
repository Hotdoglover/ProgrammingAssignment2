## The following functions create a matrix, and cahce its inverse for
## future use. If no inverse is cached, the inverse is calculated and cached

## This function sets, the data (in this case a matrix), gets the matrix values,
## sets the values of the matrix's inverse, and gets the inverse values

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function uses the data set in the previous function to either return 
## the inverse set in that function, or, if no inverse was set, 
## calculates the inverse of the matrix set in that function

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(i)
  i
}
