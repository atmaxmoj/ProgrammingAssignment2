## This is the first function "makeCacheMatrix"

makeCacheMatrix <- function( m = matrix() ) {
  
  ## Declare the type to be matrix
  ## Initialize the inverse
  
  inv <- NULL
  
  ## Set the matrix
  
  set <- function( matrix ) {
    m <<- matrix
    inv <<- NULL
  }
  
  ## Get m
  
  get <- function() {
    ## Return the matrix
    m
  }
  
  ## Set the inverse of m
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## Get the inverse of m
  getInverse <- function() {
    ## Return the inverse
    inv
  }
  
  ## Return the list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Here is the second function 'cacheSlove'

cacheSolve <- function(x, ...) {
  
  ## Return the inverse of 'x'
  m <- x$getInverse()
  
  ## Return the inverse conditionally
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from the object
  data <- x$get()
  
  ## Find the inverse
  m <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return m
  m
}
