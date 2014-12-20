## makeCasheMatrix creates a matrix to store and retrive from cache



makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve calculates the inverse of the matrix, first checking to see if it is already in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- solve(x)
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- getsolve()
    m <- solve(data, ...)
    setsolve(m)
    m
  
  
}

