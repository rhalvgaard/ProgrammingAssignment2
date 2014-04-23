## cacheMatrix contains two functions that are used to create a special object
## that stores a numeric square invertable matrix 'X' and cache's its inverse.

## makeCacheMatrix stores the invertable square matrix 'X' and
## returns a list of functions. These functions are able to
## calculate and store the matrix inverse of X.

makeCacheMatrix <- function(X = numeric()) {
  m <- NULL
  set <- function(Y) {
    X <<- Y
    m <<- NULL
  }
  get <- function() X
  setinverse <- function(inverse) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns the inverse of X by applying the functions 
## defined in makeCacheMatrix. If the inverse already has been calculated and
## stored in memory, the inverse calculation is skipped and the stored result
# is returned.

cacheSolve <- function(X, ...) {
  ## Return a matrix that is the inverse of 'X'
  m <- X$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- X$get()
  m <- solve(data)
  X$setinverse(m)
  m
}
