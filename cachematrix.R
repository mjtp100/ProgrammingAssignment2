# makeCacheMatrix takes a matrix object as an argument and returns
# a list of accessor functions to "cached" variables that store the input
# matrix data and previously calculated inversions of the input matrix,
# in a different environment

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) m <<- inv
  getInverse <- function() m
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}

# cacheSolve takes the function list created by makeCacheMatrix as input as well
# as any additional arguments to solve() and either calculates and caches the inverse
# of the matrix or retrieves a pre-calculated inverse of the matrix from the cache 

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}