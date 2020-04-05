## Both functions cache the inverse of a matrix rather 
## than compute it repeatedly

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <- NULL
  }
  get <- function() x
  setinversa <- function(i) m <<- i
  getinversa <- function() m
  list(set=set, get=get, setinversa=setinversa, getinversa=getinversa)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
##            If the inverse has already been calculated (and the matrix has not changed)
##            then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinversa()
  if(!is.null(m)){
    message("getting cache data")
    return(m)
  }
  matriz <- x$get()
  m <- solve(matriz)
  x$setinversa(m)
  m
}
