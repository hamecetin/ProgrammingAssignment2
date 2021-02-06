## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setmtx <- function(y){
    x <<- y
    inv <<- NULL
  }
  getmtx <- function() {x}
  setinv <- function(s)  {inv <<- s}
  getinv <- function() {inv}
  list( 
    set = setmtx , get = getmtx , setinv = setinv, getinv = getinv
  )
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached solution")
    return(inv)
  }
  mtx <- x$get()
  inv <- solve(mtx, ...)
  x$setinv(inv)
  inv
}
