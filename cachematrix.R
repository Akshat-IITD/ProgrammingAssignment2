## Put comments here that give an overall description of what your
## functions do

## This code will define a new type of matrix which will have cached data during 
## inverse calculation using the Solve function. Now at the execution of the
## other function which gets the cached inverse, it'll first check for a pre-existing
## inverse. 

## Write a short comment describing this function
## Makes a cache matrix which runs the sets the 4 functions, set, get, setinv, getinv
## which in turn execute in their specified environments.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function
## This function looks for the cached inverse of the Special Matrix that we have 
## defined and if it exists, returns it else calculates using the Solve function. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
