## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function provides the means by which
## a matrix can be set in cache and and retrieved

makeCacheMatrix <- function(x = matrix()) {
  ## initialize the local variable to hold the inverted matrix
  i <- NULL
  ## the set function sets the matrix to be inverted and sets the inverse matrix to NULL
  set <- function(y) {
    x <<- y  ## assign y to the matrix in the parent environment
    i <<- NULL   ## set the inverse matrix to NULL in the parent environment
  }
  get <- function() x  ## returns the matrix
  setinverse <- function(solve) i <<- solve ## sets the inverse matrix in the parent environment
  getinverse <- function() i  ## returns the inverse matrix from cache i.e. the parent environment
 
  ## return a list that contains the methods that an object created by this
  ## function can use to manipulate the matrix x.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of an invertible matrix and stores
## it cache so it can be retrieved later without needing recalculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## see if the inverse matrix has already been calculated
  i <- x$getinverse()
  if(!is.null(i)) {
    ## it has been already calculated and exists in cache 
    ## therefore return the cached inverted matrix
    message("getting cached data")
    return(i)
  }
  ## if code execution gets to this point then it means that
  ## the inverted matrix has not been previously calculated; therefore calculate it
  data <- x$get()  ## assign the matrix to variable 'data'
  i <- solve(data, ...)  ## calculate the inverse matrix and store it in the local variable i
  x$setinverse(i)  ## assign the local inverted matrix to the cached inverted matrix 
  i ## return the local invereted matrix
}
