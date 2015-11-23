## A matrix may be input into the makeCacheMatrix function,
## Within it are functions that are used to manipulate the matrix
## cacheSolve takes in the makeCacheMatrix (with a inputted matrix)
## It solves for the inverse of the matrix

## makeCacheMatrix takes a matrix as an input. The functions
## within it can load the matrix to a variable, set the matrix, 
## load the inverse of the function to a variable, or set the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {    ## assigns a new matrix to the makeCacheMatrix function
    x <<- y    ## <<- is needed to assign x in the overall makecacheMatrix function rather than just the set function
    inv <<- NULL
  }
  get <- function() x                ## Retrieves the matrix
  setinv <- function(invmat) inv <<- invmat     ##assigns a new inverse matrix value to cache
  getinv <- function() inv               ##retrieves the inverse matrix from cache
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function takes a matrix within makecacheMatrix as and input and returns the inverse of the matrix to cache

cacheSolve <- function(x, ...) {
  inv <- x$getinv()   ## retrieves the inverse matrix if it exists            
  if (!is.null(inv)){       ## triggers if the inverse matrix exists within the cache
    print("data in cache, retrieving")
    return (inv)
  }
  z <- x$get()    ##assigns the matrix to variable
  inv <- solve(z, ...)    ## This function is what is actually inverting the matrix
  x$setinv(inv)       ## storing the inverse matrix in cache
  inv
        ## Return a matrix that is the inverse of 'x'
}
