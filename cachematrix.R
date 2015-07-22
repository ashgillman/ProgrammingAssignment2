## A cached implementation of a matrix. Stores the matrix inverse internally so
## that the R solve() function need only be called once. Subsequant calls will
## result only in a memory lookup rather than a matrix inversion computation.

## makeCacheMatrix created a cachable "matrix". This returns as a list with of
## the following functions:
## - set: set the matrix value
## - get: get the matrix value
## - set.inv: set the cached inverse.
## - get.inv: get the cached inverse, or NULL if it has not yet been set.

makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL # default value for inverse

  # set and get simply access/mutate "mat".
  # set also resets inv to default
  set <- function(value) {
    mat <<- value
    inv <<- NULL
  }
  get <- function() mat

  # accessor/mutator for inverse. Should not be used by end-users, only for
  # cacheSolve()
  set.inv <- function(inverse) inv <<- inverse
  get.inv <- function() inv

  # return methods in a list. Closures ensure data is bound
  list(set=set, get=get, set.inv=set.inv, get.inv=get.inv)
}


## A wrapper for R's solve() function for use with a cacheMatrix. Requires a
## cacheMatrix created with makeCachematrix as x, along with other parameters
## for solve(), and returns the inverse. The inverse itself is calculated only
## if required, otherwise the cached value is looked up.

cacheSolve <- function(x, ...) {
  inv <- x$get.inv() # load cached inverse
  if (is.null(inv)) { # calculate the inverse if it hasn't been found already
    mat <- x$get()
    inv <- solve(mat)
    x$set.inv(inv)
  }
  inv # return the inverse
}
