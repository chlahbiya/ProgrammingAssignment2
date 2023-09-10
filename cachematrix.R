## I created two functions, the first one create a matrix (a squared one) and the second one inverse the matrix created by the first function.
## However, it first checks to see if the matrix's inverse has already been calculated.If so, it gets the matrix inverse from the cache 
##and skips the computation.Otherwise, it sorts the data and sets the inversed matrix in the cache via the setinv function.


## This function creat a matrix and stor it in cache.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function takes the matrix from the previous function and inverse it.

cacheSolve <- function(x, ...) {
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
