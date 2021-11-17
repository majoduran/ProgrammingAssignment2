## These functions will store the inverse of a matrix internally,
## avoiding recomputing, working faster!

## This function creates a special vector that stores the inverses of matrixes.

makeCacheMatrix <- function(x = matrix()) {
  invv <- NULL
  set <- function(y) {
    x <<- y
    invv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invv <<- inverse
  getinverse <- function() invv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of the matrix that is going to be stored in the function above.

cacheSolve <- function(x, ...) {
  invv <- x$getinverse()
  if(!is.null(invv)) {
    message("getting matrix inverse")
    return(invv)
  }
  matx <- x$get()
  invv <- solve(matx, ...)
  x$setinverse(invv)
  invv
}
