## Assignmnet for week 3
## A way to save a calculated matrix inverse. Data structure keeps matrix and it inverse together.
## TESTS: 3 tests at bottom to show it works as expected.

## Data struture to store a matrix and to store its inverse.

makeCacheMatrix <- function(x = matrix() ) {
  minverse <- NULL
  set <- function(y) {
    x <<- y
    minverse <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) minverse <<- Inverse
  getInverse <- function() minverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Uses Cach data structure to get saved inverse. If it has not been
## calculated yet, then it makes.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

# TEST 1: makeCacheMatrix
#> mcx <- makeCacheMatrix(mx)
#> mcx$set(mx)
#> mcx$get()
#[,1] [,2]
#[1,]    1    2
#[2,]    3    4
#> mcx$setInverse(solve(mx))
#> mcx$getInverse()
#[,1] [,2]
#[1,] -2.0  1.0
#[2,]  1.5 -0.5

#TEST 2: cacheSolve
# > cacheSolve(mcx)
#getting cached inverse
#[,1] [,2]
#[1,] -2.0  1.0
#[2,]  1.5 -0.5
#

#TEST 3: update matric and use cache
#> mx2 <- rbind(c(5,6),c(7,8))
#> mcx$set(mx2)
#> cacheSolve(mcx)
#[,1] [,2]
#[1,] -4.0  3.0
#[2,]  3.5 -2.5
#> note: no message "getting cached inverse" as matrix cahnged and new invesrse built.
