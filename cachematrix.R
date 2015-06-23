## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Create a function with getter&setter functions to matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(solve) m <<- solve
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)

}


## Write a short comment describing this function
## cache the results if exisitng otherwise, calculate and save it for later lookup
## It can be run like this:
##> a = makeCacheMatrix(matrix(c(1:4),nrow = 2))
##> a$get()
##     [,1] [,2]
##[1,]    1    3
##[2,]    2    4

##> cacheSolve(a)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(a)
##getting cached data
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}
