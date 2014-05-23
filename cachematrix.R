# Function: makeCacheMatrix
#
# Purpose
#  Encapsulates an invertible matrix, its inverse 
#  and get/set operations for them
#
# Input Arguments
#   x - an invertible square matrix
# Return value
#   a list containing the functions get, set, getInverse
#   and setInverse that are defined within makeCacheMatrix
#
# The function can be used as follows :-
#  v <- makeCacheMatrix(
#    matrix(c(1,0,0,0,1,0,0,0,1),nrow=3,ncol=3,byrow=TRUE)
#    )
# The supplied argument is a 3x3 identity matrix
#
# Subsequently, the internal functions can be invoked as
# v$get, v$set etc.
#
# set - stores the input matrix
# get - returns the matrix stored by set
# setInverse - stores the input inverse matrix
# getInverse - returns the inverse matrix stored by
# setInverse or NULL if nothing was stored
#
# x - variable that stores the input matrix
# inv - variable that stores the inverse, initially set to NULL
#

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# Function: cacheSolve
#
# Purpose
#  Computes and caches the inverse of an invertible matrix
# 
# Input Arguments
#   x - a special list created using makeCacheMatrix above
# ... - any other arguments to be passed on to the solve function
# Return value
#   a list containing the functions get, set, getInverse
#   and setInverse that are defined within makeCacheMatrix
#
# The usage of the function is illustrated below.
#
# > v <- makeCacheMatrix(
#          matrix(c(1,2,3,0,1,4,5,6,0), nrow=3,ncol=3, byrow=TRUE)
#         )
# > m <- cacheSolve(v)
# > m
# [,1] [,2] [,3]
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1
# > m <- cacheSolve(v)
# getting cached data i.e. inverse computed and stored earlier
# > m
# [,1] [,2] [,3]
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1
# 
# We can also change the input matrix using v$set and the 
# inverse will be calculated afresh, and the new value 
# cached.
#
# > v$set(matrix(c(1,0,0,0,1,0,0,0,1),nrow=3,ncol=3,byrow=TRUE))
# > m <- cacheSolve()
# > m
#      [,1] [,2] [,3]
# [1,]    1    0    0
# [2,]    0    1    0
# [3,]    0    0    1
# > m <- cacheSolve(v)
# getting cached data i.e. inverse computed and stored earlier
# > m
# [,1] [,2] [,3]
# [1,]    1    0    0
# [2,]    0    1    0
# [3,]    0    0    1
#
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data i.e. inverse computed and stored earlier")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
