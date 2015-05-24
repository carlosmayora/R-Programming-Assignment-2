## R programming course: Assignment 2: Caching the Inverse of a Matrix.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function() x
  set_inv<-function(solve) i<<-solve
  get_inv<-function() i
  list(set=set,get=get,set_inv=set_inv,get_inv=get_inv)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i<-x$get_inv()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  matrix<-x$get()
  i<-solve(matrix)
  x$set_inv(i)
  i
}
