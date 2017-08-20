## this code would support a use case of matrix inversion
## author: Jim Rhudy, 8/20/2017

## This function defines a matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  # set the matrix
  set<-function(y) {
    x<<- y
    m<<- NULL
  }
  # get the matrix
  get<-function() x
  # set value of inverse
  setinverse<-function(solve) m<<-solve
  # get value of inverse
  getinverse <-function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function inverts a matrix if it has not already been 
## inverted and if the matrix has not changed

cacheSolve <- function(x, ...) {
  # check if matrix has been inverted
  m<-x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # invert the matrix
  data<-x$get()
  m<-solve(data, ...)
  x$setinverse(m)
  m
}

# here is some code used to test the functions
# # a<-c(8, 2, 4)
# b<-c(8, 0, 5)
# c<-c(8, 2, 0)
# d<-cbind(a, b, c)
# d
# ds<-solve(d)
# ds
# makeCacheMatrix(d)
# cacheSolve(makeCacheMatrix(d))

