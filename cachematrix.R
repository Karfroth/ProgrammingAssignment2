## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y, ...){
    x<<-matrix(y, ...)
    m<<-NULL
  }
  get<-function() {
    x
  }
  setsolve<-function(solve) {
    m<<- solve
  }
  getsolve<-function() {
    m
  }
  list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getsolve()
  
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  data<-x$get()
  
  ## Calculate the Determinant of a Matrix.
  ## if det == 0, than this matrix can`t have inverse matrix.
  if(det(data)==0){
    message("This matrix doesn`t have inverse matrix.")
  }
  
  ## if det !=0, get solve.
  else{
    m<-solve(data, ...)
    x$setsolve(m)
    m
  }
}