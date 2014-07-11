## makeCacheMatrix and cacheSolve are functions for caching the inverse matrix


## makeCacheMatrix function contains 4 functions.						#
## use "(variable) <- makeCacheMatrix()" 								#
## or "(variable) <- makeCacheMatrix('variable that contain matrix')"	#
## $set will set matrix													#
## $get will show you cached matrix										#
## You can set inverse matrix yourself by using $setsolve				#
## You can get cached inverse matrix by $getsolve						#


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


## cacheSolve function check whether there is cached inverse matrix or not
## If there is, return $getsolve
## Else, calculate inverse matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getsolve()
  
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  data<-x$get()
  
  ## Calculate the Determinant of a Matrix.
  ## if det(x)==0, than this matrix can`t have inverse matrix.
  if(det(data)==0){
    message("This matrix doesn`t have inverse matrix.")
  }
  
  ## if det(x)!=0, get solve.
  else{
    m<-solve(data, ...)
    x$setsolve(m)
    m
  }
}