## makeCacheMatrix and cacheSolve are functions for caching the inverse matrix


## makeCacheMatrix function contains 4 functions.
##
## use "(variable) <- makeCacheMatrix()" 								
## or "(variable) <- makeCacheMatrix('variable that contain matrix')"	
## $set will set matrix in makeCacheMatrix function
## $get will show you cached matrix in makeCacheMatrix function
## You can set inverse matrix yourself by using $setsolve				
## You can get cached inverse matrix by $getsolve
## Check Examples in tail of this script


makeCacheMatrix <- function(x, ...) {
  m<-NULL
  set<-function(y, ...){
    if (is.matrix(y)==T){
      x<<-y
    }
    else{
      x<<-matrix(y, ...)
    }
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
    stop("This matrix doesn`t have inverse matrix.")
  }
  
  ## if det(x)!=0, get solve.
  else{
    m<-solve(data, ...)
    x$setsolve(m)
    m
  }
}

###############################################
###############################################
############Example 1
#
#> example.matrix<-makeCacheMatrix()
#> example.matrix$set(1:4, nrow=2, ncol=2)
#> example.matrix$get()
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4
#> cacheSolve(example.matrix)
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#
###############################################
############Example 2
#
#> example.matrix2<-makeCacheMatrix()
#> example.matrix2$set(1:9, nrow=3, ncol=3)
#> example.matrix2$get()
#     [,1] [,2] [,3]
#[1,]    1    4    7
#[2,]    2    5    8
#[3,]    3    6    9
#> cacheSolve(example.matrix2)
#Error in cacheSolve(example.matrix2) : 
#  This matrix doesn`t have inverse matrix.
#
###############################################
############Example 3
#
#> example1<-matrix(1:4, nrow=2, ncol=2)
#> example.matrix3<-makeCacheMatrix()
#> example.matrix3$set(example1)
#> example.matrix3$get()
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4
#> cacheSolve(example.matrix3)
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> example.matrix3$getsolve()
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#
###############################################
###############################################