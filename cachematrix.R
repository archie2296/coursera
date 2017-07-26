## Put comments here that give an overall description of what your
## functions do

##cacheMatrix.R contains two functions makeCacheMatrix() to get,set the matrix and get and set its inverse,
##cacheSolve() to cache inverse of matrix if inverse already present since it is expensive to calculate inverse of a matrix every single time

## Write a short comment describing this function

## makeCacheMatrix() has 4 steps in it:
##It takes a matrix as its arguement.
##It gets the matrix through get()
##It sets this matrix in x through the operator <<- and sets the inverse initially to null
##Inverse is set through setInverse()
##inverse of matrix is retrieved through getInverse()
##A list is returned with the functions assigned to variables set,get,setInverse,getInverso so as to call them outside the makeCacheMatrix()

makeCacheMatrix <- function(x = matrix()) {
  
  set<-function(i){
    x<<-i
    invers<<-NULL
  }
  get<-function(){
    x
  }
  setInverse<-function(inverseValue){
    invers<<-inverseValue
  }
  getInverse<-function(){
    invers
  }
  
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function

##cacheSolve() is a function with the matrix whose inverse is to be found as its arguement.
##inverse is retrived using getInverse() but this value is checked for null,if it is not null computation of inverse
##is skipped and cached value is returned
##get() retrives the matrix whose inverse is to be found
##inverse is set using setInverse()
##inverse of matrix is returned

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseV<-x$getInverse()
  if(!is.null(inverseV)){
    message("cached data")
    return(inverseV)
  }
  data<-x$get()
  inverseV<-solve(data)
  x$setInverse(inverseV)
  return(inverseV)
}
