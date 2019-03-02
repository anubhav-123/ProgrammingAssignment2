## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
  a<-NULL
  f1<-function(y) 
  {
    x<<-y
    a<<-NULL
  }##defines the function to set the vector x to a new vector y and reset a to null
  get<-function() x ##return vector x
  setInverse<-function(inverse) 
    a<<-inverse
  getInverse<-function() a ##return a
  list(f1=f1,                  ##given name 'f1' to f1() function defined earlier
       get=get,                ##given name 'get' to get() function defined earlier
       setInverse=setInverse,  ##given name 'si' to si() function defined earlier
       getInverse=getInverse)  ##given name 'gi' to gi() function defined earlier  
}##return special vectr of functions defined




## Write a short comment describing this function

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()
  if(!is.null(a)) 
  {
    return(a)##getting cached data
  }
  mat<-x$get()
  a<-solve(mat, ...)
  x$setInverse(a)
  a
}
