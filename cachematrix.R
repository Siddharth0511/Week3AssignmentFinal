## This Creates a list of setters and getters for a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) 
{
  m<-matrix(,nrow=nrow(x),ncol=ncol(x)) # declaring a null matrix
  set <-function(y)
  {
  x<<-y # for re-setting a matrix if required
  m<<-matrix(,nrow=nrow(x),ncol=ncol(x)) # resetting the matrix to null if x is reset
  }
  get<-function() x
  setinverse<-function(inverse) m<<-inverse # set m as the inverse matrix
  getinverse<-function() m # retrieve the inverse value
  list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
}


## Retrieves the inverse from cache or calculate the inverse of the matrix created through makeCacheMatrix()

cacheSolve <- function(x, ...) 
{
  m<-x$getinverse() # retrieving the inverse from Cache
  if(!all(is.na(m))) # Checking if all elements of m are not null
    {
    message("getting cached data") # if not null, message that data is retrieved from cache
    return(m) # return the cached inverse matrix
    }
  data<-x$get() # fetch the matrix
  m<-solve(data) # calculate the inverse of matrix
  x$setinverse(m) # set m as the calculated inverse
  m # Return the inverse
}