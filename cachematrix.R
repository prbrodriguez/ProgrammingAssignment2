## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix, makeCacheMatrix are the the two function
## set,get,setInv, getInv makes up makeCacheMatrix
## To calculate the inverse for non squared as well as square matrix library (Mass) is used
makeCacheMatrix <- function(x = matrix()) {
  prr<-NULL
  set<- function(y){
    x<<-y
    prr<<-NULL
  }
  get<-function()x            #function to get matrix x
  setInverse<-function(inverse)prr<<-inverse
  getInverse<-function()prr     #function to obtain the matrix's inverse
  
  list(set=set, get=get,
       setInverse =setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this function
## This function is used to get the data of cache
cacheSolve <- function(x, ...) ##gets cache data
{
  prr<-x$getInverse()
  if(!is.null(prr)){        #To check if the inverse is null
    message ("getting cached data!")
    return(prr)  #return inverse value  
  }
  data<-x$get()
  prr<-solve(data, ...)
  x$setInverse(prr)
  prr ## Return a matrix that is the inverse of 'x'
}
