## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix, makeCacheMatrix are the the two function
## set,get,setInv, getInv makes up makeCacheMatrix
## To calculate the inverse for non squared as well as square matrix library (Mass) is used
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
inv<-NULL
set<- function(y){
                  x<<-y
                  inv<<-NULL
                  }
get<-function()x            #function to get matrix x
setinverse<-function(inverse)inv<<-inverse
getinverse<-function(){
                    inver<-ginv(x)
                    inver%*%x        #function to obtain the matrix's inverse
                    }
list(set=set, get=get,
     setinverse = setinverse, 
     getinverse = getinverse)
}


## Write a short comment describing this function
## This function is used to get the data of cache
cacheSolve <- function(x, ...) ##gets cache data
  {
  inv<-x$getinverse()
  if(!is.null(inv)){        #To check if the inverse is null
                    message ("getting cached data!")
                    return(inv)  #return inverse value  
  }
  data<-x$get()
  inv<-solve(data, ...)
  x$setinverse(inv)
  inv ## Return a matrix that is the inverse of 'x'
}
