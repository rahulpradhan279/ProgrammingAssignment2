## The first function makecacheMAtrix does the following ; sets and gets matrix ,sets and gets inverse matrix
## the purpose of using cached data is to reduce time in calculating the inverse of the same matrix again


makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  
  get<-function()x
  
  setinverse<-function(inverse)i<<-inverse
  
  getinverse<-function()i
  
  list (set=set ,
        get=get,
        setinverse=setinverse,
        getinverse=getinverse)
  
}


## The function creates a inverse matrix . If the matrix data is same then the chached inverse Matrix data is returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    i<-x$getinverse()
    
    if(!is.null(i)){
      message("Getting Cached Data")
      return(i)
    }
    data<-x$get()
    i<-solve(data,...)
    x$setinverse(i)
  }
