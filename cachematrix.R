

## makeCacheMatrix function gets a new matrix and sets NULL matrix I  
##which will later store inverse of matrix after being solved 
makeCacheMatrix <- function(x = matrix()) {
I<-NULL
 set<-function(y){
   x<<-y
   I<<-NULL
 }
 get<-function()x
 setinverse<-function(inverse) I<<-inverse
 getinverse<-function() I
 list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## cachesolve function finds inverse of the matrix if its not solved already,
## and saves it in cache.
## If the matrix is alread solved,  it just gets the result from cache

cacheSolve <- function(x, ...) {
I<-x$getinverse()
if(!is.null(I)){
  message("getting cached inverse")
  return(I)
}
d<-x$get()
I<-solve(d,...)
x$setinverse(I)
I
        ## Return a matrix that is the inverse of 'x'
}


##Example of the usage of function 
##a<-matrix(c(1,0,0,0,1,0,0,0,1),nrow=3,ncol=3)
## v<-makeCacheMatrix()
## V$set(a)
##v$get()
##[,1] [,2] [,3]
##[1,]    1    0    0
##[2,]    0    1    0
##[3,]    0    0    1
##cacheSolve(v)
##[,1] [,2] [,3]
##[1,]    1    0    0
##[2,]    0    1    0
##[3,]    0    0    1
##> cacheSolve(v)
##getting cached inverse
##[,1] [,2] [,3]
##[1,]    1    0    0
##[2,]    0    1    0
##[3,]    0    0    1
