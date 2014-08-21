##These functions allow the computation of the inverse of a matrix to be 
##cached so that once it is computed it will be stored in memory and recalled
##if needed to be used rather than recomputing it
# These are two functions that are used to create a special object
# that stores a matrix and caches its inverse

# makeCacheMatrix creats a list containing functions to
# 1 set the value of the matrix
# 2 get the value of the matrix
# 3 set the value of the inverse
# 4 get the value of the inverse

makeCacheMatrix<-function(X){
  I<-matrix(0,dim(X)[1],dim(X)[1])
  set<-function(Y) {    #function to set x
    X<<-Y
    I<<-matrix(0,dim(X)[1],dim(X)[1])
  }
  get<-function() X    #function to get x
  setsolve<-function(solve) I<<-solve    #function to solve x
  getsolve<-function() I    #function to get the inverse matrix m
  list(set=set, get=get,
       setsolve=setsolve,
       getsolve=getsolve)
}

# The following function calculates the inverse of the special matrix
# created with the above function. However it checks to see if the 
# inverse has already been calculated. If so it gets the inverse from
# the cache. Otherwise, it calculates the inverse and sets the value
# of the inverse in the cache via the setmean function.


cacheSolve<-function(X) {
  I<-X$getsolve()
  if(sum(X)!=0){
    message("getting cached data")
    return(I)    #return the already calculated inverse
  }
  M<-X$get()
  I<-solve(M)    #calculate the inverse
  X$setsolve(I)    #set the value of the inverse in the cache
  I    #print the inverse
}