## this function is able to cache inverse of a matrix rather than 
## computing it repeatedly

## the following function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setInverse<-function(inverse) m<<-inverse
  getInverse<-function() m
  list(set = get,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## the following function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has been calculated then 
## cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  m <-x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  ## Return a matrix that is the inverse of 'x'
  data<- x$get()
  m<-solve(data,...)
  x$setInverse(m)
  m
}
