
## This function will create two objects x and i, then define these two objects, and eventually create a list
## containing x and i as well as four functions,

makeCacheMatrix <- function(x = matrix()) {
          i <- NULL
          set <- function(y){
                    y <<- x
                    i <<- NULL
          }
          get <- function() x
          setinverse <- function(inverse) i <<- inverse 
          getinverse <- function() i
          list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function will first check if the inverse has been defined or not. If yes, return the defined inverse; If
## not, compute the inverse then print it.

cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'x'
          i <- x$getinverse()
          if(!is.null(i)){
                    message("getting cached data")
                    return(i)
          }
          data <- x$get()
          i <- solve(data, ...)
          x$setinverse(i)
          i
}

## Thanks for reviewing my assignment
