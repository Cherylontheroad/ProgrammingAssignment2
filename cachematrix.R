#Week 3 Peer-graded assignment

#Function 1: makeCacheMatrix: creates a special 
#"matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
            
            inversematrix <- NULL
            
            set <- function(y) {
                  x <<- y
                  inversematrix <<- NULL
            }
            
            get <- function() {
                  x
            }
            
            setinversematrix <- function(inversematrix) {
                  inversematrix <<- inversematrix
            }
            
            getinversematrix <- function() {
                  inversematrix
            } 
            
            list(set = set, 
                 get = get,
                 setinversematrix = setinversematrix,
                 getinversematrix = getinversematrix)
            
      } 

      
#Function 2: cacheSolve: This function computes the inverse of
#the special "matrix" returned by makeCacheMatrix above. If the 
#inverse has already been calculated, then the cashsolve should 
#retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
      
      inversematrix <- x$getinversematrix()
      
      if(!is.null(inversematrix)) {
            message("getting cached inverse matrix")
            return(inversematrix)
      }
      
      matrix <- x$get()
      
      inversematrix <- solve(matrix, ...)
      
      x$setinversematrix(inversematrix)
      
      inversematrix
}
