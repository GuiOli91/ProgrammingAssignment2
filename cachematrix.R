## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        Inv_Matrix <- NULL
        
        setmatrix <- function(y){
                x <<- y
                Inv_Matrix <<- NULL
        }
        
        getmatrix <- function() x
                
        setinverse <- function(Inverse) Inv_Matrix <<- Inverse
        
        getinverse <- function() Inv_Matrix
        
        list(setmatrix = setmatrix, getmatrix = getmatrix, 
             setinverse = setinverse, getinverse = getinverse)
       
}


## Write a short comment describing this function


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        Inv <- x$getinverse()
        
        if(!is.null(Inv)) {
                message("Returning cached Inversed")
                return(Inv)
        } else {
                
                data <- x$getmatrix()
                Inv <- solve(data, ...)
                x$setinverse(Inv)
                Inv
        }

}
