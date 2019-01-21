################################################################
## User: Guilherme Monteiro Oliveira   Git.Login: GuiOli91    ##
## Programming Assignment 2: Lexical Scoping                  ##
## Date: 20.01.2019                    Local: Brasil          ##
################################################################

##The following functions create a "special" matrix to avoid unnecessary reverse 
## matrix calculations. When the inverse matrix is calculated a lot through the 
## code, this can be time-consuming for the processor. Thus, these functions 
## work together to save the inverse matrix in cache.

## MakeCacheMatrix creates a list of function to:
# 
# 1) setmatrix - Set the value of the Matrix
# 
# 2) getmatrix - Get the value of the Matrix
# 
# 3) getinverse - Get the value of the inverse matrix
# 
# 4) setinverse - Set the value of the inverse Matrix

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


## The cacheSolve receives a list of makeCacheMatrix and calculates the inverse 
##Matrix, if it was not calculated before or returns the inverse matrix saved 
##in memory. No test was implemented how it was stated on the assignment.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        Inv <- x$getinverse()
        
        if(!is.null(Inv)) {
                message("Returning cached Inverse Matrix")
                return(Inv)
        } else {
                
                data <- x$getmatrix()
                Inv <- solve(data, ...)
                x$setinverse(Inv)
                Inv
        }

}
