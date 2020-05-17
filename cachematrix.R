## This programs solves the inverse of a matrix with an assumption that
## the inverse of the matrix exists and then caches it. 

## This function stores the matrix and the inverse of the matrix 
## outside the current environment.This function returns a list containing functions to get or set the matrix from or to
## outside the current environment and to get or set the inverse of the matrix from or to outside the current environment

makeCacheMatrix <- function(x = matrix()) {
    inv_mat<- NULL
    
    # sets the matrix
    setMat<- function(my_mat){
        x <<- my_mat
        inv_mat <<- NULL
    }
    
    # gets the matrix 
    getMat <- function() {
        x
    }
    
    # Sets the inverse of the matrix
    setInvMat <- function(inverse) {
        inv_mat <<- inverse
    }
    
    # Gets the inverse of the matrix
    getInvMat <- function() {
        inv_mat
    }
    
    # returns the list of functions
    list(setMat = setMat, getMat = getMat,
         setInvMat = setInvMat,
         getInvMat = getInvMat)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    # Fetches the inverse of matrix from makeCacheMatrix functions
    inv_mat <- x$getInvMat()
    
    # if inverse exists return as is
    if(!is.null(inv_mat)) {
        message("getting cached data for inverse of matrix")
        return(inv_mat)
    }
    
    # if inverse doesnt exist, fetch the matrix, calcualte and set it back 
    my_mat <- x$getMat()
    inv_mat <- solve(my_mat, ...) 
    x$setInvMat(inv_mat)
    
    #return the inverse of matrix calculated
    inv_mat
}
