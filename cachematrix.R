## Following two functions are useful to reverse a matrix which is a costly 
## computation. Hence these functions cache the inverse of a matrix and use it
## when required.

## makeCacheMatrix accepts a matrix as an input and performs 4 activities on
## the passed matrix which is passed as a argument. 
## sets Value of matrix, gets value of matrix, 
## sets inverse of matrix, gets inverse of matrix

makeCacheMatrix <- function(x = matrix()) 
{
    invMat <- NULL
    set <- function(y)
    {
        x <<- y
        invMat <<- NULL
    }
    get <- function () x
    setInv <- function (inv) invMat <<- inv
    getInv <- function () invMat
    list(set = set,
         get = get,
         setInv = setInv,
         getInv = getInv)
}


## cacheSolve() either calculates and returns the inverse of the matrix passed to 
## makeCacheMatrix() or retrives the cache inverse.

cacheSolve <- function(x, ...) {
    invMat <- x$getInv()
    if(!is.null(invMat))
    {
        message("Getting cached inverse of matrix")
        return(invMat)
    }
    result <- x$get()
    invMat <- solve(result)
    x$setInv(invMat)
    invMat
}
