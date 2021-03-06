## Put comments here that give an overall description of what your
## functions do

##############################################################################
## How to test caching on an example
##############################################################################
##
## We can choose an Hilbert matrix available in the Matrix library
##
## > library(Matrix)
## > M <- as.matrix(Hilbert(10)) ## Caution: using a dimension above 11 will be lead to computational singularity
## > MC <- makeCacheMatrix(M)
## > cacheSolve(MC) ## Should return the inverted matrix without message "getting cached data"
## > cacheSolve(MC) ## Should return the inverted matrix with message "getting cached data"

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    ## Initialize cached value in scope
    ## of the function
    cached <- NULL
    
    ## Define function handlers for retrieve, compute,
    ## and caching any operation on a matrix. We are
    ## not assuming any particular operation. We are
    ## only caching the result of the operation based
    ## on passed matrix. The computation is done outside
    ## of this function
    setMatrix <- function(y) {
        x <<- y
        cached <<- NULL
    }
    getMatrix <- function() x
    setMatrixFunc <- function(computed) cached <<- computed
    getMatrixFunc <- function() cached
    
    ## Return the list of function handlers
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setMatrixFunc = setMatrixFunc,
         getMatrixFunc = getMatrixFunc)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    
    ## Retrieve the cached inverse if already
    ## processed
    matInv <- x$getMatrixFunc()
    if(!is.null(matInv)) {
        message("getting cached data")
        return(matInv)
    }
    
    ## Compute the matrix inverse
    currentMatrix <- x$getMatrix()
    matInv <- solve(currentMatrix, ...)
    x$setMatrixFunc(matInv)
    
    ## Return a matrix that is the inverse of 'x'
    matInv
}



