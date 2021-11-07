## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    ## Initialize cached value in scope
    ## of the function
    xDim = dim(x)
    cached <- matrix(, nrow=xDim[1], ncol=xDim[2])
    
    ## Define function handlers for retrieve, compute,
    ## and caching any operation on a matrix. We are
    ## not assuming any particular operation. We are
    ## only caching the result of the operation based
    ## on passed matrix. The computation is done outside
    ## of this function
    setMatrix <- function(y) {
        x <<- y
        yDim = dim(y)
        cached <<- matrix(, nrow=yDim[1], ncol=yDim[2])
    }
    getMatrix <- function() x
    setMatrixFunc <- function(computed) cached <<- computed
    getMaxtrixFunc <- function() cached
    
    ## Return the list of function handlers
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setMatrixFunc = setMatrixFunc,
         getMatrixFunc = getMatrixFunc)
}


## Write a short comment describing this function

 <- function(x, ...) {
    
    ## Retrieve the cached inverse if already
    ## processed
    matInv <- x$getMatrixFunc()
    if(!is.null(matInv)) {
        message("getting cached data")
        return(matInv)
    }
    
    ## Compute the matrix inverse
    currentMatrix <- x$getMatrix()
    matInv <- solve(currentMatrix)
    x$setMatrixFunc(matInv)
    
    ## Return a matrix that is the inverse of 'x'
    matInv
}
