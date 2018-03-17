## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## This function is basically an object that manages 
  ## a cache of an inversed matrix 
  ## functions:
  ##    getMatrix - returns the matrix unchanged
  ##    setMatrix - sets a Matrix
  ##    setCachedMatrix - keep the cached matrix
  ##    getCachedMatrix - returns the cached matrix
		cachedMatrix <- NULL
        setMatrix <- function(y) {
                x <<- y
                cachedMatrix <<- NULL
        }
        getMatrix <- function() x
        setCachedMatrix <- function(inverse) cachedMatrix <<- inverse
        getCachedMatrix <- function() cachedMatrix
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setCachedMatrix = setCachedMatrix,
             getCachedMatrix = getCachedMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## The function stores the cached matrix in the 
        ## in the x object & updates it with an inversed 
        ## matrix if it has not been instantiated.
		cachedMatrix <- x$getCachedMatrix()
        if(!is.null(cachedMatrix)) {
                message("getting cached matrix")
                return(cachedMatrix)
        }
        matrix <- x$getMatrix()
        cachedMatrix <- solve(matrix)
        x$setCachedMatrix(cachedMatrix)
        cachedMatrix
}
