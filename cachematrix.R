## cacheSolve inverts a matrix. If it has been inverted before, it will retrieve the result from a stored object. 
## If there isn't a stored inverted version, it will invert the matrix and store the inverted matrix with the original matrix.
## cacheSolve uses another function, makeMatrix, to retrieve and store matrices.
## makeMatrix stores a matrix "x" and can store an associated matrix with "x".
## makeMatrix is a generic helper function which enable you to retrieve "x" and its associated matrix when working with "x"

## The application of Function makeMatrix does nothing but being able to store matrices in pairs.
## setMatrix and getMatrix are 'methods' that store or retrieve the matrix value that you pass through X in the function call
## setCache and getCache are 'methods' that store another matrix value that you pass through calling the functions. 
## setCache and getCache values are associated with the matrix passed through X in the function call.
## The only thing it does is storing two matrices together in the same environment, so they can be stored and retrieved together.
## Thing of makeMatrix as a generic filing function to store and retrieve pairs of matrices.

makeMatrix <- function(x = matrix()) {
    m <- NULL
    setMatrix <- function(y) {
        x <<- y
        m <<- NULL
    }
    getMatrix <- function() x
    setCache <- function(var) m <<- var
    getCache <- function() m
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setCache = setCache,
         getCache = getCache)
}


## cacheSolve inverts a matrix
## cacheSolve first checks if the inverse has been stored before for a given matrix that has been stored using the makeMatrix function
## if there is no cache for a given matrix x, it will inverse the matrix and store the inverse using the makeMatrix function

cacheSolve <- function(x, ...) {
    ## First, retrieve the associated matrix of the input matrix x from the cached value using makeMatrix
    inverse <- x$getCache()
    ## Check if there is an associated matrix returned. If so, retrieve that matrix and return it as output
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    ## If there isn't an associated value cached already, retrieve the original matrix x.
    matrix <- x$getMatrix()
    ## Calculate its inverse
    inverse <- solve(matrix, ...)
    ## And store it as an associated value of matrix x.
    x$setCache(inverse)
    ## Return the inverse matrix as output
    inverse
}
