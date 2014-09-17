## Below is a pair of functions that cache the inverse of a matrix.
## Computation of the inverse of a square matrix is accomplished 
## with the "Solve" function in R.

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL    ##inversed square matrix
    
    ## Set square matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## Get square matrix 
    get <- function() {
        x
    } 
    
    ## Set inversed square matrix
    setmatrix <- function(matrix) {
        m <<- matrix
    } 
        
    ## Get inversed square matrix
    getmatrix <- function() {
        m
    }
    
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
    
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been computed (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    ## Get inversed square matrix
    m <- x$getmatrix()
    
    ## Check whether inversed square matrix i.e. m is NULL.
    ## If m!=NULL, return inversed square matrix from cache 
    if(!is.null(m)) {
        message("getting cached inversed matrix")
        return(m)
    }
    
    ## Inversed square matrix is NULL.
    ## Get square matrix from cache.
    data <- x$get()
    
    ## Compute inversed square matrix using "Solve" function
    m <- solve(data, ...)
    
    ## Set inversed square matrix in cache 
    x$setmatrix(m)
    
    ## Return a matrix that is the inverse of 'x'
    m
}
