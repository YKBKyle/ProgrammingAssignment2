## Put comments here that give an overall description of what your
## functions do
    # Below are two functions that are used to create a special object 
    # that stores a numeric matrix and cache its inverse.

## Write a short comment describing this function
    # makeCacheMatrix creates a special "matrix",
    # which is really a list containing a function to
    # 1. get the value of the matrix
    # 2. set the value of the matrix
    # 3. get the inverse of the matrix
    # 4. set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    InverseMatrix <- NULL
    get <- function() x
    set <- function(y) {
        x <<- y
        InverseMatrix <<- NULL
    }
    getinverse <- function() InverseMatrix
    setinverse <- function(Inv) InverseMatrix <<- Inv
    list(get = get, set = set, getinverse = getinverse, setinverse = setinverse)
}


## Write a short comment describing this function
    # cacheSolve calculates the inverse of the "matrix" created with makeCacheMatrix.
    # However, it first checks to see if the inverse has already been calculated.
    # If so, it gets the inverse from the cache and skips the computation.
    # Otherwise, it calculates the inverse and 
    # sets the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    y <- x$getinverse()
    if(!is.null(y)) {
        return(y)
    }
    data <- x$get()
    InverseMatrix <- solve(data)
    x$setinverse(InverseMatrix)
    InverseMatrix
}
