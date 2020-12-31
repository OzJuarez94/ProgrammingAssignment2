## Overall, both functions make possible to calculate the inverse of a matrix
## by using vectors and operators that work on vectors. 
## The first one creates and stores the matrix as a "matrix"
## The second one calculates the inverse of that matrix.

## As explained, this first function turns the matrix into a "matrix".
## Which will make it possible to calculate the inverse with vector operators.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y 
        i <<- NULL
    }
    get <- function() x 
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This one solves the inverse of the "matrix".

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
