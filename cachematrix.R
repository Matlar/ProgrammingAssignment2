## My functions takes an invertible matrix as input and returns the inverse
## by checking in the cache if it has been calculated before, otherwise
## it is calculated using the solve() function

## Function that creates a list containing both the input matrix as well
## as functions used to get and set the matrix and its inverse (caching it)

makeCacheMatrix <- function(x = matrix()) {
    m_inverse <- NULL
    set <- function(y){
        x <<- y
        m_inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m_inverse <<- inverse
    getInverse <- function() m_inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Function that takes a makeCacheMatrix object as input and returns the matrix
## inverse either by looking in the cache or by using the solve() function and 
## then storing it in the cache

cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of 'x'
    m_inverse <- x$getInverse()
    if(!is.null(m_inverse)){
        message("getting cached matrix inverse")
        return(m_inverse)
    }
    m_data <- x$get()
    m_inverse <- solve(m_data, ...)
    x$setInverse(m_inverse)
    m_inverse
}
