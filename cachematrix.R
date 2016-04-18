## The code is written by Rahul Shri Shri ShriMal
## The code is two simple functions makeCacheMatrix and cacheSolve which will help to calculate the inverse of a invertable matrix
## Created on : 18th April 2016 2045 IST


## The function gets the value of the matrix and sets to a new matrix and then upon calling it validtes the inverse and returns the results

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
		## this function assigns the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
		## this function will fetch the matrix which called upon
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## 
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m ## this is the inverse of the matrix provided
}
    ## Return a matrix that is the inverse of 'x'
