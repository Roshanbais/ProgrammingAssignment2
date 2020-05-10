##The makeCacheMatrix function is used here to get a matrix X. Here, m is used as variable for storing Inverse of matrix.
## functions do implementation, in accordance with the instructions. Hence, by using lexical scoping in R, we here are getting inverse of a matrix.

## This special object is a list of 4 functions:
## 1. set(y) -> Receives a matrix as a parameter and saves it inside the object.
## 2. get() -> Returns the matrix saved inside the object. 
## 3. setinverse(inverse) -> Receives a matrix and saves it inside the object (cache).
## 4. getinverse() -> Returns the inverse of the matrix if it's saved inside the object (cache).
makeCacheMatrix <- function(x = matrix()) 
 {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## The cacheSolve function returns the inverse of the special matrix object inside the makeCache function.
##
## If the inverse of the matrix is already stored in the object as a cache, cacheSolve() prints a message
## specifying it's getting cached data and quickly returns it.
##
## If, however, the inverse of the matrix isn't stored in the object, the function solve() is utilized to calculate
## the inverse of the matrix inside x, being promptly cached inside the object. cacheSolve() then returns the inverse
## of the matrix.


cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) 
        {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- inverse(data)
        x$setinverse(m)
        m
}
        ## Return a matrix that is the inverse of 'x'
