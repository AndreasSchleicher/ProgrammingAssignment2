## Analogous to the vector-mean-example there are two functions:

## 1. "makeCacheMatrix", to initialize a matrix "x" (either from the argument or empty by default), 
## to initialize an empty inverse of this matrix ("inverse") and to create a list of sub-functions 
## on the matrix "x"; those sub-functions are "set"(to set/change the initial matrix),"get" (to be 
## able to return the matrix), "getinverse" (to return the inverse) and "setinverse" (to calculate 
## the inverse, if not done yet)
## 2. "cacheSolve", to return the inverse of the initialized matrix (either the one already calculated 
## and stored in the previous steps or calculate a new one)


## "makeCacheMatrix" as described above:

makeCacheMatrix <- function(x = matrix()) {
  
        inverse <- matrix()  
        set <- function(y) {
                x <<- y
                inverse <<- matrix()
        }
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## "cacheSolve" as described above:

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        ## The thrown warning message can be ignored
        
        if(!is.na(x$getinverse())) {
                message("getting cached data")
                return(x$getinverse())
        }  else{        
                data <- x$get()
                inv <- solve(data, ...)
                inverse <- x$setinverse(inv)
                return(inverse)
        }      
}
