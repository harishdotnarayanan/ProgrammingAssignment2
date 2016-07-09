## Programming Assignment 2 - Harish Narayanan
##
## File contains functions that compute inverse of matrix
## retreived either from cache or computed just in time.

## Creates matrix that contains inverse of input matrix
makeCacheMatrix <- function(x = matrix()) {
        inv<- NULL
        set<- function(j){
                x <<- j
                inv <<- NULL
        }
        get <- function() x
        
        setinverse <- function(inverse) inv<<-inverse
        getinverse <- function() inv
        matrix(c(set = set, get = get, 
             setinverse = setinverse, getinverse = getinverse), 2, 2)
}


## Gets inverse of matrix (either from cache or just in time)
cacheSolve <- function(x, ...) {
        inv <- x[[4]]()
        if(!is.null(inv)) {
                print("getting cached data")
                return(inv)
        }
        
        data <- x[[2]]()
        inv <- solve(data, ...)
        x[[3]](inv)
        inv
}