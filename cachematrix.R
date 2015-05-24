## This is the the Rprograming assignment 2 including both functions
## functions do

## this first function creates a matrix and stores its  cache

makeCacheMatrix <- function(mat=matrix())
{
        inv <- NULL
        
        
         ##this function have 4 funtions to set, get the matrix, in addition to set
         ## and get the inverse of the matrix.
        
        set <- function(nmat) {
                mat <<- nmat
                inv <<- NULL
        }
        
        get <- function() mat
       
        setinv <- function(inverted) inv <<- inverted
        
        getinv <- function() inv
       
        ## list the values
        
        list(set = set, 
             get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## this functions calculates the inverse of the matrix if it has not yet been calculated
## if it was calculated it returns the cached data.


cacheSolve <- function(mat, ...)
{
        
        inv <- mat$getinv()
        
        ##verify if there is a cache of the inverse
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ##Calculate the cache and pass it to be stored.
        data <- mat$get()
        inv <- solve(data)
        mat$setinv(inv)
        inv
        
}
