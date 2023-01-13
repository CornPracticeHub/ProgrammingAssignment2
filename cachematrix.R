## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL              # create inverse variable
        set <- function(y) {     # create function to set  
                x <<- y          # pass value of y to x outside of function
                inv <<- NULL     # pass NULL value to inv outside of function
        }
        get <- function() x      # create function to get x variable
        setInverse <- function(inverse) inv <<- inverse  #
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        getInverse <- function() inv
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()      # set inv var to inverse of x
        if (!is.null(inv)) {       # checks to see if inv value is NULL or not
                message("getting cached data")  # output message 
                return(inv)        #if inv var is not NULL, returns inv
        }
        data <- x$get()            #
        inv <- solve(data, ...)    #
        x$setInverse(inv)          #
        inv                        # 
}