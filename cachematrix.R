## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse_R <- NULL ##declaring variable without assignment
        set <- function(y) { ##declaring "set" function with argument y
                x <<- y      ## function sets the value of the matrix
                inverse_R <<- NULL 
        }
        
        get <- function() x ##declares value to get the value of the matrix
        setInverse <- function(inverse) inverse_R <<- inverse ##declaring function to set the inverse
        getInverse <- function() inverse_R ##declares function to retreive the inverse
        list(set = set, get = get, ##list that contains the 4 functions described above
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_R <- x$getinverse() #retrieving variable
        if(!is.null(inverse_R)) { ##checking to see if the inverse already exists
           return inverse_R ##if it does, then we will just return the inverse from the cache
        }
        
        data <- x$get() ##retreive data
        inverse_R <- solve(data, ...) ##here we are actually calculating the inverse
        x$setInverse(inverse_R) ##assigning the inverse to a variable
        inverse_R ##this will display the variable
}
