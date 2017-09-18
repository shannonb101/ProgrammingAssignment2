## A pair of functions that when executed will cache the inverse of an invertible matrix

## Creates a list of variables including an invertible matrix and its inverse.  
## The inverse is solved in the cacheSolve function and returned to the list
makeCacheMatrix <- function(x = matrix()){
        m <- NULL  # Set m to NULL inside this function
        set <- function(y){  # Assign the input matrix (from outside this environment) to x
                x <<- y  # Look outside the function to set the value of x
                m <<- NULL  # Whatever m was before (outside this function) set it to NULL when set is run
        }
        get <- function() x  # Set "get" to get the input matrix
        setinverse <- function(inverse) m <<- inverse  # Set "setinverse" to a function returning to m a dummy variable, which will actually cache the inverse from cacheSolve
        getinverse <- function() m  # Set "getinverse" to m. If cacheSolve has not been run, it will be empty and the mean will need to be computed. If it has, then it will tell cacheSolve
        list(set = set, get = get,  # Make a list of all the public methods
             setinverse = setinverse, 
             getinverse = getinverse)
}


## Calculates inverse of matrix input from makeCacheMatrix function.
## If inverse has not already been calculated, calculates inverse and returns it to a variable in a list in makeCacheMatrix.
## If inverse has already been calculated, retrives that calculation and prints it.
cacheSolve <- function(x) {
        m <- x$getinverse()  # See if the inverse has already been computed and cached
        if(!is.null(m)) {  # If it hasn't
                message("getting cached data")  # Print this message and solve for the inverse
                return(m)
        }
        data <- x$get()  # use the matrix from the $get function
        m <- solve(data)  # Solve for the inverse
        x$setinverse(m)  # And set that value in the $setinv function so that 
        m  # Return the inverse to this environment, so if someone runs it again they can see that the inverse has been computed and cached
}
