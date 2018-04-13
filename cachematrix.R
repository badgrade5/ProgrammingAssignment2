## Put comments here that give an overall description of what your
## functions do

## This function generates a list that can be called upon to recover stored variables

makeCacheMatrix <- function(x = matrix()) {
        #Initiate an empty entry for the i variable (inverse matrix), when a new cache 
        #matrix object is generated
        i <- NULL
        #Reassign the value of the x variable based on the argument (y) to the 
        #'set' function
        #clear the i variable if data is changed via the set function
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        #return the value of the x variable as passed when making the 'CacheMatrix' object
        #or when re-set using the $set function of the object
        get <- function() x
        #reassign a new value to i via the $setInverse
        setInverse <- function(inv) i <<- inv
        #return the inverse of a matrix object defined by makeCacheMatrix using the 
        #output of the 'cacheSolve' function. 
        getInverse <- function() i
        #Index the list with '$' callable descriptors
        list(set = set, get = get, setInverse = setInverse,
             getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        #if the i variable is not NA, then print some text and return the matrix stored there
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        #if there is an NA for the i variable, then assign a 'data' variable to output from 
        #makeCacheMatrix$get call
        data <- x$get()
        #assign inverse of matrix to variable i
        i <- solve(data, ...)
        #Call setInverse function from makeCacheMatrix and reassign variable i stored for that
        #object
        x$setInverse(i)
        i
}
