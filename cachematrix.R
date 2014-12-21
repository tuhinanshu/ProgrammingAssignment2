## In the below functions,makeCacheMatrix is used to create a special vector which 
## makes a list of functions to set the value of the matrix,get the value to matrix,
## get the inverse of the matrix,set the inverse of the matrix and the function cacheSolve 
## is used to calculate the inverse of the matrix for the first time and cache the result so that henceforth
## the inverse will not be calcuated and will be fetched from the cache.

## The function makeCacheMatrix is used to make a special matrix which gives us a list of 
## functions to set the value of the matrix,get the value to matrix,get the inverse of the matrix,set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
           i <- NULL                    # inverse which is reset to null everytime it is called 

	 set <- function(y){	 	# function that is used to set the matrix and the inverse
		x <<- y
		i <<- NULL
	}
	
	get <- function() x             # function that is used to get the value of the matrix
	setinverse <- function(inverse) i <<- inverse   # function that is used to set the value of the inverse
	getinverse <- function() i			# function that is used to get the inverse of the matrix that is saved
	list(set = set,get = get,setinverse = setinverse,getinverse = getinverse)  # returns the list of functions

}


## The function cacheSolve is used to calculate the inverse of the matrix for the first time and cache the result so that henceforth
## the inverse will not be calcuated and will be fetched from the cache.

cacheSolve <- function(x, ...) {
          i <- x$getinverse()       # to retrieve the value of inverse if it is saved in the cache
          if(!is.null(i)){           
		message("getting cached data")
		return(i)           # return the cached result
	   }
	  data <- x$get()           # fetch the matrix via get function
	  i <- solve(data,...)      # calculate the inverse 
	  x$setinverse(i)           # set the inverse via the setinverse function
	  i
}
