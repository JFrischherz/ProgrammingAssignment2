## The script contains two (2) functions:
## 1. makeCacheMatrix - which creates a matrix and caches its inverse
## 2. cacheSolve - which computes the inverse of the matrix created in makeCacheMatrix,
##                 and sources the inverse from the cache if it has already been calculated 
##	        		 (provided the matrix has not changed).



## The function creates a user defined matrix and saves the corresponding inverse in the cache.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL   ##sets the inverse to NULL (locally)
set <- function(y) {        ##defines the set function to modify the existing matrix
	x <<- y
	inv <<- NULL  ##sets the inverse to Null (globally)
}
get <- function() x       ## defines the get function to return values
setinv <- function(solve)  inv<<- solve  ##applies the solve function to calculate the inverse
getinv <- function() inv                 ##defines the getinv command to return the inverse
list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)

}


## The function checks whether the inverse of the selected matrix already exists in the cache, if so, it returns the inverse, if not it calculates it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()        ##source the inverse by calling the cache from the makeCacheMatrix function
  if(!is.null(inv)){    
      message("getting cached data")    ## if the resulting value is not null the comment "getting cached data" is returned to indicate that the inverse is already in the cache
      return (inv)         ##then the inverse is returned
                            
  }
  data <- x$get()        ##if the inverse was not in the cache get calculates the inverse and returns it.
  inv <- solve(data,...)
  x$setinv(inv)
  inv
  
}