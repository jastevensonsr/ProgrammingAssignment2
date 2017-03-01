## These nested functions allow a user to find the inverse of a square,
## invertible matrix and cache the results.  The benefit of cacheing the results 
## is it can be called up instead of regenerated in future function calls.

## This function sets the parent and global environments for these functions.  
## When calling the function, it creates an object that contains four functions 
## (set(), get(), setinv() and getinv()) and two data objects x (the matrix) and 
## n (the inverse).

## This is the parent environment

	makeCacheMatrix <- function(x = matrix()) 	{
        	n <- NULL

#This is the global environment

 	       set <- function(y) {
        	        x <<- y
                	n <<- NULL
			          }
	        get <- function() x
        	setinv <- function(solve) n <<- solve
	        getinv <- function() n
 
## The list assigns each of the four functions as an element within a list() and
## returns it to the parent environment.  This provides the required input arguments 
## need for the cacheSolve() function. Naming the list elements allows us to use the
## $ extract operator instead of [[ ]] in cacheSolve(). 

	       list(set = set, get = get,
        		setinv = setinv,
            		getinv = getinv)
							}

## makeCacheMatrix() is incomplete without the cacheSolve() function.  This function
## is required to populate or retrieve the inverse from an object of type makeCacheMatrix().
## Note: Solve() return an inverse of a matrix.

	cacheSolve <- function(x, ...) {

## If there is already an inverse in the parent environment, a message will appear.  
## Otherwise, an inverse will be generated, stored in the parent environment and printed.

	        n <- x$getinv()
        	if(!is.null(n)) {
                	message("getting cached data")
	                return(n)
        			}
 	       	data <- x$get()
		n <- solve(data, ...)
        	x$setinv(n)
        	n
					}
## Use the following code as a baseline to perform these function calls.
## >m1 <- matrix(c(1/2, -1,4, -1, 3/4),2,2)
## >myMatrix_object <- makeCacheMatrix(m1)
## >cacheSolve(myMatrix_object)
## Running the previous line again, will retrieve the cached inverse
