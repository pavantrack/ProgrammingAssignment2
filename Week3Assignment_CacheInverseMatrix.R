## This assignment has been written here is in partial fulfillment of Coursera Data Science: R Programming 
## Week 3 Assignment; week starting August 22, 2016; 
## Submitted By: Pawnesh Soni

## This function will create a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {		 	## define the argument to hold default mode of "matrix"
    inv <- NULL                             		## initialize inv as NULL; this will hold value of matrix inverse 
    set <- function(y) {                    		## define the set function to assign new 
        x <<- y                             		
        inv <<- NULL                        		## if there is a new matrix, reset inv to NULL
    }
    get <- function() x                     		## define the get fucntion - returns value of the matrix argument
    
    setinverse <- function(inverse) inv <<- inverse  	## assigns value of inv in parent environment
    getinverse <- function() inv                     	## gets the value of inv where called
   
   list(set = set, 
		 get = get, 
		 setinverse = setinverse, 
		 getinverse = getinverse) 					    ## you need this in order to refer 
                                                        ## to the functions with the $ operator
}


## cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {                        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}														##End;
