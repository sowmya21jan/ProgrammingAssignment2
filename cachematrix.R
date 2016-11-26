#	The cachematrix.R file contains two functions, makeCacheMatrix() and 
#	cacheSolve(). The first function in the file, makeCacheMatrix() 
#	creates an R object that stores a matrix and its inverse. 
#	The second function, cacheSolve() requires an argument that is 
#	returned by makeCacheMatrix() in order to retrieve the Inverse from the 
#	cached value that is stored in the makeCacheMatrix() object's environment


# makeCacheMatrix(): This function creates a special "matrix" object that can 
# cache its inverse.

# cacheSolve(): This function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then the cacheSolve should retrieve the inverse from the 
# cache.

# The inverse of a square matrix is computed using solve() function in R. 


## makeCacheMatrix()
#	- class of makeCacheMatrix() - a function 
#	- formal Arguments - x (should be a matrix)
#	- Creates six objects x, m, set, get, setInverse, getInverse
#	- Output - a list with four objects - set, get, setInverse, getInverse


makeCacheMatrix <- function(x=matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve()
#	- class of cacheSolve() - a function 
#	- formal Arguments - x (should be a matrix) and ...
#	  (In technical language, ... is called an ellipsis. And it 
#	   means that the function is designed to take any number of 
#       named or unnamed arguments.)
#	- Output - It throws inverse found from the cache data for the given matrix
#			If inverse not found in the cache data, it is 
#			computed for the given matrix and printed as output 


cacheSolve <- function(x = matrix(), ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}


## Running functions

# First Way
a <- makeCacheMatrix(matrix(11:14,2,2)) 
cacheSolve(a)	# This will compute the inverse as it did not find inverse in cache
cacheSolve(a)   # This will give the inverse as output found in cache by previosu command

# Second Way
a <- makeCacheMatrix() 
a$set(matrix(11:14,2,2))  
cacheSolve(a)
cacheSolve(a)


