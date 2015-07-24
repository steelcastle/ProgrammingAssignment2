## This special function definition is a 'contructor' function. 
## Its purpose is to define a group of new functions and return a list 
## of these named objects - specifically "set", "get", "setmatinv" and 
## "getmatinv". The makeCacheMatrix environment acts as the
## enclosing environment for these new functions (where these 
## functions are defined).
## 
## Within this enclosing environment, we have defined these variables:
## "x" - which holds the initial matrix specified as the argument when
## makeCacheMatrix is called
## "mat_inv" - which will hold the inverse of the matrix specified
## in "x" (this value will be updated by the fcn "setmatinv").
## Note: the fcn. "set" can also be used to establish a new matrix
## value if desired - in this case it will clear out the matrix 
## inverse location so that a new inverse caculation will be performed.
## 
## Since this enclosing environment (makeCacheMatrix) contains 
## these 2 variables, they can be accessed by the functions
## enclosed by this environment and therefore can act as
## caches for values that we wish to maintain.
##
## The double arrow assignment operator "<<-" provides this capability.
## 
##

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize value of matrix inverse location
        mat_inv <- NULL
        ## Assign new value for matrix if desired.
        ## This is stored back into "x" and the matrix inverse
        ## is nullified (cache is cleared).
        set <- function(y) {
                x <<- y
                mat_inv <<- NULL
        }
        ## Retrieve initial matrix value
        get <- function() x
        ## Store matrix inverse value into cached location "mat_inv"
        setmatinv <- function(solve) mat_inv <<- solve
        ## Retrieve cached value of matrix inverse
        getmatinv <- function() mat_inv
        ## Return list of named functions
        list(set = set, get = get,
             setmatinv = setmatinv,
             getmatinv = getmatinv)
}


## cacheSolve: This function takes the name of a list as its main 
## argument and stores it in "x".
## It subsets this list to select the desired functions that are
## defined by makeCacheMatrix. Initially it checks to see if an
## inverse already exists (using the "getmatinv" function) and tests
## the return value (which was obtained from the cached location).
## If the cache contains a value, it will return this value and
## report that the cached data was used.
##
## If the cache contains no value, then the function "get" is
## called to access the original matrix value, the inverse is then
## calculated and this is saved back into the cache location (using 
## "setmatinv")


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat_inv <- x$getmatinv()
        ## Check if cache of the matrix inverse contains a value
        ## If so, then return this value and report that the cached
        ## data was used.
        if(!is.null(mat_inv)) {
                message("getting cached data")
                return(mat_inv)
        }
        ## Retrieve the original matrix value
        matrix <- x$get()
        ## Caclculate its inverse
        mat_inv <- solve(matrix, ...)
        ## Store this inverse into the cache location
        x$setmatinv(mat_inv)
        ## Return the matrix inverse to the calling environment
        mat_inv
}
