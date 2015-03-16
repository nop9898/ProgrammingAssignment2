###########################################################
## Programming Assignment 2 - R Programming (Coursera)   ##
## nop9898@github                                        ##
###########################################################


## makeCacheMatrix(data_matrix)
##
## Creates a 'matrix' object that can cache the inverse matrix of data_matrix.
##
## Input: data_matrix : an invertable square matrix
makeCacheMatrix <- function(data_matrix = matrix()) {

    stored_inverse <- NULL
    
    # function 'iset' - caches the input matrix (and clears the cached inverse matrix)
    iset <- function(y) {
        data_matrix <<- y
        stored_inverse <- NULL
    }
    
    # function 'iget' - returns the input matrix
    iget <- function() { 
        return(data_matrix)
    }
    
    # function 'isetinverse' - caches the inverse matrix
    isetinverse <- function(inverse_matrix) {
        stored_inverse <<- inverse_matrix
    }

    # function 'igetinverse' - returns the cached inverse matrix
    igetinverse <- function() {
        return(stored_inverse)
    }
    
    # define the set- and get- functions
    list (
        set = iset, 
        get = iget,  
        getinverse = igetinverse,
        setinverse = isetinverse
    )
}

## cacheSolve(matrix_vector)
##
## Returns the inverse matrix of matrix_vector$get().
## > if a previously cached version exists for this matrix, it will return the cached inverse
## > if not, this function will calculate the inverse, cache it, and return it
##
## Input: a 'CacheMatrix' vector as produced by the makeCacheMatrix function
cacheSolve <- function(matrix_vector, ...) {
    
    # Attempt to get the cached inverse matrix
    cached_inverse <- matrix_vector$getinverse()
    
    if (!is.null(cached_inverse)) {
        # If the cached inverse matrix exists, return it.     
        return(cached_inverse)
    }
    else {
        # If the cached inverse matrix does not exist, calculate it.
        this_matrix = matrix_vector$get()
        this_inverse = solve(this_matrix, ...)
        
        # Cache the result and return it
        matrix_vector$setinverse(this_inverse)
        return(this_inverse)
    }    
}