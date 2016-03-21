##########################################################################
##################        cachematrix.R       ############################
##########################################################################
##                                                                      ##
## AUTHOR: Carlos Conde                                                 ##
## DATE: 03-20-2016                                                     ##
## DESCRIPTION:                                                         ##
##    cachematrix.R enables the caching of the Inverse of a given Matrix##
##    to avoid future re-calculations when the Matrix has not changed   ##
##                                                                      ##
##########################################################################
##########################################################################


##########################################################################
## NAME: makeCacheMatrix()
## DESCRIPTION: Adds additional functionality to a Matrix to enable the
##    caching of its inverse
## INPUTS:
##    - x: The matrix to be converted into special Matrix
##########################################################################

makeCacheMatrix <- function(x = matrix()) {

        ## Initialize the variable that will be storing the Inverse
        matrix_inv <- NULL
    
        ## Add "set" functionality that enables storing a new Matrix
        set <- function(y) {
              x <<- y
              matrix_inv <<- NULL
        }
        ## Add "get" functionality that enables retrieving the Matrix
        get <- function() x
        ## Add "setinv" functionality that enables caching the Inverse
        setinv <- function(inv) matrix_inv <<- inv
        ## Add "getinv" functionality that enables retrieving the cached
        ##  Inverse
        getinv <- function() matrix_inv
  
        ## ADDITIONAL FUNCTIONALITY
        ##  Add "checkinv" functionality as an option to check if the 
        ##  matrix is inversible
        checkinv <- function(mat) {
              if (class(try(solve(mat),silent = T)) == "matrix") {
                    message("The Matrix is Inversible")
              } else {
                    message("The Matrix is NOT Inversible")
              }
        }
          
        list(set = set, get = get,
            setinv = setinv,
            getinv = getinv,
            checkinv = checkinv)
}


##########################################################################
## NAME: cacheSolve()
## DESCRIPTION: Calculates the inverse of the Special Matrix passed as
##    argument (if the inverse ha not been already calculated) and caches 
##    it inside the special matrix
## INPUTS:
##    - x: Special Matrix
## OUTPUTS:
##    - inv: The Inverse Matrix
##########################################################################

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        ## Check if the inverse has already been calculated before and 
        ## return the cached value if the cached value not NULL
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
    
        ## If the inverse of the matrix doesnt exist, calculate it
        # 1) Get the matrix
        data <- x$get()
        # 2) Calculate the inverse of matrix
        inv <- solve(data, ...)
        # 3) Cache the new inv value and print it out
        x$setinv(inv)
        inv
}


