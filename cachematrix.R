# The programming assignment No. 2 'Caching the Inverse of a Matrix' as part of
# the Johns Hopkins University R Programming on Coursera. Two functions are 
# contained within this file. The aim of the functions are to (1) create a 
# special matrix which allows caching of the inversion of a matrix and (2) 
# determination of inverse matrix.
# File contains the following:
#   Function: makeCacheMatrix
#   Function: cacheSolve

# Date: 06/06/2022 by Ian Partridge


## makeCacheMatrix function creates a special "matrix" object (if square) and 
# caches it's inverse if present. Returns a list
# of objects held within the the special matrix.
makeCacheMatrix <- function(baseMatrix = matrix()) {
  # arg: baseMatrix is numeric and is the function environment matrix input 
  # All matrices should be a square e.g, 2x2, 3x3
  
  # Check input base matrix is square, if not return with error message
  # and NULL value
  if (nrow(baseMatrix) != ncol(baseMatrix)) {
    # not square
    message("Matrix not square")
    return(NULL)
  }
  
  # Assign initial inverse matrix as NULL 
  invMatrix <- NULL 
  
  #Assign cache of base matrix used for inverse matrix as NULL 
  cacheBaseMatrix <- NULL
  
  # Function to set the base matrix
  set <- function(newMatrixInput) {
    # set the function environment arg baseMatrix to input matrix value
    # arg: newMatrixInput function input, again should be a square matrix
    
    # Check input is a square matrix
    if (nrow(newMatrixInput) == ncol(newMatrixInput)) {
      # if square assign
      baseMatrix <<- newMatrixInput
      # set the makeCacheMatrix function environment arg invMatrix to null
      invMatrix <<- NULL 
    }else{
      # if not square give error message
      message("Matrix not square")
    }
  }
  
  #Function to get the base matrix
  get <- function() baseMatrix 
  
  # Function to set the cache of the inverse matrix
  setInverse <- function(caclInvMatrix) {
    # sets the cached inverse of the base matrix
    invMatrix <<- caclInvMatrix
  }
  
  # Function to return the cached inverse matrix
  getInverse <- function() invMatrix 
  
  # Function that caches the matrix that was used for the cached 
  # inverse matrix. This for comparison when required
  setBase <- function(usedBaseMatrix) {
    cacheBaseMatrix <<- usedBaseMatrix
  }
  # Function to return the cached base matrix used for cached inverse matrix
  getBase <- function() cacheBaseMatrix
  
  #List of named functions/methods fully formed objects
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse,
       setBase = setBase,
       getBase = getBase)
}


## cacheSolve function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix function. If the inverse has already been calculated & the 
# matrix has not changed, then the cachesolve retrieves the inverse 
# from the cache.
cacheSolve <- function(specialMatrix, ...) {
  # arg: specialMatrix is a special object created by makeCachMatrix
     
   invMatrix<- specialMatrix$getInverse() # Assign inverse matrix (will be NULL
   # if does not exist else will be the cached value)
   
   # Boolean flag for whether the matrix ($set) in the special Matrix object and
   # the cached matrix ($getBase) used for the inverse matrix are identical 
   # TRUE if identical.
   identicalFlag <- identical(specialMatrix$get(), specialMatrix$getBase())
   
   # check if inverse already cached and the base matrix has not changed
   # if this is TRUE then use the cached inverse matrix.
   if ((!is.null(invMatrix)) & identicalFlag) {
     message("getting cached matrix")
     return(invMatrix)
   }
   
   # Determine inverse matrix
   # check if square matrix
   if (nrow(specialMatrix$get()) == ncol(specialMatrix$get())) {
     # square matrix so determine the square matrix
     data <- specialMatrix$get()  # assign matrix to data
     invMatrix <- solve(data)  # determine inverse matrix and assign
     specialMatrix$setInverse(invMatrix) # cache inverse matrix
     # cache the matrix used to determine the inverse matrix
     specialMatrix$setBase(data)  
     invMatrix # return matrix 
   }else{
     message("Not a square matrix, please renter")
   }
}
