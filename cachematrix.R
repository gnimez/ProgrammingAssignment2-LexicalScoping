## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { # Create function where argument has default mode matrix
  inv <- NULL # Create variable inv as NULL, will use to store inverse matrix
  set <- function(y){ # Create a new function that assigns new value of matrix in parent environment
    x <<- y 
    inv <<- NULL #If there is a new matrix, reset inv to NULL
  }
  get <- function() x #Define a function get that returns matrix argument
  setinv <- function(inverse) inv <<- inverse #Create function that assigns value of inv in parent environment
  getinv <- function() inv #Create function that gets value of inv when called
  list(set = set, get = get, setinv = setinv, getinv = getinv) # Create a list to refer to function with $ operator

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) { #Create a function that computes the inverse of matrix returned by makeCacheMatrix above
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  if(!is.null(inv)){ # If the inverse has already been calculated(i.e matrix has not changed), cacheSolve will retrive matrix from cache
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
      
}
