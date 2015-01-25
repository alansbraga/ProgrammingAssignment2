## Unit responsible to create functions that allow users
## to compute the inverse of a given matrix only once.
## Next time the user call the function, it will return
## a cached result.
## How to use
## 1) Call makeCacheMatrix with your matrix
##   Ex.:  aaa <- makeCacheMatrix(matrix(c(2,2,3,5),nrow=2,ncol=2))
## 2) Call cacheSolve as many times you need passing the result of 
## makeCacheMatrix function
##   Ex.: inverted <- cacheSolve(aaa)

## function makeCacheMatrix
##    Function that returns a special list wich will control 
##    if the inverse of the matrix have already been made.
## paremeter
## x = The matrix to calculate the inverse.
## return
##    special list
##    if the user needs to know the original matrix
##    it needs to use the command return$get()
## Example
##    aaa <- makeCacheMatrix(matrix(c(2,2,3,5),nrow=2,ncol=2))
##    aaa$get()
makeCacheMatrix <- function(x = matrix()) {
    inverseCached <- NULL
    set <- function(newMatrix) {
        x <<- newMatrix
        inverseCached <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inverseCached <<- inverse
    getInverse <- function() inverseCached
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## function cacheSolve
##    Function responsilble to calculate the inverse of the matrix inside
##    the special list passed as parameter.
## paremeter
## x = The special list created by makeCacheMatrix
## ... = aditional parameters to pass to solve function
## return
##    the inversed matrix
cacheSolve <- function(x, ...) {
    inverseCached <- x$getInverse()
    if (!is.null(inverseCached)) {
        message("using available cache")
        return(inverseCached)
    }
    originalData <- x$get()
    message("Computing, this can take awhile...")
    inverse <- solve(originalData, ...)
    x$setInverse(inverse)
    inverse
}
