makeCacheMatrix <- function(matr = matrix()) {
        invert <- NULL
        setmatr <- function(y) {
                matr <<- y
                invert <<- NULL
        }
        getmatr <- function() matr
        setinvert <- function(inversion) invert <<-inversion
        getinvert <- function() invert
        list(setmatr = setmatr, getmatr = getmatr,
             setinvert = setinvert,
             getinvert = getinvert)
}

cacheSolve <- function(matr, ...) {
        invert <- matr$getinvert()
        if(!is.null(invert)) {
                message("getting cached data")
                return(invert)
        }
        data <- matr$getmatr()
        invert <- solve(data, ...)
        matr$setinvert(invert)
        invert
}

# 1 - Declares a makeCacheMatrix function with the 1 argument matr, 
## which set as a matrix
# 2 - Sets variable invert as NULL
# 3 - Declares a nested function setmatr inside the makeCacheMatrix function, 
## which takes 1 argument y
# 4 - This argument y = argument matr of the makeCacheMatrix function, 
## being stored in the parental environment
# 5 - Sets variable invert as NULL in the parental environment
# 7 - Declares a function getmatr, which returns matr variable
# 8 - Declares a function setinvert, which takes 1 argument inversion, 
## which stores the argument provided into the variable invert 
## in the parental environment, replacing existing NULL
# 9 - Declares a function getinvert, which returns invert variable
# 10 - Sets and returns a list with a names similar to the functions provided
## There is an end of the function makeCacheMatrix, which returns 
## the aforementioned list and should be placed into a variable

# 15 - Declares a function cacheSolve, which takes the matr variable as argument
# 16 - Places in the variable invert results of application 
## of the getinvert function (??)
# 17-19 - Returns the same results from cache with an additional message,
## if the variable invert non-equal to NULL
# 21 - Places the results of application of the getmatr function in a variable data 
# 22 - Applies built-in function solve to the aforementioned results
## and stores into the variable invert
# 23 - Sets invert into matr (??)
# 24 - Prints inverted matrix