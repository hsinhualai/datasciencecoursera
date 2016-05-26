## the top function gives a list, which contains a function to
## 1. set the matrix
## 2. get the matrix
## 3. set the matrix inverse
## 4. get the matrix inverse 

makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setsolve <- function(solve) m <<- solve
                getsolve <- function() m
                list(set = set, get = get,
                     setsolve = setsolve,
                     getsolve = getsolve)
        
}


## The below function use the solve() to get the matrix inverse. The first line in the function 
## use the getsolve() defined in the makeCacheMatrix function to assign a matrix to m. The second
## line tells that if the assigned matrix m is not NULL, then it gives the message and return 
## a stored matrix m. If it is NULL, it assign a matrix to data and then use solve() function to
## obtain the matrix inverse of the matrix data and assign the result to m. Then it store the 
## the result using setsolve(m) and return the matrix inverse m.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                m <- x$getsolve()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get()
                m <- solve(data, ...)
                x$setsolve(m)
                m
}
