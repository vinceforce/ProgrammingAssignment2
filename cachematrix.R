## This function adds set and get accessors to seed or return value of internal variable x,
## and adds setinv and getinv methods to set and get the value of internal variable invx
## All methods are exposed through their internal name (instruction list(set = set, ......))
## Internal variables x and invx are intialized on the first call
## (x for it is an argument, invx by the first instruction of the function)

## This function constructs an object, given a matrix, for optimizing inverted matrix calculation
## If calculation has ever be done, getinv method returns the cached value in place of calculating again

makeCacheMatrix <- function(x = matrix()) {
        invx <<- NULL
        set <- function(y) {
                x <<- y
                invx <<- NULL
        }
        get <- function() x
        setinv <- function(minv) invx <<- minv
        getinv <- function() invx
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## This function returns the matrix given as argument, inverted
## It prevents calculating the inverted matrix again
## if cache has been populated by a previous call of the function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat <- x$get()
        inv <- x$getinv()
        if(!is.null(inv)) {  # cache is not empty 
                message("getting cached data for inverse matrix")
                return(inv)
        }
        else if (nrow(mat) != ncol(mat)) {  ## matrix is not square, therefore cannot be inverted
                warning("Non square matrix cannot be inversed")
        }
        else if (det(mat) == 0.0) {  ## matrix is square, but cannot be inverted (det=0)
                warning("this matrix is square but not invertible (det=0)")
        }
        else { ## matrix is invertible
                matinv <- solve(mat)  ## calculation of inverted matrix
                x$setinv(matinv)  ## put the inverted matrix in cache
                matinv  ## return the inverted matrix
        }
}
