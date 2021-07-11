makeCacheMatrix <- function(x = matrix()){
        dq1419 <- NULL
        set <- function(y){
                x <<- y
                dq1419 <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) (dq1419 <<<- inverse}
        getInverse <- function() (dq1419)
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...){
        dq1419 <- x$getInverse()
        if(!is.null(dq1419)){
                message("getting cache data")
                return(dq1419)
        }
        mat <- x$get()
        dq1419 <- solve(mat, ...)
        x$setInverse(dq1419)
        dq1419
}
