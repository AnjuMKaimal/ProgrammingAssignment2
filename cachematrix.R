## #makeCacheMatrix function makes a list of of methods with get and set values and inversing matrix 

makeCacheMatrix <- function(x = matrix()) {
     inVse1 <- NULL #intialises the inverse


        set <- function(userMatrix) 
        {
                x <<- userMatrix
                inVse1 <<- NULL
        }
        
        get <- function() x 

        setINverse <- function(inVerse) 
         {
           inVse1 <<- inVerse
           return(inVse1)
          }
        getINverse <- function() inVse1 
        list(set = set, get = get,
             setINverse= setINverse,
             getINverse = getINverse )


}


##cacheSolve tries to find the the inverse, if its written in first function otherwise it calculates inverse itself and dispays the solution


cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
         inv <- x$getINverse ()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setINverse(inv)
        inv
}
#Sample Output
#############################################
#> my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
#> my_matrix$get()
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4
#> my_matrix$getINverse()
#NULL
#> cacheSolve(my_matrix)
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> cacheSolve(my_matrix)
#getting cached data
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
################################################