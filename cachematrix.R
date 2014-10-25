#I'm not super clear on what the instructors want, I get the concept of Lexical scoping
#but I'm not sure how or why they want the data modified.
#I've basically set this up based on their examples, just pulling out variables and shunting in new
#values for them. Also, I didn't put in a failsafe for non-matrices because it doesn't seem the 
#instructors wanted us to do that. Either way, this is probably not right, so if you have any
#pointers feel free to email me at dccd202 (at) gmail (dot) com.  

## This pulls/creates the initial cache value


makeCacheMatrix <- function(x = matrix()) { # ingesting first matrix
  get <- x  #plugging in the first value for x since
  set <- (solve(x)) #finding the inverse of the original input
  x <-list(get = get,getInverse = set) #creating a list of the original matrix and its inverse
} 

## This pulls the cached value or recalculates it if null.

cacheSolve <- function(x,...) { 
  m <-x$getInverse # pulls the x value
  if(!is.null(m)) { message("Getting Cached Matrix") 
                    return(m)} #pulls the old value if its not null
  else {data <- x$get #if its null
  m<- (solve(data)) # computes the new value
  m} #returns it 
} 
