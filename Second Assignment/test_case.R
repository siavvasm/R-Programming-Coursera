## A test case for makeCacheMatrix 

x <- makeCacheMatrix(matrix(1:4,2,2))
x
x$get()
x$set(matrix(1:4,2,2,byrow = TRUE))
x$get()
x$getinverse()
y <- solve(x$get())
y
x$setinverse(y)
x$getinverse()
