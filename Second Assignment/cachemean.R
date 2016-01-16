cachemean <- function(x, ...){
  # Retrieve the mean of vector x
  m <- x$getmean()
  
  # Check if its value is already computed
  if(!is.null(m)){
    print("getting the cached data")
    return(m)
  }
  
  # If it is not computed, compute it now!
  data <- x$get()
  m <- mean(data)
  x$setmean(m)
  m
}