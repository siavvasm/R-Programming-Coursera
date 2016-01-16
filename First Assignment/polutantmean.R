pollutantmean <- function(directory, pollutant = "sulfate", id=1:332){
  
  ## Loading the data sets into a data frame...
  for(i in 1:length(id)){
   
    # Printing the loading progress
    print(id[i])
    
    # Trick for loading all the different csv files into a single data frame
    if(id[i] < 10){
      tempdata <- read.csv(paste(directory,"/00",as.character(id[i]),".csv",sep = ""))
    }else if(id[i] <100){
      tempdata <- read.csv(paste(directory,"/0",as.character(id[i]),".csv",sep = ""))
    }else{
      tempdata <- read.csv(paste(directory,"/",as.character(id[i]),".csv",sep = ""))
    }
    
    # data frame initialization         
    if(i == 1){
      data <- tempdata
      z <- 1
    }else{
      data <- rbind(data,tempdata)
    }
  }
  
  # Extracting the desired column
  x <- data[,pollutant]
  
  # Manual Calculation of the column's mean
  index = is.na(x)
  y <- x[!index]
  mu <- sum(y)/length(y)

  # Using the build in function
  mean(x, na.rm = TRUE)
}

