

a <- dir("coordinates correction/csv.all")


for (j in 1:length(a)) {
  
  
  
  v <- dir(paste("coordinates correction/csv.avg/", a[j], sep="")) 
  
  
  all.x.min <- numeric()
  all.x.max <- numeric()
  
  all.y.min <- numeric()
  all.y.max <- numeric()
  
  
  for (i in 1:length(v)) {
  
    
  
    trial.name <-  unlist(strsplit(v[i], ".csv"))
    
    dat <- read.csv(file = paste("coordinates correction/csv.avg/",a[j], "/", trial.name, ".csv", sep = ""))
    
    
    #find x min and x max
    x.min <- min(dat$X.center)
    x.max <- max(dat$X.center)
    
    #find y min and y max
    y.min <- min(dat$Y.center)
    y.max <- max(dat$Y.center)
    
    
    
 
    all.x.min <- c(all.x.min, x.min)
    all.x.max <- c(all.x.max, x.max)
    
    all.y.min <- c(all.y.min, y.min)
    all.y.max <- c(all.y.max, y.max)
    
    
       
    
  }#i
  
  #calculate avg points
  avg.min.x <- mean(all.x.min)
  avg.max.x <- mean(all.x.max)
  
  avg.min.y <- mean(all.y.min)
  avg.max.y <- mean(all.y.max)
  
  #calculate length coordinates
  x.length <- (avg.min.x*-1)+avg.max.x
  y.length <- (avg.min.y*-1)+avg.max.y
  
  
  #coordinate correction with avg points found above
  
  b <- dir(paste("coordinates correction/csv.all/", a[j], sep="")) 
  
  for (n in 1:length(b)) {
    
    trial.name <-  unlist(strsplit(b[n], ".csv"))
    
    dat.1 <- read.csv(file = paste("coordinates correction/csv.all/",a[j], "/", trial.name, ".csv", sep = ""))
    
    new.x <- dat.1$X.center/x.length*y.length
    #new.y <- dat.1$Y.center/y.length*x.length
    y.center <- dat.1$Y.center
    #create a data frame from x.new and y and save it as csv file
    
    df <- data.frame(new.x, y.center)
    
    #dir.create("coordinates correction/csv.new")
    write.csv(df, file = paste("coordinates correction/csv.new/",trial.name, ".csv",  sep=""), row.names = F)
    
   
    
    
  }#n
  
  
  
  
  
 
  
}#j

