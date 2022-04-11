

v <- dir ("coordinates correction/csv.new")

trial_name <- character()
total.dist <- numeric()
fiftheen.dist <- numeric()
proportion <- numeric()



for (j in 1:length(v)) {
  
  trial.name <-  unlist(strsplit(v[j], ".csv"))
  
  dat <- read.csv(file = paste("coordinates correction/csv.new/", trial.name, ".csv", sep = ""))
  
  
  min.y <- min(dat$y.center)
  y15 <- min(dat$y.center)+15
  
  if(min.y > -20){
    
    trial_name <- c(trial_name, trial.name)
    
    #calculate total distance traveled
    
    b1 <- sum(c(0,sqrt((diff(dat$new.x))^2+ (diff(dat$y.center))^2)))
    
    total.dist <- c(total.dist ,b1)   
    
    b3 <- 0 #(no distance moved in the lower 15  strip)
    
    fiftheen.dist <- c(fiftheen.dist, b3)
    
    proportion <- c(proportion ,(b3/b1))
    
    
  }else{
  
  trial_name <- c(trial_name, trial.name)
  
  #calculate total distance traveled
  b1 <- sum(c(0,sqrt((diff(dat$new.x))^2+ (diff(dat$y.center))^2)))
  total.dist <- c(total.dist ,b1)     
  
  
  #calculate distance traveled only in the lower 15 cm 
    a <- which(dat$y.center < y15)

    distance <- numeric()
    for (i in 1:length(a)) {

      d <- sqrt(((dat$new.x[a[i]]-dat$new.x[a[i]+1])^2)+((dat$y.center[a[i]]-dat$y.center[a[i]+1])^2)) #calculate distance
      distance <- c(distance, d)
      
      
  }#i
  
    if(is.na(distance[length(distance)]) == TRUE){
      distance <- distance[-c(length(distance))]
    }#if
  
  b2 <- sum(distance)
  fiftheen.dist <- c(fiftheen.dist ,b2)
  
  proportion <- c(proportion ,(b2/b1))
  
  
  }#ifelse-next
  
}#j


prop.dist <- data.frame(trial_name, total.dist, fiftheen.dist, proportion)

#save as csv
dir.create("distance traveled")
write.csv(prop.dist, file = paste("distance traveled/", "distance traveled15", ".csv", sep=""), row.names = F)



#plot(dat$new.x, dat$y.center)


