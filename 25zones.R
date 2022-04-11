
v <- dir ("coordinates correction/csv.new")


for (j in 1:length(v)) {
  
  trial.name <-  unlist(strsplit(v[j], ".csv"))
  
  dat <- read.csv(file = paste("coordinates correction/csv.new/", trial.name, ".csv", sep = ""))
  
  trial_list <- read.csv(file = paste("trial_list", ".csv", sep = ""))
  
  #vector that contain all the unusual trials (travel on half arena)
  
  trials.vector <- c("trial14", "trial17", "trial19", "trial31", "trial34", "trial35",
                     "trial36", "trial37", "trial38", "trial40", "trial41", "trial64",
                     "trial65", "trial66", "trial68", "trial69", "trial70", "trial73")
  
  trials.v <- which(trials.vector == trial.name)
  if(length(trials.v) > 0){
    
    min.x <- trial_list$x.min[trials.vector[trials.v] == trial_list$trials] 
    max.y <- trial_list$y.max[trials.vector[trials.v] == trial_list$trials]
    max.x <- trial_list$x.max[trials.vector[trials.v] == trial_list$trials]
    min.y <- trial_list$y.min[trials.vector[trials.v] == trial_list$trials]

    if(range(dat$new.x)[2] > max.x){max.x <- range(dat$new.x)[2]}
    if(range(dat$new.x)[1] < min.x){min.x <- range(dat$new.x)[1]}
    
    if(range(dat$y.center)[2] > max.y){max.y <- range(dat$y.center)[2]}
    if(range(dat$y.center)[1] < min.y){min.y <- range(dat$y.center)[1]}
    
    
    
  }else if(length(trials.v) == 0){
    
  min.x <- min(dat$new.x)
  max.y <- max(dat$y.center)
  max.x <- max(dat$new.x)
  min.y <- min(dat$y.center)
  
  }#else
  
  
  x.l <- (max.x-min.x)/5
  y.l <- (max.y-min.y)/5

 
  
  min.x1 <- min.x+ x.l
  min.x2 <- min.x+ (x.l*2)
  min.x3 <- min.x+ (x.l*3)
  min.x4 <- min.x+ (x.l*4)
  min.x5 <- min.x+ (x.l*5) #equal to max.x 
    
  min.y1 <- min.y+ y.l
  min.y2 <- min.y+ (y.l*2)
  min.y3 <- min.y+ (y.l*3)
  min.y4 <- min.y+ (y.l*4)
  min.y5 <- min.y+ (y.l*5) #equal to max.y 
    
#run on x axes
 a1 <- 1:21
 a2 <- 2:22
 a3 <- 3:23
 a4 <- 4:24
 a5 <- 5:25
 b1 <- a1[seq(1, length(a1), 5)] #return index- true for all a above

 #run on y axes 
 c1 <- 1:5
 c2 <- 6:10
 c3 <- 11:15
 c4 <- 16:20
 c5 <- 21:25
 
 
 #create vectors with 0 and length of 25 (number of zones) 
 #and then fill it with the right min/max point
 
 z.min.x <- rep(0,25) 
 z.min.y <- rep(0,25) 
 z.max.x <- rep(0,25) 
 z.max.y <- rep(0,25) 
   
 #x: 1,6,11,16,21
 #y: 1,2,3,4,5
  z.min.x[a1[b1]] <- min.x
  z.min.y[c1] <- min.y
  z.max.x[a1[b1]] <- min.x1
  z.max.y[c1] <-  min.y1
  
  #x: 2,7,12,17,22
  #y: 6,7,8,9,10
  z.min.x[a2[b1]] <- min.x1 
  z.min.y[c2] <-  min.y1
  z.max.x[a2[b1]] <- min.x2
  z.max.y[c2] <-  min.y2   
  
  #x: 3,8,13,18,23
  #y: 11,12,13,14,15  
  z.min.x[a3[b1]] <- min.x2 
  z.min.y[c3] <-  min.y2
  z.max.x[a3[b1]] <- min.x3
  z.max.y[c3] <-  min.y3   
 
  #x: 4,9,14,19,24
  #y: 16,17,18,19,20
  z.min.x[a4[b1]] <- min.x3 
  z.min.y[c4] <-  min.y3
  z.max.x[a4[b1]] <- min.x4
  z.max.y[c4] <-  min.y4   
    
  #x: 5,10,15,20,25
  #y: 21,22,23,24,25
  z.min.x[a5[b1]] <- min.x4 
  z.min.y[c5] <-  min.y4
  z.max.x[a5[b1]] <- max.x
  z.max.y[c5] <-  max.y   
    
    
  tag.name <- c(1:25)
  
  slope.v <- which(trial_list$trials == trial.name)
  slope <- trial_list$angle[slope.v]
  
  zones25 <- data.frame(slope, tag.name, z.min.x, z.max.x, z.min.y, z.max.y)
  
###################################################################
  
  #when the order of zones known:
  #check with dat$x/y where its located in each frame- 
  #mark with zones tag name (check each zone by itself)
  #then sum all the frames, to get duration of time in the specific zone 
  #and add it to "zones25" dataframe
  
  
  #check where zone of each frame of dat (x,y) located
  zone <- numeric()
  for (i in 1:nrow(dat)) {
  
    for (n in 1:nrow(zones25)) {
      
      if(((dat$new.x[i] > zones25$z.min.x[n]) && (dat$new.x[i] < zones25$z.max.x[n]) && (dat$y.center[i]> zones25$z.min.y[n]) && (dat$y.center[i] < zones25$z.max.y[n]))){
        zone <- c(zone, zones25$tag.name[n])
      }#if
      
    }#n
    
    
  }#i
  
  #for the last 4 frame, that somehow not include in the previous for loop
  if(length(zone) < nrow(dat)){
  for (i in (length(zone)+1):nrow(dat)) {
    
    for (n in 1:nrow(zones25)) {
      
      if(((dat$new.x[i] > zones25$z.min.x[n]) && (dat$new.x[i] < zones25$z.max.x[n]) && (dat$y.center[i]> zones25$z.min.y[n]) && (dat$y.center[i] < zones25$z.max.y[n]))){
        zone <- c(zone, zones25$tag.name[n])
      }#inside if
      
    }#n
    
    
  }#i
    
  }#outside if
  

 dat$zone <- zone 
 
 
 #time in zone
 time.in.zone.f <- numeric()
 for (i in 1:nrow(zones25)) {
   
  time.in.zone.f <- c(time.in.zone.f, length(which(dat$zone == zones25$tag.name[i])))
   
 }#i

 zones25$time.in.zone.f <- time.in.zone.f
 
 time.in.zone.sec <- time.in.zone.f/25
 
 zones25$time.in.zone.sec <- time.in.zone.sec
 
 
 #number of visits
 #first, create 'order of zones' vector and then calculate number of visits in each zone
 
 index <- numeric()
 for (i in 2:nrow(dat)) {
   
  if(dat$zone[i] == dat$zone[i-1]){
    index <- c(index, 0)
  }else{
    index <- c(index, 1)
  }#if  

   
 }#i
 
 #adding the first frame
 index <- c(1, index)
 
 dat$index <- index
 
 #order of zones
 zones.order <- numeric()
 for (i in 2:nrow(dat)) {
   
   if(dat$zone[i] == dat$zone[i-1]){
     next
   }else{
     zones.order <- c(zones.order, dat$zone[i])
   }#if  
   
 }#i
 
 #adding the first frame
 zones.order <- c(dat$zone[1], zones.order)
 
 i <- c(1:length(zones.order))
 
 zones.order.df <- data.frame(i, zones.order)
 
 #calculate number of visits
 n.visits <- numeric()
 for (i in 1:nrow(zones25)) {
   
   n.visits <- c(n.visits, length(which(zones.order == zones25$tag.name[i])))
   
 }#i
 
 zones25$n.visits <- n.visits
 
 
 time_visits <- c(time.in.zone.sec/n.visits) #time in zone dividing by number of visits
 zones25$time_visits <- time_visits
 
 
 #dir.create("zones25/csv")
 #dir.create("zones25/rdata")
 write.csv(zones25, file = paste("zones25/csv/", trial.name, ".csv", sep=""), row.names = F)
 save(zones25, file = paste('zones25/rdata/', trial.name, '.rdata', sep=''))
 
 dir.create("zones25/dat")
 save(dat, file = paste('zones25/dat/', trial.name, '.rdata', sep = ""))
 
 
 dir.create("zones25/order.of.zones")
 save(zones.order.df, file = paste('zones25/order.of.zones/', trial.name, '.rdata', sep = ""))
 
 
}#j

