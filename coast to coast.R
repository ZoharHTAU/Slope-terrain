

v <- dir('zones25/order.of.zones')

segments_all <- data.frame()

direc.counts.df <- data.frame()#variables: rat, direction, count
combine.direct.counts.df <- data.frame()

for (j in 1:length(v)) {
  
  
  trial.name <-  unlist(strsplit(v[j], ".rdata"))

  load(paste('zones25/order.of.zones/', trial.name, ".rdata", sep = ""))
  load(paste('zones25/rdata/', trial.name, ".rdata", sep = ""))
  load(paste('zones25/dat/', trial.name, ".rdata", sep = ""))
  
  rat <- character()
  category <- character() #lower.strip.lr, lower.strip.rl, ......
  #ind.start <- numeric()
  #ind.stop <- numeric()
  frame.start <- numeric()
  frame.stop <- numeric()
  
  
  
  segments.list <- list()
  
  
 ##lower-strip zone
  
  l15 <- which((zones.order.df$zones.order == 1) | (zones.order.df$zones.order == 5))#l for "left" starting, 1-5 for zone name
  
  if(length(l15) > 0){
    
  df15 <- zones.order.df[l15,]
  
  a <- numeric()
  if(nrow(df15) > 1){
  
  for (i in 2:nrow(df15)) {
    
    if((df15$zones.order[i] == df15$zones.order[i-1]) && (df15$zones.order[i] == 1)){
      
      a <- c(a, i-1)
      
    }else if((df15$zones.order[i] == df15$zones.order[i-1]) && (df15$zones.order[i] == 5)){
      
      a <- c(a, i)
      
    }#else if
    
  }#i
    
  }# if(nrow(df15) > 1)
  
  #delete the first row if its "5" and the last if its "1"
  if (df15$zones.order[1] == 5){
    a <- c(1, a)
  }#if
  
  if (df15$zones.order[nrow(df15)] == 1){
    a <- c(a, nrow(df15))
  }#if
  
  
  
  if(length(a) > 0){
  df15 <- df15[-c(a),] #delete all the irrelevant cases (stored in vector-a)
  }#if
  
  
  #find in which segment the rat cross the relevant y
  if(nrow(df15) > 0){
  a1 <- numeric()
  for (i in 1:(nrow(df15)-1)) {
    if(df15$zones.order[i] == 1){
    b <- dat$y.center[(zones.order.df$start[df15$i[i]]):(zones.order.df$stop[df15$i[i+1]])]
    
    for (n in 1:length(b)) {
      
      if(b[n] > (zones25$z.max.y[6])){
        a1 <- c(a1, i, i+1)
      }#if
      
    }#n
    }#if
 }#i
  a1 <- unique(a1)
  
  if (length(a1) > 0){
  df15 <- df15[-c(a1),] #delete all the irrelevant cases (stored in vector-a)
  }#if
  
  }#if(nrow(df15) > 0)
  
 #find in which segment the rat travel back and forth in two zones
  if(nrow(df15) > 0){
  a2 <- numeric()
  for (i in 1:nrow(df15)) {
    if(df15$zones.order[i] == 1){
      
      if((length((zones.order.df$zones.order[df15$i[i]]):(zones.order.df$zones.order[df15$i[i+1]]))) > length(unique((zones.order.df$zones.order[df15$i[i]]):(zones.order.df$zones.order[df15$i[i+1]])))){
        
        a2 <- c(a2, i, i+1)
        
      }#if- inside
      
    }#if- outside
  }#i
  
  if (length(a2) > 0) {
  df15 <- df15[-c(a2),] #delete all the irrelevant cases (stored in vector-a)
  }#if
  
  }#if(nrow(df15) > 0)
  
  
  #check
  #for (i in 1:(nrow(df15)-1)) {
   # print((zones.order.df$zones.order[df15$i[i]]):(zones.order.df$zones.order[df15$i[i+1]]))
  #}#i
  
  
  
  start <- numeric()
  stop <- numeric()
  zone.start <- numeric()
  zone.stop <- numeric()
  if(nrow(df15) > 0){
  for (i in 1:nrow(df15)) {
    if(df15$zones.order[i] == 1){
    start <- c(start, df15$stop[i])
    zone.start <- c(zone.start, df15$zones.order[i])
    }#if
  }#i
  
  for (i in 2:nrow(df15)) {
    if(df15$zones.order[i] == 5){
    stop <- c(stop, df15$start[i])
    zone.stop <- c(zone.stop, df15$zones.order[i])
    }#if
  }#i
  
  }#if(nrow(df15) > 0)
  
  category <- rep("lower.strip.lr", length(start))
  rat <- gsub("trial", "rat",trial.name ) 
  rat <- rep(rat, length(start))
  
  e1 <- data.frame()
  e1 <- data.frame(rat, category, start, stop, zone.start, zone.stop)
  
  if(length(e1) > 0){
    segments.list$e1 <- e1
  }#if
  
  }#if length(l15) > 0
  
  #direc.counts.df
  rat <- gsub("trial", "rat",trial.name )
  slope <- zones25$slope[1]
  direction <- "lower.strip.lr"
  if(nrow(e1) == 0){
    count <- 0
  }else{
  count <- nrow(e1)
  }#if(nrow(e1) == 0)
  
  f1 <- data.frame(rat, slope, direction, count)
  
  direc.counts.df <- rbind(direc.counts.df, f1)
  
  #####
  ##lower-strip zone
  #right to left
  
  r51 <- which((zones.order.df$zones.order == 1) | (zones.order.df$zones.order == 5))#r for "right" starting, 5-1 for zone name
  
  if(length(r51) > 0){
  df51 <- zones.order.df[r51,]
  
  category <- rep("lower.strip.rl", nrow(df51))
  df51$category <- category
  
  a <- numeric()
  if(nrow(df51) > 1){
  for (i in 2:nrow(df51)) {
    
    if((df51$zones.order[i] == df51$zones.order[i-1]) && (df51$zones.order[i] == 5)){
      
      a <- c(a, i-1)
      
    }else if((df51$zones.order[i] == df51$zones.order[i-1]) && (df51$zones.order[i] == 1)){
      
      a <- c(a, i)
      
    }#else if
    
  }#i
    
  }#if(nrow(df51) > 1)  
  
  #delete the first row if its "1" and the last is its "5"
  if(nrow(df51) > 0){
    
  if (df51$zones.order[1] == 1){
    a <- c(1, a)
  }#if
  
  if (df51$zones.order[nrow(df51)] == 5){
    a <- c(a, nrow(df51))
  }#if
  
  
  if(length(a) > 0){
    df51 <- df51[-c(a),] #delete all the irrelevant cases (stored in vector-a)
  }#if
  
  }#if(nrow(df51) > 0)
  
  #find in which segment the rat cross the relevant y
  if(nrow(df51) > 0){
  a1 <- numeric()
  for (i in 1:(nrow(df51)-1)) {
    if(df51$zones.order[i] == 5){
      b <- dat$y.center[(zones.order.df$start[df51$i[i]]):(zones.order.df$stop[df51$i[i+1]])]
      
      for (n in 1:length(b)) {
        
        if(b[n] > (zones25$z.max.y[6])){
          a1 <- c(a1, i, i+1)
        }#if
        
      }#n
    }#if
  }#i
  
  a1 <- unique(a1)
  
  if (length(a1) > 0){
    df51 <- df51[-c(a1),] #delete all the irrelevant cases (stored in vector-a)
  }#if
  
  }#if(nrow(df51) > 0)
  
  #find in which segment the rat travel back and forth between two zones
  if(nrow(df51) > 0){
  a2 <- numeric()
  for (i in 1:nrow(df51)) {
    if(df51$zones.order[i] == 5){
      
      if((length((zones.order.df$zones.order[df51$i[i]]):(zones.order.df$zones.order[df51$i[i+1]]))) > length(unique((zones.order.df$zones.order[df51$i[i]]):(zones.order.df$zones.order[df51$i[i+1]])))){
        
        a2 <- c(a2, i, i+1)
        
      }#if- inside
      
    }#if- outside
  }#i
  
  if (length(a2) > 0) {
    df51 <- df51[-c(a2),] #delete all the irrelevant cases (stored in vector-a)
  }#if
  
  }#if(nrow(df51) > 0)
  
  
  #chack
  #for (i in 1:(nrow(df51)-1)) {
   #print((zones.order.df$zones.order[df51$i[i]]):(zones.order.df$zones.order[df51$i[i+1]]))
  #}#i
  
  
    
  start <- numeric()
  stop <- numeric()
  zone.start <- numeric()
  zone.stop <- numeric()
  
  if(nrow(df51) > 0){
    
  for (i in 1:nrow(df51)) {
    if(df51$zones.order[i] == 5){
      start <- c(start, df51$stop[i])
      zone.start <- c(zone.start, df51$zones.order[i])
    }#if
  }#i
  
  for (i in 2:nrow(df51)) {
    if(df51$zones.order[i] == 1){
      stop <- c(stop, df51$start[i])
      zone.stop <- c(zone.stop, df51$zones.order[i])
    }#if
  }#i
  }#if(nrow(df51) > 0)
  
  category <- rep("lower.strip.rl", length(start))
  rat <- gsub("trial", "rat",trial.name ) 
  rat <- rep(rat, length(start))
  
  e2 <- data.frame()
  e2 <- data.frame(rat, category, start, stop, zone.start, zone.stop)
  
  if(length(e2) > 0){
    segments.list$e2 <- e2
  }#if
  
  
  }#if length(r51) > 0
  
  #direc.counts.df
  rat <- gsub("trial", "rat",trial.name ) 
  slope <- zones25$slope[1]
  direction <- "lower.strip.rl"
  if(nrow(e2) == 0){
    count <- 0
  }else{
    count <- nrow(e2)
  }#if(nrow(e2) == 0)
  
  f2 <- data.frame(rat, slope, direction, count)
  
  direc.counts.df <- rbind(direc.counts.df, f2)
  
  #############################################
  
  #bottom -> top
  bot <- which((zones.order.df$zones.order == 1) | (zones.order.df$zones.order == 2) | 
                 (zones.order.df$zones.order == 3) | (zones.order.df$zones.order == 4) |
                 (zones.order.df$zones.order == 5))#bot for "bottom" starting, 1-5 for zone name)
  
  top <-  which((zones.order.df$zones.order == 21) | (zones.order.df$zones.order == 22) | 
                  (zones.order.df$zones.order == 23) | (zones.order.df$zones.order == 24) |
                  (zones.order.df$zones.order == 25))#top for "top" ending, 21-25 for zone name)
                
  
  
  if((length(bot) > 0) & (length(top) > 0)){
  
  df.bot <- zones.order.df[bot,]
  df.top <- zones.order.df[top,]
    
  
  
  #delete cases which i+1 is still in the bottom line (zones 1-5)
    a <- numeric()
    for (i in 1:(nrow(df.bot)-1)) {
      
      if ((zones.order.df$zones.order[df.bot$i[i]+1] == 1) |
          (zones.order.df$zones.order[df.bot$i[i]+1] == 2) |
          (zones.order.df$zones.order[df.bot$i[i]+1] == 3) |
          (zones.order.df$zones.order[df.bot$i[i]+1] == 4) |
          (zones.order.df$zones.order[df.bot$i[i]+1] == 5)){
        
        a <- c(a, i)
      }#if
      
    }#i
    
    if (length(a) > 0){
      df.bot <- df.bot[-c(a),] #delete all the irrelevant cases (stored in vector-a)
    }#if 
   
  
  #delete the last row in df.bot if this segment is dead end (not travel up)- still in the bottom zones in the last frame (nrow(dat))
  if((df.bot$zones.order[nrow(df.bot)] == 1 |
      df.bot$zones.order[nrow(df.bot)] == 2 |
      df.bot$zones.order[nrow(df.bot)] == 3 |
      df.bot$zones.order[nrow(df.bot)] == 4 |
      df.bot$zones.order[nrow(df.bot)] == 5) & (df.bot$stop[nrow(df.bot)] == nrow(dat))  ){
    df.bot <- df.bot[-c(nrow(df.bot)),]
  }#if
  
  #merge df.top together with df.bot, and sort them, then remove irrelevant to cases
  bot.top <- rbind(df.bot, df.top)#merge
  
  bot.top <- bot.top[order(bot.top$i),]#sort
  
  category <- rep("bot.top", nrow(bot.top))
  bot.top$category <- category
  
  #delete the first row if its "top zones" (21-25) and the last if its "bottom zones" (1-5)
  b2 <- numeric()
  b3 <- numeric()
  b4 <- numeric()
  #firs rows:
  b1 <- which((bot.top$zones.order == 1) | (bot.top$zones.order == 2) | (bot.top$zones.order == 3) | 
               (bot.top$zones.order == 4) | (bot.top$zones.order == 5))
  
  if(length(b1) > 0){
    
  if((b1[1]-1) > 0){
    b2 <- 1:(b1[1]-1)
  }#if
  
  if (length(b2) > 0){
    bot.top <- bot.top[-c(b2),] #delete all the irrelevant cases (stored in vector-a)
  }#if  
  
  }#if(length(b1) > 0)  
    
  #last rows:
  b3 <- which((bot.top$zones.order == 21) | (bot.top$zones.order == 22) | (bot.top$zones.order == 23) | 
                (bot.top$zones.order == 24) | (bot.top$zones.order == 25))
  #if all zones from the bottom side (no zone from the top side)- need to be deleted
  if(length(b3) < 1){
    
    b4 <- 1:nrow(bot.top)
    
  }else if(nrow(bot.top) >= (b3[length(b3)]+1)){
  b4 <- (b3[length(b3)]+1):nrow(bot.top)
  }#if
  
  if (length(b4) > 0){
    bot.top <- bot.top[-c(b4),] #delete all the irrelevant cases (stored in vector-a)
  }#if  
         
  #if there is no visits on the bottom zones at all- that   means that there is only visits in top zones
  #than delete all top.zones ()there is no travels between bottom to top
  bb3 <- which((bot.top$zones.order == 1) | (bot.top$zones.order == 2) | (bot.top$zones.order == 3) | 
                 (bot.top$zones.order == 4) | (bot.top$zones.order == 5))
  bb4 <- numeric()     
  if(length(bb3) == 0){
    bb4 <- 1:nrow(bot.top)
  }#if
  
  if (length(bb4) > 0){
    bot.top <- bot.top[-c(bb4),] #delete all the irrelevant cases (stored in vector-a)
  }#if  
  
  #make sure that rows is alternately bottom zones - top zones, delete irrelevant cases (doubles)
  if(nrow(bot.top) > 0){
    
  a3 <- numeric()
  for (i in 2:nrow(bot.top)) {
    
    if(((bot.top$zones.order[i] == 1) | (bot.top$zones.order[i] == 2) | (bot.top$zones.order[i] == 3)
        | (bot.top$zones.order[i] == 4) | (bot.top$zones.order[i] == 5)) 
       & ((bot.top$zones.order[i-1] == 1) | (bot.top$zones.order[i-1] == 2) | (bot.top$zones.order[i-1] == 3)
       | (bot.top$zones.order[i-1] == 4) | (bot.top$zones.order[i-1] == 5))){
      
      a3 <- c(a3, i-1)
      
    }else if(((bot.top$zones.order[i] == 21) | (bot.top$zones.order[i] == 22) | (bot.top$zones.order[i] == 23)
              | (bot.top$zones.order[i] == 24) | (bot.top$zones.order[i] == 25)) 
             & ((bot.top$zones.order[i-1] == 21) | (bot.top$zones.order[i-1] == 22) | (bot.top$zones.order[i-1] == 23)
             | (bot.top$zones.order[i-1] == 24) | (bot.top$zones.order[i-1] == 25))){
      
      a3 <- c(a3, i)
      
    }#else if
    
  }#i
  
  if (length(a3) > 0){
    bot.top <- bot.top[-c(a3),] #delete all the irrelevant cases (stored in vector-a)
  }#if  
  
  }#if(nrow(bot.top) > 0)
  
  #find in which segment the rat travel back and forth between two zones
  if(nrow(bot.top) > 0){
  a4 <- numeric()
  for (i in 1:nrow(bot.top)) {
    if((bot.top$zones.order[i] == 1) | (bot.top$zones.order[i] == 2) | (bot.top$zones.order[i] == 3)
       | (bot.top$zones.order[i] == 4) | (bot.top$zones.order[i] == 5)){
      
      
      b5 <- zones.order.df[c((bot.top$i[i]):(bot.top$i[i+1])),]

      
      if((length(b5$zones.order)) > (length(unique(b5$zones.order)))){
        
        a4 <- c(a4, i, i+1)
        
      }#if- inside
      
    }#if- outside
  }#i
  
  if (length(a4) > 0) {
    bot.top <- bot.top[-c(a4),] #delete all the irrelevant cases (stored in vector-a)
  }#if
  
  }#if(nrow(bot.top) > 0)
  
  
  #create vector for each column (c1- first, c2 -second ect.) 
  a1 <- 1:21
  a2 <- 2:22
  a3 <- 3:23
  a4 <- 4:24
  a5 <- 5:25
  c1 <- a1[seq(1, length(a1), 5)]
  c2 <- a2[seq(1, length(a1), 5)]
  c3 <- a3[seq(1, length(a1), 5)]
  c4 <- a4[seq(1, length(a1), 5)]
  c5 <- a5[seq(1, length(a1), 5)]
  
  
  #check in each segment in which column the rat visit, then use unique(),
  #if unique > 2, delete this segment.
  if(nrow(bot.top) > 0){
  
  a6 <- numeric()
  for (i in 1:(nrow(bot.top)-1)) {
    
    if((bot.top$zones.order[i] == 21) || (bot.top$zones.order[i] == 22) || (bot.top$zones.order[i] == 23) 
       || (bot.top$zones.order[i] == 24) || (bot.top$zones.order[i] == 25)){
      next
    }#if
    
    b6 <- zones.order.df[c((bot.top$i[i]):(bot.top$i[i+1])),]
    
    zones <- b6$zones.order
    
    b7 <- character()
    
   if(length(intersect(zones, c1)) > 0){
     b7 <- c(b7, "c1")}#if
   if(length(intersect(zones, c2)) > 0){
     b7 <- c(b7, "c2")}#if
   if(length(intersect(zones, c3)) > 0){
     b7 <- c(b7, "c3")}#if
   if(length(intersect(zones, c4)) > 0){
     b7 <- c(b7, "c4")}#if
   if(length(intersect(zones, c5)) > 0){
     b7 <- c(b7, "c5")}#if
   

   if(length(unique(b7)) > 2){
     a6 <- c(a6, i, i+1)
   }#if
   
  }#i
  
    
  if (length(a6) > 0) {
    bot.top <- bot.top[-c(a6),] #delete all the irrelevant cases (stored in vector-a)
  }#if
  }#if(nrow(bot.top) > 0)
  
  start <- numeric()
  stop <- numeric()
  zone.start <- numeric()
  zone.stop <- numeric()
  
  if(nrow(bot.top) > 0){
    
  for (i in 1:nrow(bot.top)) {
    if((bot.top$zones.order[i] == 1) | (bot.top$zones.order[i] == 2) | (bot.top$zones.order[i] == 3)
       | (bot.top$zones.order[i] == 4) | (bot.top$zones.order[i] == 5)){
      start <- c(start, bot.top$stop[i])
      zone.start <- c(zone.start, bot.top$zones.order[i])
    }#if
  }#i
  
  for (i in 2:nrow(bot.top)) {
    if((bot.top$zones.order[i] == 21) || (bot.top$zones.order[i] == 22) || (bot.top$zones.order[i] == 23) 
       || (bot.top$zones.order[i] == 24) || (bot.top$zones.order[i] == 25)){
      stop <- c(stop, bot.top$start[i])
      zone.stop <- c(zone.stop, bot.top$zones.order[i])
    }#if
  }#i
  
  }#if(nrow(bot.top) > 0)
  
  category <- rep("bot.top", length(start))
  rat <- gsub("trial", "rat",trial.name ) 
  rat <- rep(rat, length(start))
  
  e3 <- data.frame()
  e3 <- data.frame(rat, category, start, stop, zone.start, zone.stop)
  
  if(length(e3) > 0){
    segments.list$e3 <- e3
  }#if
  
  #direc.counts.df
  rat <- gsub("trial", "rat",trial.name ) 
  slope <- zones25$slope[1]
  direction <- "bot.top"
  if(nrow(e3) == 0){
    count <- 0
  }else{
    count <- nrow(e3)
  }#if(nrow(e3) == 0)
  
  f3 <- data.frame(rat, slope, direction, count)
  
  direc.counts.df <- rbind(direc.counts.df, f3)
  
  #############################################
  
  #top -> bottom

  
    
    df.top <- zones.order.df[top,]
    df.bot <- zones.order.df[bot,]
    

    #delete cases which i+1 is still in the top line (zones 21-25)
    a <- numeric()
    for (i in 1:(nrow(df.top)-1)) {
      
      if ((zones.order.df$zones.order[df.top$i[i]+1] == 21) |
          (zones.order.df$zones.order[df.top$i[i]+1] == 22) |
          (zones.order.df$zones.order[df.top$i[i]+1] == 23) |
          (zones.order.df$zones.order[df.top$i[i]+1] == 24) |
          (zones.order.df$zones.order[df.top$i[i]+1] == 25)){
        
        a <- c(a, i)
      }#if
      
    }#i
    
    if (length(a) > 0){
      df.top <- df.top[-c(a),] #delete all the irrelevant cases (stored in vector-a)
    }#if 
    
    
    #delete the last row in df.top if this segment is dead end (not travel down)- still in the top zones in the last frame (nrow(dat))
    if((df.top$zones.order[nrow(df.top)] == 21 |
        df.top$zones.order[nrow(df.top)] == 22 |
        df.top$zones.order[nrow(df.top)] == 23 |
        df.top$zones.order[nrow(df.top)] == 24 |
        df.top$zones.order[nrow(df.top)] == 25) & (df.top$stop[nrow(df.top)] == nrow(dat))  ){
      df.top <- df.top[-c(nrow(df.top)),]
    }#if
    
    #merge df.top together with df.bot, and sort them, then remove irrelevant to cases
    top.bot <- rbind(df.bot, df.top)#merge
    
    top.bot <- top.bot[order(top.bot$i),]#sort
    
    category <- rep("top.bot", nrow(top.bot))
    top.bot$category <- category
    
    #delete the first row if its "bottom zones" (1-5) and the last if its "top zones" (21-25)
    b2 <- numeric()
    b3 <- numeric()
    b4 <- numeric()
    #firs rows:
    b1 <- which((top.bot$zones.order == 21) | (top.bot$zones.order == 22) | (top.bot$zones.order == 23) | 
                  (top.bot$zones.order == 24) | (top.bot$zones.order == 25))

    if((b1[1]-1) > 0){
    b2 <- 1:(b1[1]-1)
    }#if
    
    if (length(b2) > 0){
      top.bot <- top.bot[-c(b2),] #delete all the irrelevant cases (stored in vector-b)
    }#if  
    
    #last rows:
    #add if(top.bot$zones.order[nrow(top.bot)] == 21-25)...:
    
    b3 <- which((top.bot$zones.order == 1) | (top.bot$zones.order == 2) | (top.bot$zones.order == 3) | 
                  (top.bot$zones.order == 4) | (top.bot$zones.order == 5))
    
    #if all zones from the top side (no zone from the bottom side)- need to be deleted
    if(length(b3) < 1){
      
      b4 <- 1:nrow(top.bot)
      
    }else if(nrow(top.bot) >= (b3[length(b3)]+1)){
    b4 <- (b3[length(b3)]+1):nrow(top.bot)
    }#if
    
    if (length(b4) > 0){
      top.bot <- top.bot[-c(b4),] #delete all the irrelevant cases (stored in vector-a)
    }#if  
    
    #if there is no visits on the top zones at all- that   means that there is only visits in bottom zones
    #than delete all bottom.zones (there is no travels between top to bottom)
    bb3 <- which((top.bot$zones.order == 21) | (top.bot$zones.order == 22) | (top.bot$zones.order == 23) | 
                   (top.bot$zones.order == 24) | (top.bot$zones.order == 25))
    bb4 <- numeric()     
    if(length(bb3) == 0){
      bb4 <- 1:nrow(top.bot)
    }#if
    
    if (length(bb4) > 0){
      top.bot <- top.bot[-c(bb4),] #delete all the irrelevant cases (stored in vector-a)
    }#if  
    
    #make sure that rows is alternately bottom zones - top zones, delete irrelevant cases (doubles)
    if(nrow(top.bot) > 0){
    a3 <- numeric()
    for (i in 2:nrow(top.bot)) {
      
      if(((top.bot$zones.order[i] == 21) | (top.bot$zones.order[i] == 22) | (top.bot$zones.order[i] == 23)
          | (top.bot$zones.order[i] == 24) | (top.bot$zones.order[i] == 25)) 
         & ((top.bot$zones.order[i-1] == 21) | (top.bot$zones.order[i-1] == 22) | (top.bot$zones.order[i-1] == 23)
            | (top.bot$zones.order[i-1] == 24) | (top.bot$zones.order[i-1] == 25))){
        
        a3 <- c(a3, i-1)
        
      }else if(((top.bot$zones.order[i] == 1) | (top.bot$zones.order[i] == 2) | (top.bot$zones.order[i] == 3)
                | (top.bot$zones.order[i] == 4) | (top.bot$zones.order[i] == 5)) 
               & ((top.bot$zones.order[i-1] == 1) | (top.bot$zones.order[i-1] == 2) | (top.bot$zones.order[i-1] == 3)
                  | (top.bot$zones.order[i-1] == 4) | (top.bot$zones.order[i-1] == 5))){
        
        a3 <- c(a3, i)
        
      }#else if
      
    }#i
    
    if (length(a3) > 0){
      top.bot <- top.bot[-c(a3),] #delete all the irrelevant cases (stored in vector-a)
    }#if  
    
    }#if(nrow(top.bot) > 0)
    
    
    #find in which segment the rat travel back and forth between two zones
    if(nrow(top.bot) > 0){
      
    if(nrow(top.bot) > 1){
    a4 <- numeric()
    for (i in 1:(nrow(top.bot)-1)) {
      if((top.bot$zones.order[i] == 21) | (top.bot$zones.order[i] == 22) | (top.bot$zones.order[i] == 23)
         | (top.bot$zones.order[i] == 24) | (top.bot$zones.order[i] == 25)){
        
        
        b5 <- zones.order.df[c((top.bot$i[i]):(top.bot$i[i+1])),]
        
        
        if((length(b5$zones.order)) > (length(unique(b5$zones.order)))){
          
          a4 <- c(a4, i, i+1)
          
        }#if- inside
        
      }#if- outside
    }#i
    
    if (length(a4) > 0) {
      top.bot <- top.bot[-c(a4),] #delete all the irrelevant cases (stored in vector-a)
    }#if
    }#if(nrow(top.bot)
    
    }#if(nrow(top.bot) > 0)  
      
    #create vector for each column (c1- first, c2 -second ect.) 
    a1 <- 1:21
    a2 <- 2:22
    a3 <- 3:23
    a4 <- 4:24
    a5 <- 5:25
    c1 <- a1[seq(1, length(a1), 5)]
    c2 <- a2[seq(1, length(a1), 5)]
    c3 <- a3[seq(1, length(a1), 5)]
    c4 <- a4[seq(1, length(a1), 5)]
    c5 <- a5[seq(1, length(a1), 5)]
    
    
    #check in each segment in which column the rat visit, then use unique(),
    #if unique > 2, delete this segment.
    if(nrow(top.bot) > 0){
    
    a6 <- numeric()
    b6 <- numeric()
    for (i in 1:(nrow(top.bot)-1)) {
      
      if((top.bot$zones.order[i] == 1) || (top.bot$zones.order[i] == 2) || (top.bot$zones.order[i] == 3) 
         || (top.bot$zones.order[i] == 4) || (top.bot$zones.order[i] == 5)){
        next
      }#if
      
      b6 <- zones.order.df[c((top.bot$i[i]):(top.bot$i[i+1])),]
      
      zones <- b6$zones.order
      
      b7 <- character()
      
      if(length(intersect(zones, c1)) > 0){
        b7 <- c(b7, "c1")}#if
      if(length(intersect(zones, c2)) > 0){
        b7 <- c(b7, "c2")}#if
      if(length(intersect(zones, c3)) > 0){
        b7 <- c(b7, "c3")}#if
      if(length(intersect(zones, c4)) > 0){
        b7 <- c(b7, "c4")}#if
      if(length(intersect(zones, c5)) > 0){
        b7 <- c(b7, "c5")}#if
      
      
      if(length(unique(b7)) > 2){
        a6 <- c(a6, i, i+1)
      }#if
      
    }#i
    
    
    if (length(a6) > 0) {
      top.bot <- top.bot[-c(a6),] #delete all the irrelevant cases (stored in vector-a)
    }#if
    
    }#if(nrow(top.bot) > 0)
    
    start <- numeric()
    stop <- numeric()
    zone.start <- numeric()
    zone.stop <- numeric()
    
    if(nrow(top.bot) > 0){
    
    for (i in 1:nrow(top.bot)) {
      if((top.bot$zones.order[i] == 21) || (top.bot$zones.order[i] == 22) || (top.bot$zones.order[i] == 23) 
         || (top.bot$zones.order[i] == 24) || (top.bot$zones.order[i] == 25)){
        start <- c(start, top.bot$stop[i])
        zone.start <- c(zone.start, top.bot$zones.order[i])
      }#if
    }#i
    
    for (i in 2:nrow(top.bot)) {
      if((top.bot$zones.order[i] == 1) | (top.bot$zones.order[i] == 2) | (top.bot$zones.order[i] == 3)
         | (top.bot$zones.order[i] == 4) | (top.bot$zones.order[i] == 5)){
        stop <- c(stop, top.bot$start[i])
        zone.stop <- c(zone.stop, top.bot$zones.order[i])
      }#if
    }#i
    
    }#if(nrow(top.bot) > 0)
    
    category <- rep("top.bot", length(start))
    rat <- gsub("trial", "rat",trial.name ) 
    rat <- rep(rat, length(start))
    
    e4 <- data.frame()
    e4 <- data.frame(rat, category, start, stop, zone.start, zone.stop)
    
    if(length(e4) > 0){
      segments.list$e4 <- e4
    }#if
    
    #direc.counts.df
    rat <- gsub("trial", "rat",trial.name ) 
    slope <- zones25$slope[1]
    direction <- "top.bot"
    if(nrow(e4) == 0){
      count <- 0
    }else{
      count <- nrow(e4)
    }#if(nrow(e4) == 0)
    
    f4 <- data.frame(rat, slope, direction, count)
    
    direc.counts.df <- rbind(direc.counts.df, f4)
    
  }#if length(bot) > 0) && (length(top) > 0)
  
  
  
  #############################################
  
  #left -> right
  left <- which((zones.order.df$zones.order == 6) | (zones.order.df$zones.order == 11) | 
                 (zones.order.df$zones.order == 16) | (zones.order.df$zones.order == 21))#left for "left" starting, 6,11,16,21 for zone name
  
  right <-  which((zones.order.df$zones.order == 10) | (zones.order.df$zones.order == 15) | 
                  (zones.order.df$zones.order == 20) | (zones.order.df$zones.order == 25))#right for "right" ending, 10,15,20,25 for zone name
  
  
  
  if((length(left) > 0) && (length(right) > 0)){
    
    df.left <- zones.order.df[left,]
    df.right <- zones.order.df[right,]  
  
    
    #delete cases which i+1 is still in the left column (zones 6-21)
    a <- numeric()
    if(nrow(df.left) > 1){
   
    for (i in 1:(nrow(df.left)-1)) {
      
      if ((zones.order.df$zones.order[df.left$i[i]+1] == 6)  |
          (zones.order.df$zones.order[df.left$i[i]+1] == 11) |
          (zones.order.df$zones.order[df.left$i[i]+1] == 16) |
          (zones.order.df$zones.order[df.left$i[i]+1] == 21)){
        
        a <- c(a, i)
      }#if
      
    }#i
    
    }#if(nrow(df.left)>1)
    
    if (length(a) > 0){
      df.left <- df.left[-c(a),] #delete all the irrelevant cases (stored in vector-a)
    }#if 
    
    
    #delete the last row in df.left if this segment is dead end (not travel right)- still in the left zones in the last frame (nrow(dat))
    if((df.left$zones.order[nrow(df.left)] == 6  |
        df.left$zones.order[nrow(df.left)] == 11 |
        df.left$zones.order[nrow(df.left)] == 16 |
        df.left$zones.order[nrow(df.left)] == 21) & (df.left$stop[nrow(df.left)] == nrow(dat))  ){
      
      df.left <- df.left[-c(nrow(df.left)),]
      
    }#if
    
    #merge df.left together with df.right, and sort them, then remove irrelevant to cases
    left.right <- rbind(df.left, df.right)#merge
    
    left.right <- left.right[order(left.right$i),]#sort
    
    category <- rep("left.right", nrow(left.right))
    left.right$category <- category
    
    #delete the first row if its "right zones" (10,15,20,25) and the last if its "left zones" (6,11,16,21)
      b2 <- numeric()
    b3 <- numeric()
    b4 <- numeric()
    #firs rows:
    b1 <- which((left.right$zones.order == 6) | (left.right$zones.order == 11) | (left.right$zones.order == 16) | 
                  (left.right$zones.order == 21))
   
    if((b1[1]-1) > 0){
      b2 <- 1:(b1[1]-1)
    }#if
    
    if (length(b2) > 0){
      left.right <- left.right[-c(b2),] #delete all the irrelevant cases (stored in vector-b)
    }#if  
    
    #last rows:
    b3 <- which((left.right$zones.order == 10) | (left.right$zones.order == 15) | (left.right$zones.order == 20) | 
                  (left.right$zones.order == 25))
    #if all zones from the left side (no zone from the right side)- need to be deleted
    if(length(b3) < 1){
      
      b4 <- 1:nrow(left.right)
      
    }else if(nrow(left.right) >= (b3[length(b3)]+1)){
      
      b4 <- (b3[length(b3)]+1):nrow(left.right)
      
    }#else if
    
    if (length(b4) > 0){
      left.right <- left.right[-c(b4),] #delete all the irrelevant cases (stored in vector-a)
    }#if  
    
    
    
    #make sure that rows is alternately left zones - right zones, delete irrelevant cases (doubles)
    if(nrow(left.right) > 0){
    
    a3 <- numeric()
    for (i in 2:nrow(left.right)) {
      
      if(((left.right$zones.order[i] == 6) | (left.right$zones.order[i] == 11) | (left.right$zones.order[i] == 16)
          | (left.right$zones.order[i] == 21)) 
         & ((left.right$zones.order[i-1] == 6) | (left.right$zones.order[i-1] == 11) | (left.right$zones.order[i-1] == 16)
            | (left.right$zones.order[i-1] == 21))){
        
        a3 <- c(a3, i-1)
        
      }else if(((left.right$zones.order[i] == 10) | (left.right$zones.order[i] == 15) | (left.right$zones.order[i] == 20)
                | (left.right$zones.order[i] == 25)) 
               & ((left.right$zones.order[i-1] == 10) | (left.right$zones.order[i-1] == 15) | (left.right$zones.order[i-1] == 20)
                  | (left.right$zones.order[i-1] == 25))){
        
        a3 <- c(a3, i)
        
      }#else if
      
    }#i
    
    if (length(a3) > 0){
      left.right <- left.right[-c(a3),] #delete all the irrelevant cases (stored in vector-a)
    }#if  
    
    }#nrow(left.right) > 0
    
    #find in which segment the rodent travel back and forth between two zones
    if(nrow(left.right) > 0){
      
    a4 <- numeric()
    for (i in 1:nrow(left.right)) {
      if((left.right$zones.order[i] == 6) | (left.right$zones.order[i] == 11) | (left.right$zones.order[i] == 16)
         | (left.right$zones.order[i] == 21)){
        
        
        b5 <- zones.order.df[c((left.right$i[i]):(left.right$i[i+1])),]
        
        
        if((length(b5$zones.order)) > (length(unique(b5$zones.order)))){
          
          a4 <- c(a4, i, i+1)
          
        }#if- inside
        
      }#if- outside
    }#i
    
    if (length(a4) > 0) {
      left.right <- left.right[-c(a4),] #delete all the irrelevant cases (stored in vector-a)
    }#if
    
    }#nrow(left.right) > 0
    
    #create vector for each column (r1- first, r2 -second ect.) 
    r1 <- 6:10
    r2 <- 11:15
    r3 <- 16:20
    r4 <- 21:25

    
    
    #check in each segment in which column the rat visit, then use unique(),
    #if unique > 2, delete this segment- (means that the rodent traveled in more than two rows)
    if(nrow(left.right) > 0){
    
    a6 <- numeric()
    b6 <- numeric()
    for (i in 1:(nrow(left.right)-1)) {
      
      if((left.right$zones.order[i] == 10) || (left.right$zones.order[i] == 15) || (left.right$zones.order[i] == 20) 
         || (left.right$zones.order[i] == 25)){
        next
      }#if
      
      b6 <- zones.order.df[c((left.right$i[i]):(left.right$i[i+1])),]
      
      zones <- b6$zones.order
      
      b7 <- character()
      
      if(length(intersect(zones, r1)) > 0){
        b7 <- c(b7, "r1")}#if
      if(length(intersect(zones, r2)) > 0){
        b7 <- c(b7, "r2")}#if
      if(length(intersect(zones, r3)) > 0){
        b7 <- c(b7, "r3")}#if
      if(length(intersect(zones, r4)) > 0){
        b7 <- c(b7, "r4")}#if
  
      
      
      if(length(unique(b7)) > 2){
        a6 <- c(a6, i, i+1)
      }#if
      
    }#i
    
    
    if (length(a6) > 0) {
      left.right <- left.right[-c(a6),] #delete all the irrelevant cases (stored in vector-a)
    }#if
    
    }#nrow(left.right) > 0
  
    #delete segments that include visits in the bottom row (1-5)
    
    if(nrow(left.right) > 0){
    
    bot.row <- 1:5
    
#######   
    a7 <- numeric()
    for (u in seq(1, nrow(left.right), by = 2)) {
      
      b8 <- zones.order.df$zones.order[c((left.right$i[u]):(left.right$i[u+1]))]
      
      if(length(intersect(bot.row,b8)) > 0){
        a7 <- c(a7, u, u+1)
    }
    }#u
    
    if (length(a7) > 0) {
      left.right <- left.right[-c(a7),] #delete all the irrelevant cases (stored in vector-a)
    }#if
      
    }#nrow(left.right) > 0
      
    
    start <- numeric()
    stop <- numeric()
    zone.start <- numeric()
    zone.stop <- numeric()
    
    if(nrow(left.right) > 0){
      
    for (i in 1:nrow(left.right)) {
      if((left.right$zones.order[i] == 6) | (left.right$zones.order[i] == 11) | (left.right$zones.order[i] == 16)
         | (left.right$zones.order[i] == 21)){
        start <- c(start, left.right$stop[i])
        zone.start <- c(zone.start, left.right$zones.order[i])
      }#if
    }#i
    
    for (i in 2:nrow(left.right)) {
      if((left.right$zones.order[i] == 10) || (left.right$zones.order[i] == 15) || (left.right$zones.order[i] == 20) 
         || (left.right$zones.order[i] == 25)){
        stop <- c(stop, left.right$start[i])
        zone.stop <- c(zone.stop, left.right$zones.order[i])
      }#if
    }#i
    
    }#if(nrow(left.right) > 0)
    
    category <- rep("left.right", length(start))
    rat <- gsub("trial", "rat",trial.name ) 
    rat <- rep(rat, length(start))
    
    e5 <- data.frame()
    e5 <- data.frame(rat, category, start, stop, zone.start, zone.stop)
    
    if(length(e5) > 0){
      segments.list$e5 <- e5
    }#if
    
    #direc.counts.df
    rat <- gsub("trial", "rat",trial.name )
    slope <- zones25$slope[1]
    direction <- "left.right"
    if(nrow(e5) == 0){
      count <- 0
    }else{
      count <- nrow(e5)
    }#if(nrow(e5) == 0)
    
    f5 <- data.frame(rat, slope, direction, count)
    
    direc.counts.df <- rbind(direc.counts.df, f5)
   
    
    ###########################################
    
    #right -> left
    
    df.left <- zones.order.df[left,]
    df.right <- zones.order.df[right,]  
    
    
    #delete cases which i+1 is still in the right column (zones 10-25)
    if(nrow(df.right) > 1){
    a <- numeric()
    for (i in 1:(nrow(df.right)-1)) {
      
      if ((zones.order.df$zones.order[df.right$i[i]+1] == 10)  |
          (zones.order.df$zones.order[df.right$i[i]+1] == 15) |
          (zones.order.df$zones.order[df.right$i[i]+1] == 20) |
          (zones.order.df$zones.order[df.right$i[i]+1] == 25)){
        
        a <- c(a, i)
      }#if
      
    }#i
    
    if (length(a) > 0){
      df.right <- df.right[-c(a),] #delete all the irrelevant cases (stored in vector-a)
    }#if 
    
    }#nrow(df.right) > 1
    
    #delete the last row in df.right if this segment is dead end (not travel right)- still in the right zones in the last frame (nrow(dat))
    if((df.right$zones.order[nrow(df.right)] == 10  |
        df.right$zones.order[nrow(df.right)] == 15 |
        df.right$zones.order[nrow(df.right)] == 20 |
        df.right$zones.order[nrow(df.right)] == 25) & (df.right$stop[nrow(df.right)] == nrow(dat))  ){
      
      df.right <- df.right[-c(nrow(df.right)),]
      
    }#if
    
    #merge df.left together with df.right, and sort them, then remove irrelevant to cases
    right.left <- rbind(df.left, df.right)#merge
    
    right.left <- right.left[order(right.left$i),]#sort
    
    category <- rep("right.left", nrow(right.left))
    right.left$category <- category
    
    #delete the first row if its "left zones" (6,11,16,21) and the last if its "right zones" (10,15,20,25)
    b2 <- numeric()
    b3 <- numeric()
    b4 <- numeric()
    #firs rows:
    b1 <- which((right.left$zones.order == 10) | (right.left$zones.order == 15) | (right.left$zones.order == 20) | 
                  (right.left$zones.order == 25))
   
    if((b1[1]-1) > 0){
      b2 <- 1:(b1[1]-1)
    }#if
    
    if (length(b2) > 0){
      right.left <- right.left[-c(b2),] #delete all the irrelevant cases (stored in vector-b)
    }#if  
    
     #last rows:
    b3 <- which((right.left$zones.order == 6) | (right.left$zones.order == 11) | (right.left$zones.order == 16) | 
                  (right.left$zones.order == 21))
    
    #if all zones from the left side (no zone from the right side)- need to be deleted
    if(length(b3) < 1){
      
      b4 <- 1:nrow(right.left)
      
    }else if(nrow(right.left) >= (b3[length(b3)]+1)){
      b4 <- (b3[length(b3)]+1):nrow(right.left)
    }#if
    
    if (length(b4) > 0){
      right.left <- right.left[-c(b4),] #delete all the irrelevant cases (stored in vector-b)
    }#if  
    
    
    #make sure that rows is alternately right zones - left zones, delete irrelevant cases (doubles)
    if(nrow(right.left) > 0){
    a3 <- numeric()
    for (i in 2:nrow(right.left)) {
      
      if(((right.left$zones.order[i] == 10) | (right.left$zones.order[i] == 15) | (right.left$zones.order[i] == 20)
          | (right.left$zones.order[i] == 25)) 
         & ((right.left$zones.order[i-1] == 10) | (right.left$zones.order[i-1] == 15) | (right.left$zones.order[i-1] == 20)
            | (right.left$zones.order[i-1] == 25))){
        
        a3 <- c(a3, i-1)
        
      }else if(((right.left$zones.order[i] == 6) | (right.left$zones.order[i] == 11) | (right.left$zones.order[i] == 16)
                | (right.left$zones.order[i] == 21)) 
               & ((right.left$zones.order[i-1] == 6) | (right.left$zones.order[i-1] == 11) | (right.left$zones.order[i-1] == 16)
                  | (right.left$zones.order[i-1] == 21))){
        
        a3 <- c(a3, i)
        
      }#else if
      
    }#i
    
    if (length(a3) > 0){
      right.left <- right.left[-c(a3),] #delete all the irrelevant cases (stored in vector-a)
    }#if  
    
    }#nrow(right.left) > 0
    
    #find in which segment the rodent travel back and forth between two zones
    if(nrow(right.left) > 0){
      
    a4 <- numeric()
    for (i in 1:(nrow(right.left)-1)) {
      if((right.left$zones.order[i] == 10) | (right.left$zones.order[i] == 15) | (right.left$zones.order[i] == 20)
         | (right.left$zones.order[i] == 25)){
        
        
        b5 <- zones.order.df[c((right.left$i[i]):(right.left$i[i+1])),]
        
        
        if((length(b5$zones.order)) > (length(unique(b5$zones.order)))){
          
          a4 <- c(a4, i, i+1)
          
        }#if- inside
        
      }#if- outside
    }#i
    
    if (length(a4) > 0) {
      right.left <- right.left[-c(a4),] #delete all the irrelevant cases (stored in vector-a)
    }#if
    
    }#nrow(right.left) > 0
    
   

    #create vector for each column (r1- first, r2 -second ect.) 
    r1 <- 6:10
    r2 <- 11:15
    r3 <- 16:20
    r4 <- 21:25
    
    
    
    #check in each segment in which column the rat visit, then use unique(),
    #if unique > 2, delete this segment- (means that the rodent traveled in more than two rows)
    if(nrow(right.left) > 0){
    
    a6 <- numeric()
    b6 <- numeric()
    for (i in 1:(nrow(right.left)-1)) {
      
      if((right.left$zones.order[i] == 6) || (right.left$zones.order[i] == 11) || (right.left$zones.order[i] == 16) 
         || (right.left$zones.order[i] == 21)){
        next
      }#if
      
      b6 <- zones.order.df[c((right.left$i[i]):(right.left$i[i+1])),]
      
      zones <- b6$zones.order
      
      b7 <- character()
      
      if(length(intersect(zones, r1)) > 0){
        b7 <- c(b7, "r1")}#if
      if(length(intersect(zones, r2)) > 0){
        b7 <- c(b7, "r2")}#if
      if(length(intersect(zones, r3)) > 0){
        b7 <- c(b7, "r3")}#if
      if(length(intersect(zones, r4)) > 0){
        b7 <- c(b7, "r4")}#if
      
      
      
      if(length(unique(b7)) > 2){
        a6 <- c(a6, i, i+1)
      }#if
      
    }#i
    
    
    if (length(a6) > 0) {
      right.left <- right.left[-c(a6),] #delete all the irrelevant cases (stored in vector-a)
    }#if
    
    
    }#if(nrow(right.left) > 0)
    
    
    #delete segments that include visits in the bottom row (1-5)
    
   
      
    bot.row <- 1:5
    
    a7 <- numeric()
    b8 <- numeric()
    
    if(nrow(right.left) > 0){
    ######################################
    #for (i in 1:(nrow(right.left)-1)) {
      
     # b8 <- zones.order.df$zones.order[c((right.left$i[i]):(right.left$i[i+1]))]
      
      #if(length(intersect(bot.row,b8)) > 0){
       # a7 <- c(a, i, i+1)
      #}#if
      
    #}#i
    ####################################
    
    for (u in seq(1, nrow(right.left), by = 2)) {
      
      b8 <- zones.order.df$zones.order[c((right.left$i[u]):(right.left$i[u+1]))]
      
      if(length(intersect(bot.row,b8)) > 0){
        a7 <- c(a7, u, u+1)
      }####
    }#u
    
  
    if (length(a7) > 0) {
      right.left <- right.left[-c(a7),] #delete all the irrelevant cases (stored in vector-a)
    }#if
    
    }#if(nrow(right.left) > 0)
    
    
    start <- numeric()
    stop <- numeric()
    zone.start <- numeric()
    zone.stop <- numeric()
    
    if(nrow(right.left) > 0){
      
    for (i in 1:nrow(right.left)) {
      if((right.left$zones.order[i] == 10) || (right.left$zones.order[i] == 15) || (right.left$zones.order[i] == 20) 
         || (right.left$zones.order[i] == 25)){
        start <- c(start, right.left$stop[i])
        zone.start <- c(zone.start, right.left$zones.order[i])
      }#if
    }#i
    
    for (i in 2:nrow(right.left)) {
    
      if((right.left$zones.order[i] == 6) | (right.left$zones.order[i] == 11) | (right.left$zones.order[i] == 16)
         | (right.left$zones.order[i] == 21)){
        stop <- c(stop, right.left$start[i])
        zone.stop <- c(zone.stop, right.left$zones.order[i])
      }#if
    }#i
    
    }#if(nrow(right.left) > 0)
    
    category <- rep("right.left", length(start))
    rat <- gsub("trial", "rat",trial.name ) 
    rat <- rep(rat, length(start))
    
    
    e6 <- data.frame()
    e6 <- data.frame(rat, category, start, stop, zone.start, zone.stop)
    
    if(length(e6) > 0){
      segments.list$e6 <- e6
    }#if
    
   
    #direc.counts.df
    rat <- gsub("trial", "rat",trial.name ) 
    slope <- zones25$slope[1]
    direction <- "right.left"
    if(nrow(e6) == 0){
      count <- 0
    }else{
      count <- nrow(e6)
    }#if(nrow(e6) == 0)
    
    f6 <- data.frame(rat, slope, direction, count)
    
    direc.counts.df <- rbind(direc.counts.df, f6)
    
  }#if(length(left) > 0) && (length(right) > 0) 
  
  
  #combine left+right, up+down and the lower strip's directions and create new data frame:
  counts <- c(f1$count+f2$count, f3$count+f4$count, f5$count+f6$count)
  direc <- c("lower.strip", "vertical", "sideways")
  rat <- gsub("trial", "rat",trial.name ) 
  rat <- rep(rat, 3)
  slope <- rep(zones25$slope[1], 3)
  
  combine.direct.counts.df <- rbind(combine.direct.counts.df, data.frame(rat, slope, direc, counts))
  
  if(length(segments.list) > 0){
    
  if(nrow(segments.list[[1]]) > 0){
    
  segments.df <- data.frame()
  for (i in 1:length(segments.list)) {
    
    segments.df <- rbind(segments.df, segments.list[[i]])
    
  }#i
  
  ############

  #add the right slope
  slope <- rep(zones25$slope[1], nrow(segments.df))
  segments.df$slope <- slope
    
  #calculate time, distance and velocity for each segment(row)

  #distance
  dist.m <- numeric()
  for (i in 1:nrow(segments.df)) {
  
  d <- sum(sqrt((diff((dat$new.x[(segments.df$start[i]):(segments.df$stop[i])]))^2) + (diff((dat$y.center[(segments.df$start[i]):(segments.df$stop[i])]))^2)))
  
  dist.m <- c(dist.m, d/100)
  
  }#i
  
  segments.df$dist.m <- dist.m
  
  #time
  time.sec <- ((segments.df$stop - segments.df$start)+1)/25
  
  segments.df$time.sec <- time.sec
  
  #velocity= distance/time (m/sec)
  velocity <- dist.m/time.sec 

  segments.df$velocity <- velocity
  
  segments_all <- rbind(segments_all, segments.df) #bind all rats segments.df together to one dataframe
  
  }# if(length(segments.list) > 0 )
    
  }#if(length(segments.list) > 0)
  
}#j


write.csv(segments_all, "zones25/segments_all.csv")


#avg counts all rats without slopes
a1 <- mean(direc.counts.df$count[which(direc.counts.df$direction == "lower.strip.lr")])
a2 <- mean(direc.counts.df$count[which(direc.counts.df$direction == "lower.strip.rl")])
a3 <- mean(direc.counts.df$count[which(direc.counts.df$direction == "bot.top")])
a4 <- mean(direc.counts.df$count[which(direc.counts.df$direction == "top.bot")])
a5 <- mean(direc.counts.df$count[which(direc.counts.df$direction == "left.right")])
a6 <- mean(direc.counts.df$count[which(direc.counts.df$direction == "right.left")])  

avg.count <- c(a1, a2, a3, a4, a5, a6)
directions <- c("lower.strip.lr", "lower.strip.rl", "bot.top", "top.bot", "left.right", "right.left")
rats.avg.count <- data.frame(directions, avg.count)

write.csv(rats.avg.count, "zones25/rats.avg.count_no_slopes.csv")

#avg counts with slopes
a1 <- c("lower.strip.lr",0,mean(direc.counts.df$count[which((direc.counts.df$direction == "lower.strip.lr") & (direc.counts.df$slope == 0))]))
a2 <- c("lower.strip.rl",0,mean(direc.counts.df$count[which((direc.counts.df$direction == "lower.strip.rl") & (direc.counts.df$slope == 0))]))
a3 <- c("bot.top",0,mean(direc.counts.df$count[which((direc.counts.df$direction == "bot.top") & (direc.counts.df$slope == 0))]))
a4 <- c("top.bot",0,mean(direc.counts.df$count[which((direc.counts.df$direction == "top.bot") & (direc.counts.df$slope == 0))]))
a5 <- c("left.right",0,mean(direc.counts.df$count[which((direc.counts.df$direction == "left.right") & (direc.counts.df$slope == 0))]))
a6 <- c("right.left",0,mean(direc.counts.df$count[which((direc.counts.df$direction == "right.left") & (direc.counts.df$slope == 0))]))

b1 <- c("lower.strip.lr",15,mean(direc.counts.df$count[which((direc.counts.df$direction == "lower.strip.lr") & (direc.counts.df$slope == 15))]))
b2 <- c("lower.strip.rl",15,mean(direc.counts.df$count[which((direc.counts.df$direction == "lower.strip.rl") & (direc.counts.df$slope == 15))]))
b3 <- c("bot.top",15,mean(direc.counts.df$count[which((direc.counts.df$direction == "bot.top") & (direc.counts.df$slope == 15))]))
b4 <- c("top.bot",15,mean(direc.counts.df$count[which((direc.counts.df$direction == "top.bot") & (direc.counts.df$slope == 15))]))
b5 <- c("left.right",15,mean(direc.counts.df$count[which((direc.counts.df$direction == "left.right") & (direc.counts.df$slope == 15))]))
b6 <- c("right.left",15,mean(direc.counts.df$count[which((direc.counts.df$direction == "right.left") & (direc.counts.df$slope == 15))]))

c1 <- c("lower.strip.lr",30,mean(direc.counts.df$count[which((direc.counts.df$direction == "lower.strip.lr") & (direc.counts.df$slope == 30))]))
c2 <- c("lower.strip.rl",30,mean(direc.counts.df$count[which((direc.counts.df$direction == "lower.strip.rl") & (direc.counts.df$slope == 30))]))
c3 <- c("bot.top",30,mean(direc.counts.df$count[which((direc.counts.df$direction == "bot.top") & (direc.counts.df$slope == 30))]))
c4 <- c("top.bot",30,mean(direc.counts.df$count[which((direc.counts.df$direction == "top.bot") & (direc.counts.df$slope == 30))]))
c5 <- c("left.right",30,mean(direc.counts.df$count[which((direc.counts.df$direction == "left.right") & (direc.counts.df$slope == 30))]))
c6 <- c("right.left",30,mean(direc.counts.df$count[which((direc.counts.df$direction == "right.left") & (direc.counts.df$slope == 30))]))

d1 <- c("lower.strip.lr",45,mean(direc.counts.df$count[which((direc.counts.df$direction == "lower.strip.lr") & (direc.counts.df$slope == 45))]))
d2 <- c("lower.strip.rl",45,mean(direc.counts.df$count[which((direc.counts.df$direction == "lower.strip.rl") & (direc.counts.df$slope == 45))]))
d3 <- c("bot.top",45,mean(direc.counts.df$count[which((direc.counts.df$direction == "bot.top") & (direc.counts.df$slope == 45))]))
d4 <- c("top.bot",45,mean(direc.counts.df$count[which((direc.counts.df$direction == "top.bot") & (direc.counts.df$slope == 45))]))
d5 <- c("left.right",45,mean(direc.counts.df$count[which((direc.counts.df$direction == "left.right") & (direc.counts.df$slope == 45))]))
d6 <- c("right.left",45,mean(direc.counts.df$count[which((direc.counts.df$direction == "right.left") & (direc.counts.df$slope == 45))]))

g1 <- c("lower.strip.lr",60,mean(direc.counts.df$count[which((direc.counts.df$direction == "lower.strip.lr") & (direc.counts.df$slope == 60))]))
g2 <- c("lower.strip.rl",60,mean(direc.counts.df$count[which((direc.counts.df$direction == "lower.strip.rl") & (direc.counts.df$slope == 60))]))
g3 <- c("bot.top",60,mean(direc.counts.df$count[which((direc.counts.df$direction == "bot.top") & (direc.counts.df$slope == 60))]))
g4 <- c("top.bot",60,mean(direc.counts.df$count[which((direc.counts.df$direction == "top.bot") & (direc.counts.df$slope == 60))]))
g5 <- c("left.right",60,mean(direc.counts.df$count[which((direc.counts.df$direction == "left.right") & (direc.counts.df$slope == 60))]))
g6 <- c("right.left",60,mean(direc.counts.df$count[which((direc.counts.df$direction == "right.left") & (direc.counts.df$slope == 60))]))

h1 <- c("lower.strip.lr",75,mean(direc.counts.df$count[which((direc.counts.df$direction == "lower.strip.lr") & (direc.counts.df$slope == 75))]))
h2 <- c("lower.strip.rl",75,mean(direc.counts.df$count[which((direc.counts.df$direction == "lower.strip.rl") & (direc.counts.df$slope == 75))]))
h3 <- c("bot.top",75,mean(direc.counts.df$count[which((direc.counts.df$direction == "bot.top") & (direc.counts.df$slope == 75))]))
h4 <- c("top.bot",75,mean(direc.counts.df$count[which((direc.counts.df$direction == "top.bot") & (direc.counts.df$slope == 75))]))
h5 <- c("left.right",75,mean(direc.counts.df$count[which((direc.counts.df$direction == "left.right") & (direc.counts.df$slope == 75))]))
h6 <- c("right.left",75,mean(direc.counts.df$count[which((direc.counts.df$direction == "right.left") & (direc.counts.df$slope == 75))]))

i1 <- c("lower.strip.lr",90,mean(direc.counts.df$count[which((direc.counts.df$direction == "lower.strip.lr") & (direc.counts.df$slope == 90))]))
i2 <- c("lower.strip.rl",90,mean(direc.counts.df$count[which((direc.counts.df$direction == "lower.strip.rl") & (direc.counts.df$slope == 90))]))
i3 <- c("bot.top",90,mean(direc.counts.df$count[which((direc.counts.df$direction == "bot.top") & (direc.counts.df$slope == 90))]))
i4 <- c("top.bot",90,mean(direc.counts.df$count[which((direc.counts.df$direction == "top.bot") & (direc.counts.df$slope == 90))]))
i5 <- c("left.right",90,mean(direc.counts.df$count[which((direc.counts.df$direction == "left.right") & (direc.counts.df$slope == 90))]))
i6 <- c("right.left",90,mean(direc.counts.df$count[which((direc.counts.df$direction == "right.left") & (direc.counts.df$slope == 90))]))


rats.avg.count.slope <- rbind(a1, a2, a3, a4, a5, a6,
                                   b1, b2, b3, b4, b5, b6,
                                   c1, c2, c3, c4, c5, c6,
                                   d1, d2, d3, d4, d5, d6,
                                   g1, g2, g3, g4, g5, g6,
                                   h1, h2, h3, h4, h5, h6,
                                   i1, i2, i3, i4, i5, i6)


colnames(rats.avg.count.slope) <- c("direction", "slope", "avg.count")
rats.avg.count.slope <- data.frame(rats.avg.count.slope)
 
write.csv(rats.avg.count.slope, "zones25/rats.avg.count_slopes.csv")




#avg counts with slopes combined directions
a1 <- c("lower.strip",0,mean(combine.direct.counts.df$counts[which((combine.direct.counts.df$direc == "lower.strip") & (combine.direct.counts.df$slope == 0))]))
a2 <- c("vertical",0,mean(combine.direct.counts.df$counts[which((combine.direct.counts.df$direc == "vertical") & (combine.direct.counts.df$slope == 0))]))
a3 <- c("sideways",0,mean(combine.direct.counts.df$counts[which((combine.direct.counts.df$direc == "sideways") & (combine.direct.counts.df$slope == 0))]))

b1 <- c("lower.strip",15,mean(combine.direct.counts.df$counts[which((combine.direct.counts.df$direc == "lower.strip") & (combine.direct.counts.df$slope == 15))]))
b2 <- c("vertical",15,mean(combine.direct.counts.df$counts[which((combine.direct.counts.df$direc == "vertical") & (combine.direct.counts.df$slope == 15))]))
b3 <- c("sideways",15,mean(combine.direct.counts.df$counts[which((combine.direct.counts.df$direc == "sideways") & (combine.direct.counts.df$slope == 15))]))

c1 <- c("lower.strip",30,mean(combine.direct.counts.df$counts[which((combine.direct.counts.df$direc == "lower.strip") & (combine.direct.counts.df$slope == 30))]))
c2 <- c("vertical",30,mean(combine.direct.counts.df$counts[which((combine.direct.counts.df$direc == "vertical") & (combine.direct.counts.df$slope == 30))]))
c3 <- c("sideways",30,mean(combine.direct.counts.df$counts[which((combine.direct.counts.df$direc == "sideways") & (combine.direct.counts.df$slope == 30))]))

d1 <- c("lower.strip",45,mean(combine.direct.counts.df$counts[which((combine.direct.counts.df$direc == "lower.strip") & (combine.direct.counts.df$slope == 45))]))
d2 <- c("vertical",45,mean(combine.direct.counts.df$counts[which((combine.direct.counts.df$direc == "vertical") & (combine.direct.counts.df$slope == 45))]))
d3 <- c("sideways",45,mean(combine.direct.counts.df$counts[which((combine.direct.counts.df$direc == "sideways") & (combine.direct.counts.df$slope == 45))]))

g1 <- c("lower.strip",60,mean(combine.direct.counts.df$counts[which((combine.direct.counts.df$direc == "lower.strip") & (combine.direct.counts.df$slope == 60))]))
g2 <- c("vertical",60,mean(combine.direct.counts.df$counts[which((combine.direct.counts.df$direc == "vertical") & (combine.direct.counts.df$slope == 60))]))
g3 <- c("sideways",60,mean(combine.direct.counts.df$counts[which((combine.direct.counts.df$direc == "sideways") & (combine.direct.counts.df$slope == 60))]))

h1 <- c("lower.strip",75,mean(combine.direct.counts.df$counts[which((combine.direct.counts.df$direc == "lower.strip") & (combine.direct.counts.df$slope == 75))]))
h2 <- c("vertical",75,mean(combine.direct.counts.df$counts[which((combine.direct.counts.df$direc == "vertical") & (combine.direct.counts.df$slope == 75))]))
h3 <- c("sideways",75,mean(combine.direct.counts.df$counts[which((combine.direct.counts.df$direc == "sideways") & (combine.direct.counts.df$slope == 75))]))

i1 <- c("lower.strip",90,mean(combine.direct.counts.df$counts[which((combine.direct.counts.df$direc == "lower.strip") & (combine.direct.counts.df$slope == 90))]))
i2 <- c("vertical",90,mean(combine.direct.counts.df$counts[which((combine.direct.counts.df$direc == "vertical") & (combine.direct.counts.df$slope == 90))]))
i3 <- c("sideways",90,mean(combine.direct.counts.df$counts[which((combine.direct.counts.df$direc == "sideways") & (combine.direct.counts.df$slope == 90))]))


rats.avg.count.slope_combine.direc <- rbind(a1, a2, a3, 
                              b1, b2, b3,
                              c1, c2, c3, 
                              d1, d2, d3, 
                              g1, g2, g3, 
                              h1, h2, h3, 
                              i1, i2, i3)


colnames(rats.avg.count.slope_combine.direc) <- c("direction", "slope", "avg.count")
rats.avg.count.slope_combine.direc <- data.frame(rats.avg.count.slope_combine.direc)


write.csv(rats.avg.count.slope_combine.direc, "zones25/rats.avg.count_slopes_combine.direc.csv")
write.csv(combine.direct.counts.df, "zones25/combine.direct.counts.df_all.csv")






#don't forget to add the relevant slope

#######################################################
#lower strip- left to right
for (i in 1:(nrow(zones.order.df)-5)) {
  if ((zones.order.df$zones.order[i] == 1) && (zones.order.df$zones.order[i+1] == 2) && (zones.order.df$zones.order[i+2] == 3)
      && (zones.order.df$zones.order[i+3] == 4) && (zones.order.df$zones.order[i+4] == 5)){
    
    category <- c(category, "lower.strip.lr")
    ind.start <- c(ind.start, zones.order.df$i[i])
    ind.stop <- c(ind.stop, zones.order.df$i[i+4])
    frame.start <- c(frame.start, zones.order.df$start[i])
    frame.stop <- c(frame.stop, zones.order.df$stop[i+4])
  }
}#i


#lower strip- right to left
for (i in 1:nrow(zones.order.df)) {
  if ((zones.order.df$zones.order[i] == 5) && (zones.order.df$zones.order[i+1] == 4) && (zones.order.df$zones.order[i+2] == 3)
      && (zones.order.df$zones.order[i+3] == 2) && (zones.order.df$zones.order[i+4] == 1)){
    
    category <- c(category, "lower.strip.rl")
    ind.start <- c(ind.start, zones.order.df$i[i])
    ind.stop <- c(ind.stop, zones.order.df$i[i+4])
    frame.start <- c(frame.start, zones.order.df$start[i])
    frame.stop <- c(frame.stop, zones.order.df$stop[i+4])
  }
}#i





#lower strip- left to right
for (i in 1:nrow(zones.order.df)) {
  if ((zones.order.df$zones.order[i] == 1) && (zones.order.df$zones.order[i+4] == 5)){
    
    category <- c(category, "lower.strip.lr")
    ind.start <- c(ind.start, zones.order.df$i[i])
    ind.stop <- c(ind.stop, zones.order.df$i[i+4])
    frame.start <- c(frame.start, zones.order.df$start[i])
    frame.stop <- c(frame.stop, zones.order.df$stop[i+4])
  }#if
}#i





write.csv(right.left, "zones25/right.left.csv")


