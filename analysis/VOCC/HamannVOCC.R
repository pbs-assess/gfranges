library(SDMTools)     # install package to read and write ESRI ASCII grids
library(yaImpute)     # install package for k-nearest neighbour (kNN) search

# Simple example data
present <- asc2dataframe("eg_data/MAT6190.asc")
future  <- asc2dataframe("eg_data/MAT2020s.asc")

# Two variable example data
present <- asc2dataframe("eg_data/PC1-6190.asc")# principal component grids
present2 <- asc2dataframe("eg_data/PC2-6190.asc")
future  <- asc2dataframe("eg_data/PC1-2020s.asc")
future2  <- asc2dataframe("eg_data/PC2-2020s.asc")


# STEP 1: Extract varible grids as dataframes for both time slices and generate IDs

# glimpse(present)
# glimpse(present2)

# x <- present$x                    # vector of grid cell x coordinates
# y <- present$y                    # vector of grid cell y coordinates


idxy <- cbind(id=1:nrow(present),present[,1:2])   # data frame of IDs and XY coords

# glimpse(idxy)


# STEP 2: Apply difference threshold using rounding

t <- 0.25               # plus/minus threshold to define climate match
t <- 1/(t*2)            # inverse for rounding, double for plus/minus

p <- round(present$var.1*t)/t     # vector of rounded present climate values 
f <- round(future$var.1*t)/t      # vector of rounded future climate values 
d <- vector(length=length(p))     # empty vector to write distance to climate match

# # Repeat for additional variables as needed
# t2 <- 0.5               # plus/minus threshold to define climate match
# t2 <- 1/(t2*2)            # inverse for rounding, double for plus/minus
# 
# p2 <- round(present2$var.1*t2)/t2     # vector of rounded present climate values 
# f2 <- round(future2$var.1*t2)/t2      # vector of rounded future climate values 

# # STEP 3: Combine variables (if more than one)
# p  <- paste(p,p2)                         # PC1/PC2 combinations in present climate
# f  <- paste(f,f2)                         # PC1/PC2 combinations in future climate

# STEP 4: Generate list of unique values in present
u  <- unique(p)[order(unique(p))]          # list of unique values, or PC1/PC2 combinations

# STEP 5: Find nearest analogue for each location

# Simple method:
match <- function(u){c(which(u==f))}    # function finding climate matches of u with f
m     <- sapply(u, match)               # list of climate matches for unique values

for(i in 1:length(p)){                  # loop for all grid cells of p
  mi   <- m[[which(u==p[i])]]          # recalls list of climate matches for p[i]
  d[i] <- sqrt(min((x[i]-x[mi])^2 + (y[i]-y[mi])^2))    # distance to closest match
}

# kNN search method (works for multiple variables):
sid <- c()                                 # empty vector for source IDs
tid <- c()                                 # empty vector for target IDs
d   <- c()                                 # empty vector for distances

for(i in u){                          # loop for each unique PC1/PC2 combination
  pxy <- idxy[which(p==i),]           # coordinates of i-th combination in present
  fxy <- idxy[which(f==i),]           # coordinates of i-th combination in future
  sid <- c(sid, pxy$id)               # append i-th PC1/PC2 combination to previous 
  
  if(nrow(fxy)>0){                    # kNN search unless no-analogue climate
    knn <- data.frame(ann(as.matrix(fxy[,-1]), as.matrix(pxy[,-1]), k=1)$knnIndexDist)      
    tid <- c(tid, fxy[knn[,1],"id"]) # the IDs of the closest matches  
    d <- c(d, sqrt(knn[,2]))         # their corresponding geographic distances
  }
  else {                              # else statement for no-analogue climates
    tid <- c(tid, rep(NA,nrow(pxy))) # flag destinations as missing for no analogues
    d <- c(d, rep(Inf,nrow(pxy)))    # flag distances as infinity for no analogues
  }
}


#### something weird happening here...
glimpse(sid)
glimpse(idxy)
sxy <- merge(sid, idxy, by.y="id", all.x=T, all.y=F, sort=F)[2:3]  # source coordinates
head(sxy)
txy <- merge(tid, idxy, by.y="id", all.x=T, all.y=F, sort=F)[2:3]  # target coordinates
head(txy)
names(txy)=c("target_y","target_x")


# write output table in CSV format with source and target coordinates and distances
outtab <- cbind(id=sid, sxy, txy, distance=d)   
write.csv(outtab, "output.csv", row.names=F)

View(outtab)
# writes out log10 velocities and distances multiplied by 100 in ESRI ASCII format
# conversion: -200=0.01km, -100=0.1km, 0=1km, 100=10km, 200=100km etc.
out=merge(present1[,1:2], outtab[,c(2,3,6)], by=c("y","x"), sort=F)
out$distance[out$distance==Inf] <- 10000  # sets no analogue to 10,000km
out$distance[out$distance==0] <- 0.5  # sets zero distance to 0.5km (1/2 cell size)
out$logDist=round(log10(out$distance)*100)
out$logSpeed=round(log10(out$distance/50)*100)
dataframe2asc(out)


