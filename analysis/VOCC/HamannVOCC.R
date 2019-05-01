library(SDMTools)     # install package to read and write ESRI ASCII grids
library(yaImpute)     # install package for k-nearest neighbour (kNN) search
library(dplyr)

# Simple example data
# start <- asc2dataframe("eg_data/MAT6190.asc")
# finish  <- asc2dataframe("eg_data/MAT2020s.asc")

# Two variable example data
var1_s <- asc2dataframe("eg_data/PC1-6190.asc")# principal component grids
var2_s <- asc2dataframe("eg_data/PC2-6190.asc")
var1_e  <- asc2dataframe("eg_data/PC1-2020s.asc")
var2_e  <- asc2dataframe("eg_data/PC2-2020s.asc")


# make sparate named lists containing climate rasters or dataframes
# element names should describe which variable they contain
# the variable_names vector will contain what the column or layer within each element is called

start_data <- list(var1 = var1_s, var2 = var2_s)
end_data <- list(var1 = var1_e, var2 = var2_e)

b <- (max(var1_s$var.1)-min(var1_s$var.1))/120  

# STEP 1: Extract varible grids as dataframes for both time slices and generate IDs

# glimpse(start)
# glimpse(start2)

data_lists_to_dfs <- function (start_data, 
                               end_data, 
                               x = "x", y = "y", 
                               variable_names, 
                               raster = FALSE
  ){
  #n_variables <- length(variable_names)
  start_xy <- start_data[[1]][, c(x, y)]  # data frame of XY coords
  end_xy <- end_data[[1]][, c(x, y)]   
  
  # start_data_vars <- matrix(nrow = length(start_xy[,1]), ncol = length(variable_names))
  # end_data_vars <- matrix(nrow = length(end_xy[,1]), ncol = length(variable_names))

  start_data_vars <- list()
  end_data_vars <- list()
  
  # if data is in raster form  
  if (raster) {
    for (i in seq_along(variable_names)) {
      start_data_vars[i] <- as.data.frame(raster::rasterToPoints(start_data[[i]]))[ ,3]
      end_data_vars[i] <- as.data.frame(raster::rasterToPoints(end_data[[i]]))[ ,3]
    }
    start_df <- as.data.frame(start_data_vars)
    names(start_df) <- gsub("V", "z", names(start_df))
    start_df <- cbind(start_xy, start_df)
    end_df <- as.data.frame(end_data_vars)
    names(end_df) <- gsub("V", "z", names(end_df))
    end_df <- cbind(end_xy, end_df)
  } else {
    
    # if data is in list of dataframes from predict functions
    for (i in seq_along(variable_names)) {
      var <- variable_names[i]
      start_data_vars[i] <- start_data[[i]][var]
      end_data_vars[i] <- end_data[[i]][var]
    }
    
    start_df <- as.data.frame(start_data_vars)
    names(start_df) <- names(start_data)
    start_df <- cbind(start_xy, start_df)

    end_df <- as.data.frame(end_data_vars)
    names(end_df) <- names(start_data)
    end_df <- cbind(start_xy, end_df)
  }
  
  if (!identical(length(start_df), length((end_df)))) {
    warning("Start and end data are not the same length.", 
      "Only coordinate combinations found in both will be retained.")
  }
  inner_join(start_df, end_df, by = c(x, y), suffix = c("_s", "_e"))
}


dist_based_vocc <- function( start_data, 
          end_data, 
          x = "x",
          y = "y", 
          variable_names = c("var.1", "var.2"), 
          thresholds = c(0.13, 0.1), # plus/minus thresholds to define climate match
          cell_size = 2, # 2 km utm grid
          max_dist = NULL,
          raster = FALSE
  ){
  
  # check that start_data list is equal in length to varaible_names vector
  if (!identical(length(variable_names), length((start_data)))) {
    stop("Must have a layer for each varible, ",
      "therefore `start_data` must be of the same length as `varible_names`.")
  }
  
  if (!identical(length(variable_names), length((thresholds)))) {
    stop("Must have `thresholds` value for all `varible_names`.")
  }
  
  n_variables <- length(variable_names)
  
  data <- data_lists_to_dfs(start_data, end_data, x = x, y = y, variable_names, raster)
  #d <- vector(length = length(nrow(data))     # empty vector to write distance to climate match

  X <- data[x] # x coords
  Y <- data[y] # y coords
  idxy <- cbind(id=1:nrow(data), X, Y)   # data frame of IDs and XY coords
  
  # Apply difference threshold using rounding
  round_fact <- c()
  start <- list() # FIXME: does this need to be a list?
  end <- list() # FIXME: does this need to be a list?
  for (i in seq_along(thresholds)) {
      round_fact <- 1/(thresholds[i]*2)   # inverse for rounding, double for plus/minus
      start[[i]] <- round(data[ , (2 + i)]*round_fact)/round_fact  # vector of rounded start values 
      end[[i]] <- round(data[ , (2+ n_variables + i)]*round_fact)/round_fact # vector of rounded end values 
      if (i==1) {
        s <- as.vector(start[[i]])
        e <- as.vector(end[[i]])
      } else {
        s <- paste(s, as.vector(start[[i]])) 
        e <- paste(e, as.vector(end[[i]])) 
      }
      
  }
  
  # Generate list of unique values in start
  u  <- unique(s)[order(unique(s))]          # list of unique values, or PC1/PC2 combinations
browser()
  # Find nearest analogue for each location
  # # Very slow method:
  # match <- function(u){c(u == e)}    # function finding climate matches of u with f
  # m     <- sapply(u, match)               # list of climate matches for unique values
  # 
  # for(i in 1:length(d)){          # loop for all grid cells in both time periods
  #   mi   <- m[[u==s[i]]]          # recalls list of climate matches for p[i]
  #   d[i] <- sqrt(min((X[i]-X[mi])^2 + (Y[i]-Y[mi])^2))    # distance to closest match
  # }
  ## results in 15 infinity warnings

# kNN search method (works for multiple variables):
#dist_kNN_search <- function(idxy, s, e, u){
sid <- list()                                 # empty list for source IDs
tid <- list()                                  # empty list for target IDs
d   <- list()                              # empty list for distances

  for(i in u){                 # loop for each unique PC1/PC2 combination
  browser() # FIXME: (idxy, s == i, drop = FALSE) : undefined columns selected
    sxy <- idxy[s==i, drop = FALSE]          # coordinates of i-th unique combination in start
    txy[[i]] <- idxy[e==i, drop = FALSE]          # coordinates of i-th unique combination in end
    sid[[i]] <- sxy$id
    if(nrow(exy) > 0){                    # kNN search unless no-analogue climate
      knn <- data.frame(ann(as.matrix(txy[,-1]), as.matrix(sxy[,-1]), k=1)$knnIndexDist)    
      tid[[i]] <- txy[knn[,1],"id"]        # the IDs of the closest matches  
      d[[i]] <- sqrt(knn[,2])         # their corresponding geographic distances
    } else {                              # else statement for no-analogue climates
      tid[[i]] <- rep(NA,nrow(sxy)) # flag destinations as missing for no analogues
      d[[i]] <- rep(Inf,nrow(sxy))    # flag distances as infinity for no analogues
    }
}  
  sid <- do.call("c", sid)
  tid <- do.call("c", tid)
  d <- do.call("c", d)

#}
#dist_kNN_search(idxy, s, e, u)

sxy <- merge(sid, idxy, by.y="id", all.x=T, all.y=F, sort=F)[2:3]  # source coordinates
#head(sxy)
txy <- merge(tid, idxy, by.y="id", all.x=T, all.y=F, sort=F)[2:3]  # target coordinates
#head(txy)
names(txy) = c("target_X", "target_Y")


# write output table in CSV format with source and target coordinates and distances
dist_tab <- cbind(id = sid, txy, distance = d)   
out <- inner_join(data, dist_tab, by = "id")
# View(outtab)

# writes out log10 velocities and distances multiplied by 100 in ESRI ASCII format
# conversion: -200=0.01km, -100=0.1km, 0=1km, 100=10km, 200=100km etc.
if (!max_dist) {
  max_dist <- min((max(X)-min(X)), (max(Y)-min(Y)))
}

out$distance[out$distance==Inf] <- max_dist  # sets no analogue to 10,000km
out$distance[out$distance==0] <- cell_size/2  # sets zero distance to 1/2 cell size
out$logDist <- round(log10(out$distance)) 
out$logSpeed <- round(log10(out$distance/2))
out
}

dist_based_vocc(start_data = start_data, 
                end_data = end_data, 
                x = "x",
                y = "y", 
                variable_names = c("var.1", "var.1"), 
                thresholds = c(0.13, 0.13), # plus/minus thresholds to define climate match
                cell_size = 0.5, # 2 km utm grid
                max_dist = 10000,
                raster = FALSE
                )
