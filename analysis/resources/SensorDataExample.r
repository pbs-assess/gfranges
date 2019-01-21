# Simple example to demonstrate extraction of sensor data from GFBioSQL

library(gfplot)
require("RODBC")

# SQL to extract salinity data from set #1 of the 2018 West Coast Haida Gwaii
# synoptic trawl survey. TRIP_ID 84230 = 2018 WCHG synoptic survey;
# FE_MAJOR_LEVEL_ID 1 = Set #1; SENSOR_DATA_ATTRIBUTE 4 = Dissolved O2;
# SENSOR_DATA_UNIT_CODE 5 = Millilitres per litre
sql1 <- paste("SELECT SD.TIME_STAMP,",
              "SD.SENSOR_DATA_VALUE",
              "FROM FISHING_EVENT FE",
              "INNER JOIN FISHING_EVENT_SENSOR_DATAFILE FESD ON",
              "FE.FISHING_EVENT_ID = FESD.FISHING_EVENT_ID",
              "INNER JOIN SENSOR_DATA SD ON",
              "FESD.SENSOR_DATAFILE_NAME = SD.SENSOR_DATAFILE_NAME AND",
              "TIME_STAMP BETWEEN FE_BEGIN_DEPLOYMENT_TIME AND",
              "FE_END_RETRIEVAL_TIME",
              "WHERE FE.TRIP_ID = 84230 AND",
              "FE.FE_MAJOR_LEVEL_ID = 1 AND",
              "SD.SENSOR_DATA_ATTRIBUTE_CODE = 4 AND",
              "SD.SENSOR_DATA_UNIT_CODE = 5",
              "ORDER BY FE.FE_MAJOR_LEVEL_ID,",
              "SD.TIME_STAMP")

# SQL to extract the beginning and end of bottom contact time
sql2 <- paste("SELECT FE_MAJOR_LEVEL_ID,",
              "FE_BEGIN_BOTTOM_CONTACT_TIME,",
              "FE_END_BOTTOM_CONTACT_TIME",
              "FROM FISHING_EVENT FE",
              "WHERE TRIP_ID = 84230 AND",
              "FE_MAJOR_LEVEL_ID = 1")

# Fetch the data frames
# cnn <- gfplot:::db_connection()

#cnn <- odbcDriverConnect(paste("Driver={SQL Server};Server=dfbcv9twvasp001;","Database=GFBioSQL;Trusted_Connection=Yes;"))
sdf <- gfplot::run_sql("GFBioSQL", sql1) 
bdf <- gfplot::run_sql("GFBioSQL", sql2) 

# Plot the dissolved O2 profile and show the begin/end bottom contact times
plot(sdf$TIME_STAMP, sdf$SENSOR_DATA_VALUE, col = "red", type = "l", lwd = 2,
   xlab = "Time", ylab = "Dissolved O2 (ml/L)")
abline(v = bdf$FE_BEGIN_BOTTOM_CONTACT_TIME, lty = 2, lwd = 2)
abline(v = bdf$FE_END_BOTTOM_CONTACT_TIME, lty = 2, lwd = 2)
