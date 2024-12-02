AllC2 <- read_csv("../results/All_Clusters.csv")
col2keep <- c('TOPPID','clusterAll')
AllC2 <- All_Clusters[,col2keep]

# Load the tracking data
data <- read.csv("../data/NESETracks6h.csv")
data$longitude <- transform_longitude_vector(data$longitude) # Shift to 0 - 360
range(data$longitude)
#data$longitude <- data$longitude - 256 # ShiftLong  to avoid issues with the IDL
c2 <- c("FieldID","track_id","JulDate","longitude","latitude","Season","date_gmt", "time_gmt" )
data <- data[,c2]
colnames(data) <- c("FieldID","TOPPID","JulDate","longitude","latitude","Season","date_gmt", "time_gmt","clusterAll" )

# Match the clusters to the tracks
data2 <- merge(data,AllC2, by.x = 'TOPPID')
write.csv('data2','tracksAllClusters.csv')
