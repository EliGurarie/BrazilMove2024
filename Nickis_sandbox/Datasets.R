# Hebblewhite et al 2008
# DOI: https://doi.org/10.1890/06-1708.1
# migrant and resident elk (Cervus elaphus), all females (N=119)

# Hebblewhite & Merrill 2007
# DOI: https://doi.org/10.1007/s00442-007-0661-y
# N= 14 VHF, N=16 GPS collared wolves

# UTM zone North: 11N

elk <- read.csv("./data/Elk_data.csv")
wolf <- read.csv("./data/Wolf_data.csv")

length(unique(elk$individual.local.identifier)) # 132 individuals

length(unique(subset(elk, sensor.type=="gps")$individual.local.identifier)) # 31 gps ids

length(unique(wolf$individual.local.identifier)) # 15 individuals

length(unique(subset(wolf, sensor.type=="gps")$individual.local.identifier)) # 15 gps ids

wolf_gps <- subset(wolf, sensor.type=="gps")
elk_gps <-subset(elk, sensor.type=="gps")

# save data
save(wolf_gps, file="./data/wolf_gps.rda")
save(elk_gps, file="./data/elk_gps.rda")

# write to csv
write.csv(elk_gps, file="C:/Users/nicol/Documents/BrazilMove2024/data/Elk_GPS_data.csv")

# -----------------------------------------------------------------------

# Mountain tapir, Colombia (N=3)
# The Nature Conservancy Colombia

tapir <- read.csv("./data/MountainTapirColombia.csv")

# save
save(tapir, file="./data/tapir.rda")
