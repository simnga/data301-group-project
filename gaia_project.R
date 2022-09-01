# Import GAIA data (GaiaSource_1000172165251650944_1000424567594791808)
data <- read.csv("gaia.csv")

# Total missing data
sum(is.na(data))

# Count data type
table(sapply(data, class))

# % of missing data.
sum(is.na(data))/prod(dim(data)) * 100

# Columns that contain missing data
names(which(sapply(data, anyNA)))

# EDA - Simeon


# EDA  - Shaam


# EDA - Tama
