# Import GAIA data (GaiaSource_1000172165251650944_1000424567594791808)
data <- read.csv("gaia.csv")

# summary of dta
summary(data)

# Total missing data
sum(is.na(data))

# Count data type
table(sapply(data, class))

# % of missing data.
sum(is.na(data))/prod(dim(data)) * 100

# Columns that contain missing data
names(which(sapply(data, anyNA)))

# Top 5 features w/ missing data
head(sort(colSums(sapply(data, is.na)), decreasing = TRUE), 5)

# Correlation Matrix
data_numerical <- data[, sapply(data, class) == "numeric"]
correlationMatrix <- cor(Filter(sd, data_numerical), method = "pearson", use = "complete.obs")

# Top 5 correlated variables
correlationMatrix[lower.tri(correlationMatrix,diag=TRUE)] <- NA
correlationMatrix <- as.data.frame(as.table(correlationMatrix))
correlationMatrix <- na.omit(correlationMatrix)
correlationMatrix <- correlationMatrix[order(-abs(correlationMatrix$Freq)),]
head(correlationMatrix, 10)

# EDA - Simeon
ggplot(data, aes(x=phot_g_mean_flux_over_error)) +
  geom_histogram(fill="steelblue", color="black")

ggplot(data, aes(x=phot_g_mean_mag)) +
  geom_histogram(fill="steelblue", color="black")

ggplot(data, aes(x=phot_g_mean_flux_over_error, y=phot_g_mean_mag)) + 
  geom_point()

# EDA  - Shaam


# EDA - Tama

