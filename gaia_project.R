library(ggplot2)

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

# Top 15 sorted correlated variables
correlationMatrix[lower.tri(correlationMatrix,diag=TRUE)] <- NA
correlationMatrix <- as.data.frame(as.table(correlationMatrix))
correlationMatrix <- na.omit(correlationMatrix)
correlationMatrix <- correlationMatrix[order(-abs(correlationMatrix$Freq)),]
head(correlationMatrix, 15)

# EDA - Simeon

# Stellar Effective Temperature Distribution
ggplot(data, aes(x=teff_val)) +
  geom_histogram(fill="steelblue", color="black") +
  xlab("Stellar Effective Temperature") +
  ylab("Count") +
  ggtitle("Stellar Effective Temperature Distribution")

# Normality Test
shapiro.test(data_numerical$teff_val)

# Bp-Rp vs Stellar Effective Temperature
ggplot(data, aes(x=teff_val, y=bp_rp)) + 
  geom_point() +
  xlab("Stellar Effective Temperature") +
  ylab("BP - RP colour") +
  ggtitle("BP - RP colour vs Stellar Effective Temperature")

# Absolute Magnitude vs Stellar Effective Temperature
ggplot(data, aes(x=teff_val, y=phot_g_mean_mag+5*log10(parallax)-10)) + 
  geom_point() +
  xlab("Stellar Effective Temperature") +
  ylab("Absolute magnitude") +
  ggtitle("Absolute Magnitude vs Stellar Effective Temperature")


# EDA  - Shaam


# EDA - Tama

