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
ggplot(data, aes(x=phot_g_mean_flux_over_error)) +
  geom_histogram(fill="steelblue", color="black") +
  xlab("G-band mean flux divided by its error") +
  ylab("Count") +
  ggtitle("Distribution: Mean flux divided by its error in G-band")

ggplot(data, aes(x=phot_g_mean_mag)) +
  geom_histogram(fill="steelblue", color="black") +
  xlab("G-band mean magnitude") +
  ylab("Count") +
  ggtitle("Distribution: Mean magnitude in G-band")

ggplot(data, aes(x=phot_g_mean_flux_over_error, y=phot_g_mean_mag)) + 
  geom_point() +
  geom_smooth(se = FALSE, method = "gam", formula = y ~ s(log(x))) +
  xlab("G-band mean flux divided by its error") +
  ylab("G-band mean magnitude") +
  ggtitle("Mean magnitude vs Mean flux divided by its error in G-band")

ggplot(data, aes(x=ra_dec_corr, y=mean_varpi_factor_al)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)

# EDA  - Shaam


# EDA - Tama

