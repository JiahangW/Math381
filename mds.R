# install.packages("wordcloud")
# install.packages("scatterplot3d")
# install.packages("DescTools")
library(wordcloud)
library(scatterplot3d)
library(DescTools)

# Load Dataset
typewriters <- read.csv("https://sites.math.washington.edu/~conroy/m381-general/MDSdataSets/hartigan-file24.csv",
                        header = FALSE)

# Find the matrix size
numcol <- ncol(typewriters)
numrow <- nrow(typewriters)

# Normalize datasets with different methods
normalized <- matrix(, nrow = numrow, ncol = 5)

for (i in 2:6) {
  column <- typewriters[,i]
  cmean <- mean(column)
  csd <- sd(column)
  normalized[,i-1] <- ((column - cmean) / csd)
}

normalized2 <- matrix(, nrow = numrow, ncol = 5)
for (i in 1:5) {
  column <- typewriters[, i+1]
  cmax <- max(column)
  cmin <- min(column)
  normalized2[, i] <- (column - cmin) / (cmax - cmin)
}

# Calculate distance matrices with two normalization methods
distance <- as.matrix(dist(normalized, method = "minkowski", p=2))
distance2 <- as.matrix(dist(normalized2, method = "minkowski", p =2))

# Calculate GOF of three distance matrices and compare in plot
GOFs = c()
for (i in 1:5) {
  GOFs[i] <- cmdscale(distance, k = i, eig = TRUE)$GOF[1]
}

GOFs2 = c()
for (i in 1:5) {
  GOFs2[i] <- cmdscale(distance2, k = i, eig = TRUE)$GOF[1]
}

GOFs_original = c()
for (i in 1:5) {
  GOFs_original[i] <- cmdscale(dist(typewriters[,2:6]), k = i, eig = TRUE)$GOF[1]
}
plot((1:5), GOFs2, main = "GOF in three methods", xlab = "Dimension", ylab = "GOF")
points((1:5), GOFs2, col = "red")
points((1:5), GOFs_original, col = "blue")

# Plot the eigenvalue graph
plot(cmdscale(distance, k=1, eig=TRUE)$eig, main = "Eigenvalues")

# 1D Model and comparisons to input distance matrix
cmdscale(distance, k=1, eig=TRUE)
model1 <- cmdscale(distance, k=1, eig = TRUE)
for1Dplot <- data.frame(model1$points,0)
plot(for1Dplot, main = "Distance Plot in 1D", xlab = "Typewriter Models")
MeanAD(distance - as.matrix(dist(for1Dplot)))
plot(as.dist(distance),dist(for1Dplot),asp=1, main = "Distance Difference for 1D Model")
abline(0,1)

# 2D Model and comparisons to input distance matrix
cmdscale(distance, k=2, eig=TRUE)
model2 <- cmdscale(distance, k=2, eig = TRUE)
for2Dplot <- data.frame(cmdscale(distance, k=2), 0)
#plot(model2$points,asp=1, xlim = c(-3, 4), ylim = c(-3,4))
textplot(model2$points[,1],model2$points[,2],
         typewriters[, 1],
         main = "2D MDS model for Typewriter Models",
         asp=1,xlim = c(-3, 4), ylim = c(-3, 4),
         cex=0.6)
MeanAD(distance - as.matrix(dist(for2Dplot)))
plot(as.dist(distance),dist(for2Dplot),asp=1, main = "Distance Difference for 2D Model")
abline(0,1)

# 2D plot analysis
attach(mtcars)
par(mfrow=c(2,3))
# Weight vs x axis, clear negative correlation
plot(typewriters$V5, model2$points[, 1],
     main = "weight vs x axis")
# depth vs x axis, strong negative correlation
plot(typewriters$V4, model2$points[, 1],
     main = "Depth vs x axis")
# height vs x axis, negative correlation
plot(typewriters$V2, model2$points[, 1],
     main = "Height vs x axis")
# width vs x axis, negative correlation
plot(typewriters$V3, model2$points[, 1],
     main = "Width vs x axis")
# Plate length vs y axis, slight and vague negative correlation
plot(typewriters$V6, model2$points[, 2],
     main = "Plate length vs y axis")
# height vs y axis, slight positive correlation
plot(typewriters$V2, model2$points[, 2],
     main = "Height vs y axis")

# Resetting the plot
par(mfrow=c(1,1))
# 3D Model and comparisons to input distance matrix
model3 <- cmdscale(distance, k=3, eig=TRUE)
s3d <- scatterplot3d(model3$points[, 1:3], angle = 60,
                     xlim = c(-4, 4), ylim = c(-4, 4), zlim = c(-4, 4),
                     xlab = "X Axis", ylab = "Y Axis", zlab = "Z Axis")
text(s3d$xyz.convert(model3$points[, 1:3]), labels = typewriters$V1,
     cex= 0.7, col = "steelblue")
for3Dplot <- data.frame(model3$points, 0)
MeanAD(distance - as.matrix(dist(for3Dplot)))
plot(as.dist(distance), dist(for3Dplot), asp=1,
     main = "Distance Difference for 3D Model")
abline(0,1)

# 4D and 5D plot
model4 <- cmdscale(distance, k=4, eig=TRUE)
model5 <- cmdscale(distance, k=5, eig=TRUE)
for4Dplot <- data.frame(model4$points, 0)
for5Dplot <- data.frame(model5$points, 0)
MeanAD(distance - as.matrix(dist(for4Dplot)))
MeanAD(distance - as.matrix(dist(for5Dplot)))
plot(as.dist(distance), dist(for4Dplot), asp=1,
     main = "Distance Difference for 4D Model")
abline(0,1)
plot(as.dist(distance), dist(for5Dplot), asp=1,
     main = "Distance Difference for 5D Model")
abline(0,1)

# histogram
par(mfrow = c(1,2))
hist(distance)
hist(as.matrix(dist(model5$points)),
     main = "Histogram of 5D Model",
     xlab = "5D Model Distance")