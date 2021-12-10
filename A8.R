library(dplyr)
library(wordcloud)
library(scatterplot3d)
library(DescTools)

standing <- read.csv("./table.csv")
team_stats <- read.csv("teams.csv")
all_data <- standing %>% 
  left_join(team_stats, by = "Squad")

numcol <- ncol(all_data)
numrow <- nrow(all_data)

normalized <- matrix(, nrow = numrow, ncol = numcol - 7)
#normalized <- matrix(, nrow = numrow, ncol = 2)

goals_for <- standing$GF
goals_against <- standing$GA
plot(goals_against, goals_for)

for (i in 1:(numcol - 7)){
  column <- all_data[,i + 6]
  cmean <- mean(column)
  csd <- sd(column)
  normalized[,i] <- ((column - cmean) / csd)
}

distance <- as.matrix(dist(normalized, method = "minkowski", p=2))

plot(cmdscale(distance, k=1, eig=TRUE)$eig, main = "Eigenvalues")
GOFs = c()
for (i in 1:10) {
  GOFs[i] <- cmdscale(distance, k = i, eig = TRUE)$GOF[1]
}
plot((1:10), GOFs, ylim = c(0,1))

cmdscale(distance, k=1, eig=TRUE)
model1 <- cmdscale(distance, k=1, eig = TRUE)
for1Dplot <- data.frame(model1$points,0)
plot(for1Dplot)

cmdscale(distance, k=2, eig=TRUE)
model2 <- cmdscale(distance, k=2, eig = TRUE)
for2Dplot <- data.frame(cmdscale(distance, k=2), 0)
textplot(model2$points[,1],model2$points[,2],
         all_data[, 2],
         main = "2D MDS model for Premier League Teams",
         asp=1, #xlim = c(-8, 8), ylim = c(-8, 8),
         cex=0.6)
MeanAD(distance - as.matrix(dist(for2Dplot)))
plot(as.dist(distance),dist(for2Dplot),asp=1, main = "Distance Difference for 2D Model")
abline(0,1)


model3 <- cmdscale(distance, k=3, eig = TRUE)
for3Dplot <- data.frame(model3$points, 0)
MeanAD(distance - as.matrix(dist(for3Dplot)))
plot(as.dist(distance), dist(for3Dplot), asp=1,
     main = "Distance Difference for 3D Model")
abline(0,1)

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