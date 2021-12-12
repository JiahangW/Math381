library(dplyr)
library(wordcloud)
library(scatterplot3d)
library(DescTools)

standing <- read.csv(
  "https://raw.githubusercontent.com/JiahangW/Math381/main/table.csv"
  )
team_stats <- read.csv(
  "https://raw.githubusercontent.com/JiahangW/Math381/main/teams.csv"
  )
all_data <- standing %>% 
  left_join(team_stats, by = "Squad")

numcol <- ncol(all_data)
numrow <- nrow(all_data)

normalized <- matrix(, nrow = numrow, ncol = numcol - 7)

for (i in 8:(numcol)){
  column <- all_data[,i]
  cmean <- mean(column)
  csd <- sd(column)
  normalized[,i - 7] <- ((column - cmean) / csd)
}

distance <- as.matrix(dist(normalized, method = "minkowski", p=2))

plot(cmdscale(distance, k=1, eig=TRUE)$eig, main = "Eigenvalues",
     xlab = "Dimension",ylab = "Eigenvalues")
GOFs = c()
for (i in 1:10) {
  GOFs[i] <- cmdscale(distance, k = i, eig = TRUE)$GOF[1]
}
plot((1:10), GOFs, ylim = c(0,1))


cmdscale(distance, k=2, eig=TRUE)
model2 <- cmdscale(distance, k=2, eig = TRUE)
for2Dplot <- data.frame(cmdscale(distance, k=2), 0)
textplot(model2$points[,1],model2$points[,2],
         all_data[, 2],
         main = "2D MDS model for Premier League Teams",
         asp=1, xlim = c(-6, 4), ylim = c(-5, 5),
         cex=0.6)
print(model2$GOF[1])
MeanAD(distance - as.matrix(dist(for2Dplot)))
plot(as.dist(distance),dist(for2Dplot),asp=1,
     main = "Distance Difference for 2D Model")
abline(0,1)


attach(mtcars)
par(mfrow=c(2,1))
plot(all_data$Shots, model2$points[, 1],
     main = "shots vs x axis")
plot(all_data$Tackle, model2$points[, 2],
     main = "tackles vs y axis")

par(mfrow=c(1,1))
plot(all_data$Rk, model2$points[, 1],
     main = "ranking vs x axis")

plot(all_data$GA, all_data$Tackle,
     main = "goals conceded vs tackles",
     xlab = "Goals Conceded",
     ylab = "Tackles")

par(mfrow=c(2,2))
plot(all_data$Age, model2$points[, 1],
     main = "age vs x axis")
plot(all_data$Age, model2$points[, 2],
     main = "age vs y axis")
plot(all_data$NumPlayers, model2$points[, 1],
     main = "number of players vs x axis")
plot(all_data$NumPlayers, model2$points[, 2],
     main = "number of players vs y axis")

