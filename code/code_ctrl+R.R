
# load soc.ca package for MCA
# install.packages("soc.ca")
library(soc.ca)

# get the help file documentation for the package
?soc.ca

# load data
data(taste)
names(taste)

# create a data frame of factors containing all the active variables (+ remove supplementary individuals)
taste          <- taste[which(taste$Isup == 'Active'), ]

attach(taste)
active         <- data.frame(TV, Film, Art, Eat)
sup            <- data.frame(Gender, Age, Income)
detach(taste)

# alternatively, you may prefer using the tidyverse
library(tidyverse)
active <- taste %>% select(TV, Film, Art, Eat)
sup <- taste %>% select(Gender, Age, Income)

# run the analysis ----
result         <- soc.mca(active, sup)

# inspect the results ----
result

variance(result)
contribution(result, dim = 1)
contribution(result, dim = 2)

# details for all modalities
contribution(result, 1:2, mode = "variable")

# plot the results ----
# get information on functions
?map.active
map.active(
  result,
  dim = c(1, 2),
  point.shape = "variable",
  point.alpha = 0.8,
  point.fill = "whitesmoke",
  point.color = "black",
  point.size = "freq",
  label = TRUE,
  label.repel = FALSE,
  label.alpha = 0.8,
  label.color = "black",
  label.size = 4,
  label.fill = NULL,
  map.title = "active",
  labelx = "default",
  labely = "default",
  legend = NULL
) + xlim(-1.4,1.4) + ylim(-1.4,1.4)

# map contributive modalities
?map.ctr
map.ctr(
  result,
  dim = c(1, 2),
  ctr.dim = 1,
  point.shape = "variable",
  point.alpha = 0.8,
  point.fill = "whitesmoke",
  point.color = "black",
  point.size = "freq",
  label = TRUE,
  label.repel = TRUE,
  label.alpha = 0.8,
  label.color = "black",
  label.size = 4,
  label.fill = NULL,
  map.title = "ctr",
  labelx = "default",
  labely = "default",
  legend = NULL
) + xlim(-1.4,1.4) + ylim(-1.4,1.4)

# map supplementary variable: gender
str(sup)
result$names.sup

?map.sup
map.sup(
  result,
  dim = c(1, 2),
  point.shape = "variable",
  point.alpha = 0.8,
  point.fill = "whitesmoke",
  point.color = "black",
  point.size = "freq",
  label = TRUE,
  label.repel = TRUE,
  label.alpha = 0.8,
  label.color = "black",
  label.size = 4,
  label.fill = NULL,
  map.title = "sup",
  labelx = "default",
  labely = "default",
  legend = NULL
) + xlim(-1.4,1.4) + ylim(-1.4,1.4)

# map the cloud of individuals
map <- map.ind(result, point.color = "black", point.size = 1.5, map.title = "")
map + xlim(-1.75,1.75) + ylim(-1.75,1.75)

# clustering ----
set.seed(123)
library(factoextra)

# retrieve coordinates of individuals on the three first dimensions
coords <- result$coord.ind[, 1:3]

# optimal number of clusters
nbcluters <- fviz_nbclust(coords, kmeans, method = "silhouette") # wss
nbcluters$data
nbcluters

# run k-means clustering
kmeanclust <- kmeans(coords, 4, nstart = 25)
table(kmeanclust$cluster)
taste$kmeanclust <- as.factor(kmeanclust$cluster)
table(taste$kmeanclust)

# plot clusters (1)
plot <- map.ind(result, point.fill = as.factor(taste$kmeanclust), point.size = 1)
palette <- c("coral2", "deepskyblue3", "green", "grey20")
plot + scale_fill_manual(values=palette) + xlim(-1.75,1.75) + ylim(-1.75,1.75)
plot

# plot clusters (2) with concentration ellipses
plot <- map.ellipse(result, map, taste$kmeanclust, draw.levels = 1:nlevels(taste$kmeanclust), label.size = 5)
palette <- c("coral2", "deepskyblue3", "green", "grey20")
plot + scale_fill_manual(values=palette) + xlim(-1.75,1.75) + ylim(-1.75,1.75) + labs(x = "Dimension 1 (63.5%)", y = "Dimension 2 (16.9%)")
plot

# inspect the distribution of the modalities in the clusters
# we look for over / under-representations of the modalities. 
# for furher information on the method, see FactoMineR documentation. 
# see also: Husson, F., Lê, S., & Pagès, J. (2017). Exploratory multivariate analysis by example using R. Boca Raton: CRC press.
library(FactoMineR)
names(taste)
catdes(taste, 10, proba = 0.05) # clusters

# alternatively, univariate analysis for gender
catdes(taste, 7, proba = 0.05) # gender

