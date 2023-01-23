### Finding Patterns in Data ####

### Packages ####

library(dplyr)
library(ggdendro)
library(ggplot2)
library(reshape2)
library(vegan)

### Data ####

animals <- read.csv("/animals.csv")
sheeps <- read.csv("/sheeps.csv")

### Sample Size ####

### Plotting distribution ###
ggplot(sheeps, aes(x=value, fill=id)) +
  geom_density(alpha=0.5)

### Sub-sampling the dataset ###
sample500 <- sample_n(sheeps, 500)
sample50 <- sample_n(sample500, 50)
sample10 <- sample_n(sample50, 10)

### Turning sub-samples into dataframes ###
sheeps$sampleSize <- 1000
sample500$sampleSize <- 500
sample50$sampleSize <- 50
sample10$sampleSize <- 10
samples <- rbind(sheeps, sample500, sample50, sample10)

### Plotting the sub-samples ###
ggplot(samples, aes(x=value, fill=id)) + 
  geom_density(alpha=0.5) + 
  facet_grid(~sampleSize)

#### Simpson's Index ####

### Creating a Presence/Absence Matrix ###
animalsMatrix <- acast(animals, id~species, value.var="values")
animalsMatrix

### Calculating Simpson's Index ###
diversity(animalsMatrix, index="simpson")

#### Jaccard's Similarity Coefficient ####

### Plotting species at each site ###
ggplot(animals, aes(x=id, y=values, fill=species)) +
  geom_bar(stat="identity") +
  facet_wrap(~period, scales="free_x")

### Calculating Jaccard's Similarity Coefficient ###
vegdist(animalsMatrix, method="jaccard", binary=TRUE)

#### Morisita-Horn Overlap Coefficient ####

### Calculating the Morisita-Horn Overlap Coefficient ###
zooDistance <- vegdist(animalsMatrix, method="horn")
zooDistance

#### Visaualising Pairwise Similarities ####

### Converting data into a non-tidy format ###
zooDistanceMatrix <- as.matrix(zooDistance)
zoo <- melt(zooDistanceMatrix, varnames=c("site1", "site2"), value.name="distance")

### Plotting the Morisita-Horn Overlap Coefficient ###
ggplot(zoo, aes(x=site1, y=site2, fill=distance)) +
  geom_raster()

### Adding Values ###
ggplot(zoo, aes(x=site1, y=site2, fill=distance, label=round(distance, 2))) +
  geom_raster() +
  geom_text(color="white")

### Plotting as a dendrogram ###
morisitaHClust <- hclust(zooDistance, method="average")
ggdendrogram(morisitaHClust, rotate=T) +
  labs(title="Animal Similarity Between Sites Using Morisita-Horn Overlap Coefficient", x="", y="Distance between assemblages") +
  theme_minimal()


