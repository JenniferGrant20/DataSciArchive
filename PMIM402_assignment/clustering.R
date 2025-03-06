## PMIM402J Script 1
## Clustering
## Student no. 2005070
## Date: 02/07/24
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse) # For code syntax
library(mice) # For imputation
options(repr.plot.width=20, repr.plot.height=10)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Data preparation / preprocessing

# Loading the data
data <- read.csv('heart-c.csv')

# Removing the classifier variable
heartdisease <- data %>% select(-num)

# Recoding character variables into numeric
heartdisease$sex <- recode(heartdisease$sex, 
                           "male" = 1, 
                           "female" = 2)
heartdisease$cp <- recode(heartdisease$cp, 
                          "typ_angina" = 1,
                          "atyp_angina" = 2, 
                          "non_anginal" = 3, 
                          "asympt" = 4)
heartdisease$fbs <- recode(heartdisease$fbs, 
                           "f" = 0, 
                           "t" = 1)
heartdisease$restecg <- recode(heartdisease$restecg, 
                               "normal" = 1, 
                               "st_t_wave_abnormality" = 2,
                               "left_vent_hyper" = 3)
heartdisease$exang <- recode(heartdisease$exang, 
                             "no" = 0, 
                             "yes" = 1)
heartdisease$slope <- recode(heartdisease$slope, 
                             "up" = 1,
                             "flat" = 2, 
                             "down" = 3)
heartdisease$thal <- recode(heartdisease$thal, 
                            "normal" = 3, 
                            "fixed_defect" = 6,
                            "reversable_defect" = 7)

# Imputation
md.pattern(heartdisease) # Looking at the missing data
imputedata <- heartdisease
imputedata <- mice(heartdisease, m=5, method='pmm')
imputedata <- complete(imputedata)
summary(imputedata) # Checking the data looks okay after imputation
md.pattern(imputedata)

# Scaling the data
hd.scaled <- scale(imputedata) %>% as.data.frame()

# Plotting histograms
hd.scaled %>% gather(attributes, value, 1:ncol(hd.scaled)) %>%
  ggplot(aes(x=value)) +
  geom_histogram(fill="lightblue", colour = "black") +
  facet_wrap(~attributes, scales = "free")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# K-means

cl <- kmeans(hd.scaled,2)
cl

# Checking the error at different K values and creating an elbow plot
error <- vector()
for (i in 1:10){
  error[i] <- kmeans(hd.scaled,i)$tot.withinss
}

plot(error,type="l")

# Repeating with smaller increments to create a better elbow plot
error <- vector()
for (i in seq(1, 3, 0.25)){
  error[i] <- kmeans(hd.scaled,i)$tot.withinss
}

plot(error,type="l")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Hierarchical clustering

set.seed(200)
hd.dist <- dist(hd.scaled)
cluster.wrd <- hclust(d = hd.dist, method = 'ward.D2')

# Plotting the dendrogram
plot(cluster.wrd, labels = FALSE)

# Cutting it at k = 2 and plotting coloured lines to show the decided clusters
cut.wrd <- cutree(cluster.wrd, k = 2)
plot(cluster.wrd, labels = FALSE)
rect.hclust(cluster.wrd, k = 2, border = 2:6)

# Using a barplot to check the optimal K value was chosen
barplot(cluster.wrd$height,
        names.arg = (nrow(hd.scaled) - 1):1,
        xlab = "Number of Clusters",
        ylab = "Height") # show the number of clusters
