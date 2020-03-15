library(visdat)
library(ggplot2)
#install.packages("ggplot")
library(tidyverse)
#remove.packages("dplyr")
#install.packages("dply")
library(dplyr)
#install.packages("naniar")
library(naniar)
#install.packages("kableExtra")
#install.packages("hrbrthemes")
#install.packages("patchwork")
library(hrbrthemes)
library(kableExtra)
options(knitr.table.format = "html")
library(patchwork)
library(forcats)





# Data Preprocessing 
# importing the dataset
getwd()
setwd("C:\\Users\\Vero\\Desktop\\UJM\\M1S2\\Data mineing and knowledge discovery\\Project")
dataset = read.csv("Chemicals.csv", header=T, na.strings=c("","NA"))
ogdataset = read.csv("Chemicals.csv", header=T, na.strings=c("","NA"))
dim(dataset)
# Dataset has about 112925 rows and 22 columns 
head(dataset)# prints the last part of the data 
summary(dataset)



# visualizing missing data
gg_miss_var(dataset)

# Cleaninng the data 
#from this graph we see that 1.4% of the data is missing which
#comes from the CSFId column which is basically the CDPH internal i
#dentification number for a color/scent/flavor.
# since about 30% of the data inn csfid is missing and it conveys almost the same information as csf i see the best way 
# to move foward with this analysis is to drop the data  and in addition all primary Keys 

dataset = select(dataset,-c(DiscontinuedDate, ChemicalDateRemoved,
                            CasNumber,PrimaryCategoryId,CompanyId,
                            ChemicalId,CDPHId, CasId,CSFId, SubCategoryId, PrimaryCategoryId  ))
gg_miss_var(dataset)

# Changing missing values in CSF to "Other"
dataset$CSF[is.na(dataset$CSF)]="None"

#Revisualizing the missing data
vis_miss(dataset, 'warn_large_data' = FALSE )



# visulizing the missing data in Brand name field
# rows with missing values for brand name 
missing_brands = dataset[!complete.cases(dataset$BrandName), ]
par(las=4)
counts <- table(missing_brands$CompanyName)
Bar_chart = barplot(counts, main="Missing brand Names",
                    xlab="Company Name ", horiz=FALSE, angle = 45) 



# Now that we know the exrent of the missing data, we will
# will fill in the missing brand for the companies manualy with the highest freqiences 
dataset$CSF[is.na(dataset$CSF)]="None"
data.frame(missing_brands)
# Taking a subset of the data with missing brands to know if the same company can have different brands
#From the look of this table it appears that a company has different brands for each product and since no
# product seems to be repeated, it is impossible to know which brands they fall under
# because of this findig I deem it best to drop all rows with missing brand  as well as other data from different columns 
# from the dataset 
c1 = filter(dataset, dataset$CompanyName =="Greenbrier International, Inc.")     


#Dropping rows with missing rows 
chemicals = dataset
chemicals = chemicals[-c(is.nan(chemicals))]
chemicals<-chemicals[!(chemicals$BrandName=="NA" | chemicals$BrandName=="None")|
                       chemicals$CompanyName=="NA" | chemicals$PrimaryCategory=="NA"| chemicals$SubCategory=="NA", ]
vis_miss(chemicals, 'warn_large_data' = FALSE )



# At this point we have dealt with the missing data so Lets move on to building a model

#Exploring the dataset . 

# Top 20 companies in the dataset by product  
#Creating a Frequency Table
company.freq <- table(chemicals$CompanyName)
company.freq = data.frame(company.freq)
data <- company.freq

# Plot
data %>%
  filter(!is.na(Freq)) %>%
  arrange(Freq) %>%
  tail(20) %>%
  mutate(Company=factor(Var1,levels = Var1)) %>%
  ggplot( aes(x=Var1, y=Freq) ) +
  geom_segment( aes(x=Var1 ,xend=Var1, y=0, yend=Freq), color="grey") +
  geom_point(size=3, color="#69b3a2") +
  coord_flip() +
  theme_ipsum() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none"
  ) +
  xlab("") +
  ylab("Frequency of company by amount of products ")


# Most popular brands by product 
# Top 20 most popular chemicals  
chemical.freq <- table(chemicals$ChemicalName)
chemical.freq = data.frame(chemical.freq)
chemical.freq$Freq = log10(chemical.freq$Freq)
data = chemical.freq
# Plot
data %>%
  filter(!is.na(Freq)) %>%
  arrange(Freq) %>%
  tail(20) %>%
  mutate(Company=factor(Var1,levels = Var1)) %>%
  ggplot( aes(x=Var1, y=Freq) ) +
  geom_segment( aes(x=Var1 ,xend=Var1, y=0, yend=Freq), color="grey") +
  geom_point(size=3, color="#69b3a2") +
  coord_flip() +
  theme_ipsum() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none"
  ) +
  xlab("") +
  ylab(" log count Frequency of chemicals by amount of products ")





#Brands with the highest number of recalls , Making a circle barplot
# Sub categories with the gighest chemical count 
recall <- ogdataset[,c("PrimaryCategory", "DiscontinuedDate")]
recall <- na.omit(recall) 
recall.freq = table(recall$PrimaryCategory)
recall.freq = data.frame(recall.freq)

# Librares
data = recall.freq

# Libraries
library(tidyverse)
library(hrbrthemes)
library(kableExtra)
options(knitr.table.format = "html")
library(viridis)

# Order data
tmp <- data %>%
  filter(!is.na(Freq)) %>%
  arrange(desc(Freq)) %>%
  mutate(PrimaryCategory=factor(Var1, Var1))

# Set a number of 'empty bar'
empty_bar=3

# Add lines to the initial tmpset
to_add = matrix(NA, empty_bar, ncol(tmp))
colnames(to_add) = colnames(tmp)
tmp=rbind(tmp, to_add)
tmp$id=seq(1, nrow(tmp))

# Get the name and the y position of each label
label_tmp=tmp
number_of_bar=nrow(label_tmp)
angle= 90 - 360 * (label_tmp$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_tmp$hjust<-ifelse( angle < -90, 1, 0)
label_tmp$angle<-ifelse(angle < -90, angle+180, angle)
label_tmp$Var1 <- gsub("Baby Products", "Baby", label_tmp$Var1)
label_tmp$Var1 <- gsub("Bath Products", "Bath", label_tmp$Var1)
label_tmp$Var1 <- gsub("Hair Care Products (non-coloring)", "Hair Care", label_tmp$Var1)
label_tmp$Var1 <- gsub("Makeup Products", "Makeup", label_tmp$Var1)
label_tmp$Var1 <- gsub("Tattoos and Permanent Makeup", "Permanent Makeup", label_tmp$Var1)
label_tmp$Var1 <- paste(label_tmp$Var1, "(", label_tmp$Freq,")", sep="")

# Make the plot
ggplot(tmp, aes(x=as.factor(id), y=Freq)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", fill=alpha("#69b3a2", 0.9)) +
  ylim(-7000,13000) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar(start = 0) + 
  geom_text(data=label_tmp, aes(x=id, y=Freq+400, label=Var1 ), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_tmp$angle, hjust=label_tmp$hjust, inherit.aes = FALSE ) +
  geom_text( aes(x=10, y=8500, label="Category with the most Recals"), color="black", inherit.aes = FALSE)



#NOW WE FINALLY MOVE ON TO heirachical clustering 

datasetF = subset(chemicals, select = -c(MostRecentDateReported, ChemicalCreatedAt,ChemicalUpdatedAt, InitialDateReported))
datasetFl = subset(datasetF, PrimaryCategory=="Skin Care Products " & datasetF$ChemicalCount >=2)

#-Dissimilarity Matrix
library(cluster) 
gower.dist <- daisy(datasetFl[ ,1:8], metric = c("gower"))


# AGGLOMERATIVE CLUSTERING 

aggl.clust.c <- hclust(gower.dist, method = "complete")
plot(aggl.clust.c,
     main = "Agglomerative, complete linkages")


install.packages("fpc")
library(fpc)

#________ Identifying best cluster ______________

#Using the elbow  method to identify the best cluster. From the look of the graph it is clear that the initial guess of 5 was quite off and 
# a k=8 number of clusters would be more apporiate 
ggplot(data = data.frame(t(cstats.table(gower.dist, aggl.clust.c, 15))), 
       aes(x=cluster.number, y=within.cluster.ss)) + 
  geom_point()+
  geom_line()+
  ggtitle("Agglomerative clustering") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5))



# CONCLUSIONS
# To close from our results we have been able to identify the categories with the most recalls. 
# This can potentially point  to the categories which are more regulated than others. This decision can
# help a consumer know which type of products they should be more aware of when shopping. In addition we also 
# identified the most popular chemicals in product. This graph is of significance because it helps a consumer understand 
# the causation of certain reactions on their skin.  In addition we are able to also identify the companies with the most
# products which can help a consumer identify brands that give them more options when it comes to skin care. While at the
# moment it is not quite apparent what the possible clusters we found could mean , In the future when analyzed by someone 
# with domain knowledge this could possibly be able to explain a significant trend in the skin care data. 



#ACKNOWELEDGEMENTS
# 1.	Schneider, Günther et al (2005). "Skin Cosmetics" in Ullmann's Encyclopedia of Industrial Chemistry, Wiley-VCH, Weinheim. doi:10.1002/14356007.a24_219
# 2.	https://github.com/yanfei-wu/chemical_cosmetic/blob/master/notebook/chemical_in_beauty_products.ipynb
# 3.	Hierarchical clustering tutorial (data preparation, clustering, visualization), overall, this blog might be useful for someone interested in business analytics with R: http://uc-r.github.io/hc_clustering and https://uc-r.github.io/kmeans_clustering
# 4.	Cluster validation: http://www.sthda.com/english/articles/29-cluster-validation-essentials/97-cluster-validation-statistics-must-know-methods/
# 5.	An example of document categorization (hierarchical and k-means): https://eight2late.wordpress.com/2015/07/22/a-gentle-introduction-to-cluster-analysis-using-r/
# 6.	Hierarchical Clustering on Categorical Data in R, https://towardsdatascience.com/hierarchical-clustering-on-categorical-data-in-r-a27e578f2995
# 7.	The graph gallery, https://www.r-graph-gallery.com/



















