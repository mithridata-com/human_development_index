# project on multivariate data analysis
# 2021-2022

# MAIN CODE STARTS HERE
# Let's attach some packages
library(WDI)
library(ggplot2)
library(dplyr)
library(tidyr)
library(factoextra)
library(usethis)
library(devtools)
library(ggbiplot)
library(pracma)
library(dialr)
library(countrycode)
library(NbClust)
library(dendextend)

library(ggrepel)
library(mdatools)
library(ggfortify)
library(stargazer)

options(scipen = 999)
set.seed(1)

# Let's load classic HDI scores for 2019 
url <- "https://raw.githubusercontent.com/mithridata-com/human_development_index/main/Classic_HDI_2019.csv"
classic_HDI <- read.csv(url, sep = ";")
classic_HDI$iso2c <- countryname(classic_HDI $Country, destination = 'iso2c')

# list of indicators, that would be used to develop an index
indicator_list <- c("Liv_LifeExp" = "SP.DYN.LE00.IN",
                    "Liv_DeathRate" = "SP.DYN.CDRT.IN",
                    "Liv_UrbanPopulation" = "SP.URB.TOTL.IN.ZS",
                    "Liv_SlumHouses" = "EN.POP.SLUM.UR.ZS",
                    "Liv_BroadbandInternet" = "IT.NET.BBND.P2",
                    
                    "Health_Immunization" = "SH.IMM.IDPT",
                    "Health_Undernourishment" = "SN.ITK.DEFC.ZS",
                    "Health_BirthBySkilledStaff" = "SH.STA.BRTC.ZS",
                    "Health_HospitalBeds" = "SH.MED.BEDS.ZS",
                    "Health_Under5Mortality" = "SH.DYN.MORT",
                    
                    "Edu_GovernExpend" = "SE.XPD.TOTL.GD.ZS",
                    "Edu_PupilTeacherRatio" = "SE.PRM.ENRL.TC.ZS",
                    "Edu_SchoolEnroll" = "SE.PRM.NENR",
                    "Edu_R&DExpenditure" = "GB.XPD.RSDV.GD.ZS",
                    "Edu_ScientificPublications" = "IP.JRN.ARTC.SC",
                    "Edu_LiteracyRate" = "SE.ADT.LITR.ZS",
                    "Edu_PrimaryCompletionRate" = "SE.PRM.CMPT.ZS",
                    "Edu_TrainTeachersPrimary" = "SE.PRM.TCAQ.ZS",
                    "Edu_ProgressionToSecondary" = "SE.SEC.PROG.MA.ZS",
                    "Edu_ExpectedSchoolYears" = "HD.HCI.EYRS",
                    
                    "Econ_GdpPerCapita" = "NY.GDP.PCAP.CD",
                    #"Econ_GNIPerCapita" = "NY.GNP.PCAP.CD",
                    "Econ_Unemployment" = "SL.UEM.TOTL.ZS",
                    "Econ_Inflation" = "FP.CPI.TOTL.ZG",
                    "Econ_GINI" = "SI.POV.GINI"

)

# download data
data.raw_wdi = WDI(indicator=indicator_list,
               #country=country_codes, 
               start=2019, end=2019)


#this vector will change the direction of variables, so their interpretation can be done easier
# for example, life expectancy is better when it grows, while death rate is better when it decreases
vector_of_impact <- c(1,-1,1,-1,1,
                      1,-1,1,1,-1,
                      1,-1,1,1,1,1,1,1,1,1,
                      #1,-1,-1,-1,1
                      1,-1,-1,-1)

data.raw <- data.raw_wdi
data.raw[,4:ncol(data.raw)] <- t(t(data.raw[,4:ncol(data.raw)]) * vector_of_impact)

# with this we drop all non-country level data items (such as whole World etc)
data.raw <- data.raw[check_cc(data.raw$iso2c) & !(data.raw$iso2c %in% c("TD","CF","IR","AO")),]
data.raw <- data.raw %>% group_by(iso2c, country) %>% filter(year == max(year)) %>% ungroup()

# here we compute basic data quality metrics such as completeness
# higher score means higher quality
data_quality_byIndicator <- data.raw %>%
  gather(key = Indicator, value = Value, -iso2c, -country, -year) %>%
  group_by(Indicator) %>%
  dplyr::summarize(MeanCompleteScore = mean(ifelse(!is.na(Value),1,0))) %>%
  ungroup()

data_quality_byCountry<- data.raw %>%
  gather(key = Indicator, value = Value, -iso2c, -country) %>%
  group_by(country) %>%
  dplyr::summarize(MeanCompleteScore = mean(ifelse(!is.na(Value),1,0))) %>%
  ungroup()

# Let's find columns, that are 1/2 filled
completeCols <- data_quality_byIndicator %>%
  filter(MeanCompleteScore>=0.5) %>% 
  select(Indicator) %>% unlist() %>% unname()


# Let's find columns, that are 1/2 filled
completeRows <- data_quality_byCountry %>%
  filter(MeanCompleteScore>=0.46) %>% 
  select(country) %>% unlist() %>% unname()

# Here we create a subset of dataframe with complete columns, 
# all NAs that are left are filled with mean 
data <- data.raw %>% 
  select(iso2c, country, unlist(completeCols)) %>%
  filter(country %in% completeRows) %>%
  mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))

# first solution. 
# 1. PCA
# 1.1. Ordinary PCA
data.scaled <- scale(data[,3:ncol(data)], center = T, scale = T)
row.names(data.scaled) <- data$country
data.pca <- prcomp(data.scaled, scale = FALSE)
summary(data.pca)
data.pca$rotation[,1:10]
eig.val <- get_eigenvalue(data.pca)
eig.val

# Biplot of individuals and variables
fviz_pca_biplot(data.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969")  # Individuals color

# Graph of individuals. Individuals with a similar profile are grouped together.
fviz_pca_ind(data.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)     # Avoid text overlapping

# Visualize eigenvalues (scree plot). Show the percentage of variances explained by each principal component.
fviz_eig(data.pca)

# Graph of variables. Positive correlated variables point to the same side of the plot. Negative correlated variables point to opposite sides of the graph
fviz_pca_var(data.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)     # Avoid text overlapping

fviz_contrib(data.pca, choice="var", axes = 1, top = 30,
             fill = "lightgray", color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45))     # Avoid text overlapping

# 1.2 Rotated loadings matrix
ncomp <- 5
rawLoadings <- data.pca$rotation[,1:ncomp] %*% diag(data.pca$sdev, ncomp, ncomp)
rotatedLoadings <- varimax(rawLoadings)$loadings
rotatedLoadings

# 1.3 Transform original scaled data to varimaxed 
varimaxed <- varimax(data.pca$rotation[,1:ncomp])
data.varimax <- data.scaled %*% varimaxed$loadings
data.varimax

# PC 1 is responsible for overall wealth, comfort of life
# It increases when we observe:
# weak economical development, low levels of urbanization
# very low levels of broadband internet penetration
# shorter life expectancy

# PC 2 is responsible for overall risks of death 
# It increases when we observe:
# high risks of death

# PC 3 is responsible for labor market condition
# It increases when we observe:
# unemployment grows

# PC 4 is responsible for education level and quality of life
# It increases when we observe:
# Low levels of primary completion rate in schools
# low levels of immunization and shorter life expectancy
# low levels of children mortality and undernourishment

# PC 5 is responsible for inflationary concerns
# It increases when we observe:
# low inflation




invLoadings <- t(pracma::pinv(rotatedLoadings))
scores <- data.scaled %*% invLoadings
scores

# CLUSTERING
d <- dist(scores)

fviz_nbclust(scores, kmeans, method = "wss")+labs(subtitle = "Elbow method")
fviz_nbclust(scores, kmeans, method = "silhouette")+labs(subtitle = "Silhoutte method")
fviz_nbclust(scores, kmeans, nstart = 25, method = "gap_stat", nboot = 50)+labs(subtitle = "Gap statistic method")

fviz_nbclust(scores, hcut, method = "wss")+labs(subtitle = "Elbow method")
fviz_nbclust(scores, hcut, method = "silhouette")+labs(subtitle = "Silhoutte method")
fviz_nbclust(scores, hcut, nstart = 25, method = "gap_stat", nboot = 50)+labs(subtitle = "Gap statistic method")


nb <- NbClust(scores, distance = "minkowski", min.nc = 2,
              max.nc = 10, method = "kmeans")

nb <- NbClust(scores, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans")


# so the optimal number of clusters are 2,3,5 or 7
# we choose 7

par(mfrow = c(1,1))

k.clust <- 7
fit_complete<-hclust(d, method="complete")
plot(fit_complete)
rect.hclust(fit_complete, k=k.clust, border="red")
groups_complete <- cutree(fit_complete, k=k.clust)
data.frame(groups_complete) 

fit_ward<-hclust(d, method="ward.D")
plot(fit_ward, cex = 0.6)
rect.hclust(fit_ward, k=k.clust, border="red")
groups_ward <- cutree(fit_ward, k=k.clust)
data.frame(groups_ward)

#K-means clustering

fit_kmeans<-kmeans(d, k.clust, nstart=10)
fit_kmeans
fviz_cluster(fit_kmeans,d, ellipse.type = "norm", repel = TRUE)
fit_kmeans$centers

groups_kmeans <- fit_kmeans$cluster
# # Create three dendrograms
# dend_complete <- as.dendrogram (fit_complete)
# dend_centroid <- as.dendrogram (fit_centroid)
# dend_single <- as.dendrogram (fit_single)
# dend_ward <- as.dendrogram (fit_ward)
# # Create a list to hold dendrograms
# dend_list <- dendlist(dend_complete, dend_centroid, dend_single,dend_ward)
# dend_list
#tanglegram(dend_complete, dend_centroid)

# cor.dendlist(dend_list, method = "cophenetic")
# cor.dendlist(dend_list, method = "baker")
# so now we know that four methods provide statisticaly different results

scores.df <- data.frame(scores) %>% mutate(cluster = factor(groups_ward))
scores.df$iso2c <- countryname(row.names(scores.df), destination = 'iso2c')
colnames(scores.df) <- c(paste0("PC_",seq(1,ncomp,by = 1)),"CLUSTER_NUM", "iso2c")

scores.clust <- scores.df %>% 
  group_by(CLUSTER_NUM) %>% 
  dplyr::summarise(PC_1 = round(mean(PC_1),3),
                   PC_2 = round(mean(PC_2),3),
                   PC_3 = round(mean(PC_3),3),
                   PC_4 = round(mean(PC_4),3),
                   PC_5 = round(mean(PC_5),3))

scores.clust
# test if cluster is important
summary(aov(PC_1 ~ CLUSTER_NUM,data = scores.df))


# Let's try to make an index of all PC scores, weighted by some values
scale_to_a_range <- function(x){
  (x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE))
}

scores.weights <- (eig.val[1:ncomp,]$variance.percent) #bad 
#scores.weights2 <- sqrt(eig.val[1:ncomp,]$variance.percent) # worse
#scores.weights3 <- abs((seq(1, ncomp, by = 1)/ncomp-1-1/ncomp)*ncomp) # terrible
#scores.weights4 <- (eig.val[1:ncomp,]$eigenvalue) #bad 


# let's join our data with classic HDI
data.HDI <- scores.df %>% 
  left_join(classic_HDI, by = ("iso2c")) %>%
  mutate(UN_HDI = HDI) %>% select(-HDI) %>%
  mutate(Alternative_HDI = scale_to_a_range((-1*scores) %*% scores.weights)) %>%
  mutate(PC_1_size = scale_to_a_range(-1*PC_1)^exp(1)*10)

corHDI <- round(cor(data.HDI$Alternative_HDI, data.HDI$UN_HDI)*100,1)
corHDI

p1 <- ggplot(data = data.HDI) + theme_bw()
p1 <- p1 + geom_point(aes(x = UN_HDI,
                          y = Alternative_HDI, 
                          #size = PC_1,
                          #shape = factor(sign(PC_1)),
                          color = factor(CLUSTER_NUM)), alpha = 0.75)

p1 <- p1 + geom_smooth(aes(x = UN_HDI,
                           y = Alternative_HDI),
                       method = "glm",
                       se = F, 
                       color = "darkgrey",linetype = "dashed", alpha = 0.75)

p1 <- p1 + labs(x = "United Nations HDI",
                y = "Alternative HDI",
                title = paste0("UN HDI vs alternative HDI (based on PC)"),
                subtitle = paste0("Correlation between two HDIs = ",corHDI,"%"),
                size = "PC1",
                shape = "Sign of PC1",
                color = "Cluster #")
p1 <- p1 + theme(legend.position = c(0.85, 0.13)) + guides(color=guide_legend(nrow=2,byrow=TRUE))
#p1 <- p1 + theme(legend.position="bottom")
p1


### PLOT 2 
angle <- function(x,y){
  dot.prod <- x%*%y 
  norm.x <- norm(x,type="2")
  norm.y <- norm(y,type="2")
  theta <- acos(dot.prod / (norm.x * norm.y))
  as.numeric(theta)
}


PC1.var <- round(eig.val$variance.percent[1],0)
PC2.var <- round(eig.val$variance.percent[2],0)

p2 <- ggplot(data = data.HDI) + theme_bw()
p2 <- p2 + geom_point(aes(x = PC_1,
                          y = PC_2,
                          color = UN_HDI))
                                           
p2 <- p2 + scale_colour_gradient(low = "green", 
                                 high = "red",
                                 space = "Lab",
                                 na.value = "grey50",  
                                 guide = "colourbar",  
                                 aesthetics = "colour")

p2 <- p2 + labs(x = paste0("PC 1 (", PC1.var,"% of variance)"),
                y = paste0("PC 2 (", PC2.var,"% of variance)"),
                title = paste0("PC 1 vs PC 2, coloured by UN HDI"),
                color = "UN HDI")
p2 <- p2 + geom_vline(xintercept = 0)
p2 <- p2 + geom_hline(yintercept = 0)
p2 <- p2 + geom_label_repel(aes(x = PC_1,
                            y = PC_2, label = Country),
                            label.size = NA, 
                            fill = NA,
                            size=3,
                            max.overlaps = 4, 
                            min.segment.length=0, 
                            box.padding=0.5, 
                            label.padding=0, 
                            point.padding=0)
p2


####PLOT 3
PC1.var <- round(eig.val$variance.percent[1],0)
PC2.var <- round(eig.val$variance.percent[2],0)

p3 <- ggplot(data = data.HDI) + theme_bw()
p3 <- p3 + geom_text(aes(x = PC_1,
                        y = PC_2,
                        label = iso2c,
                        color = factor(CLUSTER_NUM)))


p3 <- p3 + labs(x = paste0("PC 1 (", PC1.var,"% of variance)"),
                y = paste0("PC 2 (", PC2.var,"% of variance)"),
                title = paste0("PC 1 vs PC 2 by cluster"),
                color = "Cluster")
p3 <- p3 + geom_vline(xintercept = 0)
p3 <- p3 + geom_hline(yintercept = 0)
#p3 <- p3 + geom_label_repel(aes(x = PC_1,
                            #     y = PC_2, label = iso2c),
                            # label.size = NA, 
                            # fill = NA,
                            # size=3,
                            # max.overlaps = 8, 
                            # min.segment.length=0, 
                            # box.padding=0.5, 
                            # label.padding=0, 
                            # point.padding=0)
p3

#### PLOT 4 
var_explained_df <- data.frame(PC_NUM= 1:11,
                               PC= paste0("PC",1:11),
                               var_explained=(data.pca$sdev)^2/sum((data.pca$sdev)^2))

p4 <- ggplot(data = var_explained_df) + theme_bw()
p4 <- p4 + geom_col(aes(x = PC_NUM, y = var_explained), fill= rgb(190,214,47, maxColorValue = 255))
p4 <- p4 + geom_line(aes(x = PC_NUM, y = var_explained, group=1))+
  geom_point(aes(x = PC_NUM, y = var_explained, group=1), size=4)
p4 <- p4 + labs(x = "Principal component",
                y = "Variance explanied (%)",
                title = paste0("Scree plot: PCA on scaled data"))
p4 <- p4 + geom_label_repel(aes(x = PC_NUM,
                                y = var_explained, label = paste0(round(var_explained*100,0),"%")),
                                label.size = NA,
                                fill = NA,
                                size=3,
                                max.overlaps = 8,
                                min.segment.length=0,
                                box.padding=0.5,
                                label.padding=0,
                                point.padding=0)
p4 <- p4 + scale_x_continuous(labels=as.character(var_explained_df$PC_NUM),breaks=var_explained_df$PC_NUM)
p4


#### PLOT 5 and 6
data.pca2 <- pca(data.scaled, center = F, scale = F)
plot(data.pca2, show.labels = T)


autoplot(data.pca, data = data.HDI, colour = 'PC_3',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)


par(mfrow = c(1, 2))

plotScores(data.pca2, c(1, 2), 
           main = "PC1 vs PC2 Scores (colour by UN HDI)",
           show.labels = F, 
           cgroup = data.HDI$UN_HDI, 
           labels = data.HDI$CLUSTER_NUM)

mdaplot(data.pca2$loadings, type = 'p', #p
        main = "Loadings",
        col = rgb(190,214,47, maxColorValue = 255),
        lab.col = rgb(92,102,108, maxColorValue = 255),
        show.labels = T, 
        show.lines = c(0, 0))

getwd()
### PLOT 7
par(mfrow = c(1,1))
stargazer(scores.clust,
          summary = F,
          rownames = F,
          align=T, 
          digits=2, digits.extra = 2,
          column.labels = c("#", "PC1","PC2","PC3","PC4","PC5"),
          covariate.labels = c("#", "PC1","PC2","PC3","PC4","PC5"),
          out="Clusters.html", 
          type = "html",
          title="Clusters' mean of principal components")


### PLOT 8
p8 <- ggplot(data.HDI) + theme_bw()
p8 <- p8 + geom_boxplot(outlier.shape = NA, coef = 2,
                        aes(x = reorder(CLUSTER_NUM, Alternative_HDI), 
                            y = Alternative_HDI, fill = CLUSTER_NUM), show.legend = F)
p8 <- p8 + geom_jitter(aes(x = reorder(CLUSTER_NUM, Alternative_HDI), 
                       y = Alternative_HDI),width = 0.2)
p8 <- p8 + labs(x = paste0("Cluster#"),
                y = paste0("Value of alternative HDI"),
                title = paste0("Boxplots of alternative HDI by cluster"),
                fill = "Cluster#")
p8

data.export <- data.HDI %>% 
  select(Country, Alternative_HDI, CLUSTER_NUM) %>%
  mutate(Alternative_HDI = round((Alternative_HDI+0.01)*100,0))
write.table(data.export, "data_export.csv", sep = ";", dec = ",", row.names = F)


# #factor analysis
# library(psych)
# library(stats)
# 
# Y=cor(data.scaled)
# 
# KMO(r=cor(data.scaled))
# # Liv_DeathRate is not very adequate for FA
# det(cor(Y))
# cortest.bartlett(Y)
# #we can proceed but dat ais not perfect for FA
# 
# covmat <- cor(data.scaled)
# 
# fit <- factanal(data.scaled[,c(1:12)], factors=4, 
#                 rotation="varimax",
#                 control = list(nstart = 3, trace = T))
# fit 
# 
# #checking residuals
# Lambda <- fit$loadings
# Psi <- diag(fit$uniquenesses)
# S <- fit$correlation
# Sigma <- Lambda %*% t(Lambda) + Psi
# round(S - Sigma, 3)
# 
# # print results
# 
# par(mfrow = c(1,3))
# 
# plot(fit$loadings[,1], 
#      fit$loadings[,2],
#      xlab = "Factor 1", 
#      ylab = "Factor 2", 
#      ylim = c(-1,1),
#      xlim = c(-1,1),
#      main = "Varimax rotation")
# text(fit$loadings[,1]-0.08, 
#      fit$loadings[,2]+0.08,
#      col="blue")
# abline(h = 0, v = 0)
# 
# plot(fit$loadings[,1], 
#      fit$loadings[,3],
#      xlab = "Factor 1", 
#      ylab = "Factor 3", 
#      ylim = c(-1,1),
#      xlim = c(-1,1),
#      main = "Varimax rotation")
# text(fit$loadings[,1]-0.08, 
#      fit$loadings[,3]+0.08,
#      col="blue")
# abline(h = 0, v = 0)
# 
# plot(fit$loadings[,2], 
#      fit$loadings[,3],
#      xlab = "Factor 1", 
#      ylab = "Factor 3", 
#      ylim = c(-1,1),
#      xlim = c(-1,1),
#      main = "Varimax rotation")
# text(fit$loadings[,2]-0.08, 
#      fit$loadings[,3]+0.08,
#      col="blue")
# abline(h = 0, v = 0)

