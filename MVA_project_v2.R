# project on multivariate data analysis
# Professor Jorge Mendes
# 2021

# MAIN CODE STARTS HERE
# Let's attach some packages
library(WDI)
library(ggplot2)
library(dplyr)
library(tidyr)
library(factoextra)
library(devtools)
library(ggbiplot)
library(pracma)
library(dialr)
library(countrycode)
library(NbClust)
library(dendextend)
options(scipen = 999)
set.seed(1)
# Let's load classic HDI scores for 2019 
url <- "https://raw.githubusercontent.com/mithridata-com/human_development_index/main/Classic_HDI_2019.csv"
classic_HDI <- read.csv(url, sep = ";")
classic_HDI$iso2c <- countryname(classic_HDI $Country, destination = 'iso2c')


# full list of country codes
country_codes <- c("BE","BG","CZ","DK","DE",
                   "EE","IE","GRC","ES","FR",
                   "HR","IT","CY","LV","LT",
                   "LU","HU","MT","NL","AT",
                   "PL","PT","RO","SI","SK",
                   "FI","SE", "CV", "ZA", "RU")

# short list of country codes overwrites the previous one for quicker testing
#country_codes <- c("PT", "CV", "ZA", "RU")

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
                    "Econ_Unemplyment" = "SL.UEM.TOTL.ZS",
                    "Econ_Inflation" = "FP.CPI.TOTL.ZG",
                    "Econ_GINI" = "SI.POV.GINI",
                    "Econ_GNIPerCapita" = "NY.GNP.PCAP.CD"
                    
)

# download data
data.raw = WDI(indicator=indicator_list,
          #country=country_codes, 
          start=2019, end=2019)


#this vector will change the direction of variables, so their interpretation can be done easier
# for example, life expectancy is better when it grows, while death rate is better when it decreases
vector_of_impact <- c(1,-1,1,-1,1,
                      1,-1,1,1,-1,
                      1,-1,1,1,1,1,1,1,1,1,
                      1,-1,-1,-1,1)

data.raw[,4:ncol(data.raw)] <- t(t(data.raw[,4:ncol(data.raw)]) * vector_of_impact)

# with this we drop all non-country level data items (such as whole World etc)
data.raw <- data.raw[check_cc(data.raw$iso2c),]
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

# Let's find columns, that are 2/3 filled
completeCols <- data_quality_byIndicator %>%
  filter(MeanCompleteScore>=0.50) %>% 
  select(Indicator) %>% unlist() %>% unname()


# Let's find columns, that are 2/3 filled
completeRows <- data_quality_byCountry %>%
  filter(MeanCompleteScore>=0.33) %>% 
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

fviz_contrib(data.pca, choice="var", axes = 5, top = 30,
             fill = "lightgray", color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45))     # Avoid text overlapping

round(data.pca$rotation,3)

# 1.2 Rotated loadings matrix
ncomp <- 6
rawLoadings <- data.pca$rotation[,1:ncomp] %*% diag(data.pca$sdev, ncomp, ncomp)
rotatedLoadings <- varimax(rawLoadings)$loadings
rotatedLoadings

# PC 1
# A measure of weak economical development, low urbanization, limited access to internet and bad living conditions (through life-exp)
# PC 2
# A measure of countries with weak health systems, with low immunization, low life exp. and high children mortality, undernourishment 
# PC 3
# A measure of weak labor markets
# PC 4
# A measure for countries with high death rates
# PC 5
# A measure for countries with high inflation
# PC 6
# A measure of educational development through primary completion rate

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


nb <- NbClust(scores, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans")
fviz_nbclust(nb)

# so the optimal number of clusters are 2,7 or 8
# we choose 7


k.clust <- 7
fit_complete<-hclust(d, method="complete")
plot(fit_complete)
rect.hclust(fit_complete, k=k.clust, border="red")
groups_complete <- cutree(fit_complete, k=k.clust)

fit_centroid<-hclust(d, method="centroid")
plot(fit_centroid)
rect.hclust(fit_centroid, k=k.clust, border="red")
groups_centroid <- cutree(fit_centroid, k=k.clust)

fit_single<-hclust(d, method="single")
plot(fit_single)
rect.hclust(fit_single, k=k.clust, border="red")
groups_single <- cutree(fit_single, k=k.clust)

fit_ward<-hclust(d, method="ward.D")
plot(fit_ward)
rect.hclust(fit_ward, k=k.clust, border="red")
groups_single <- cutree(fit_ward, k=k.clust)

#K-means clustering

fit_complete2<-kmeans(d, k.clust, nstart=10)
fit_complete2
fviz_cluster(fit_complete2,d, ellipse.type = "norm", repel = TRUE)


# Create three dendrograms
dend_complete <- as.dendrogram (fit_complete)
dend_centroid <- as.dendrogram (fit_centroid)
dend_single <- as.dendrogram (fit_single)
# Create a list to hold dendrograms
dend_list <- dendlist(dend_complete, dend_centroid, dend_single)
dend_list
#tanglegram(dend_complete, dend_centroid)

cor.dendlist(dend_list, method = "cophenetic")
cor.dendlist(dend_list, method = "baker")
# so now we know that three methods provide statisticaly different results

scores.df <- data.frame(scores) %>% mutate(cluster = groups_complete)
scores.df$iso2c <- countryname(row.names(scores.df), destination = 'iso2c')
colnames(scores.df) <- c(paste0("PC_",seq(1,ncomp,by = 1)),"CLUSTER_NUM")

scores.clust <- scores.df %>% 
 group_by(CLUSTER_NUM) %>% 
  dplyr::summarise(meanPC_1 = mean(PC_1),
            meanPC_2 = mean(PC_2),
            meanPC_3 = mean(PC_3),
            meanPC_4 = mean(PC_4),
            meanPC_5 = mean(PC_5),
            meanPC_6 = mean(PC_6))

scores.clust
# test if cluster is important
summary(aov(PC_1 ~ CLUSTER_NUM,data = scores.df))


# let's join our data with classic HDI
data.HDI <- scores.df %>% left_join(classic_HDI, by = ("iso2c"))
ggplot(data = data.HDI) + geom_point(aes(x = HDI, y = PC_1, color = factor(CLUSTER_NUM)))
