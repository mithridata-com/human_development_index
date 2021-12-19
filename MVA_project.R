# project on multivariate data analysis
# Professor Jorge Mendes
# 2021

# Installing packages, that are required for the project
# skip this step if you already installed everything required
#install.packages('WDI')
#install.packages('ggplot2')
#install.packages('dplyr')
#install.packages('tidyr')
#install.packages('devtools')
#library(devtools)
#install_github("vqv/ggbiplot")
#install.packages('pracma')

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
                    "Liv_LifeExpMale" = "SP.DYN.LE00.MA.IN",
                    "Liv_LifeExpFemale" = "SP.DYN.LE00.FE.IN",
                    "Liv_DeathRate" = "SP.DYN.CDRT.IN",
                    "Liv_FertilityRate" = "SP.DYN.TFRT.IN",
                    "Liv_BirthRate" = "SP.DYN.CBRT.IN",
                    "Liv_PopulationDensity" = "EN.POP.DNST",
                    "Liv_UrbanPopulation" = "SP.URB.TOTL.IN.ZS",
                    "Liv_RoadMortality" = "SH.STA.TRAF.P5",
                    "Liv_ChildrenEmployment" = "SL.TLF.0714.ZS",
                    "Liv_AgeDependency" = "SP.POP.DPND",
                    "Liv_IntentionalHomicide" = "VC.IHR.PSRC.P5",
                    "Liv_SlumHouses" = "EN.POP.SLUM.UR.ZS",
                    #"Liv_AccessToElectricity" = "EG.ELC.ACCS.ZS", croosed it out because of very low variance
                    "Liv_BroadbandInternet" = "IT.NET.BBND.P2",
                    "Liv_Population" = "SP.POP.TOTL",
                    "Liv_FemalePopulation" = "SP.POP.TOTL.FE.ZS",
                    "Liv_PopulationGrowth" = "SP.POP.GROW",
                    
                    "Health_Immunization" = "SH.IMM.IDPT",
                    "Health_Undernourishment" = "SN.ITK.DEFC.ZS",
                    "Health_BirthBySkilledStaff" = "SH.STA.BRTC.ZS",
                    "Health_DeathByInjury" = "SH.DTH.INJR.ZS",
                    "Health_Diabites" = "SH.STA.DIAB.ZS",
                    "Health_HospitalBeds" = "SH.MED.BEDS.ZS",
                    "Health_InfantMortality" = "SP.DYN.IMRT.IN",
                    "Health_Under5Mortality" = "SH.DYN.MORT",
                    "Health_HIV" = "SH.DYN.AIDS.ZS",
                    "Health_MaleHIV" = "SH.HIV.1524.MA.ZS",
                    "Health_FemaleHIV" = "SH.HIV.1524.FE.ZS",
                    
                    "Social_FemaleParticipationInBusiness" = "IC.FRM.FEMO.ZS",
                    "Social_WomenInParliament" = "SG.GEN.PARL.ZS",
                    "Social_IncomeShareHighest10p" = "SI.DST.10TH.10",
                    "Social_IncomeShareLowest10p" = "SI.DST.FRST.10",
                    "Social_Income0_20p" = "SI.DST.FRST.20",
                    "Social_Income21_40p" = "SI.DST.02ND.20",
                    "Social_Income41_60p" = "SI.DST.03RD.20",
                    "Social_Income61_80p" = "SI.DST.04TH.20",
                    "Social_Income81_100p" = "SI.DST.05TH.20",

                    "Edu_GovernExpend" = "SE.XPD.TOTL.GD.ZS",
                    "Edu_GovernExpendPerStudent" = "SE.XPD.SECO.PC.ZS",
                    "Edu_PupilTeacherRatio" = "SE.PRM.ENRL.TC.ZS",
                    "Edu_SchoolEnroll" = "SE.PRM.NENR",
                    "Edu_R&DExpenditure" = "GB.XPD.RSDV.GD.ZS",
                    "Edu_ScientificPublications" = "IP.JRN.ARTC.SC",
                    "Edu_LiteracyRate" = "SE.ADT.LITR.ZS",
                    "Edu_PrimaryCompletionRate" = "SE.PRM.CMPT.ZS",
                    "Edu_TrainTeachersPrimary" = "SE.PRM.TCAQ.ZS",
                    "Edu_ProgressionToSecondary" = "SE.SEC.PROG.MA.ZS",
                    "Edu_SchoolEnrollmentGenderParity" = "SE.ENR.PRSC.FM.ZS",
                    
                    "Env_FreshwaterPerCapita" = "ER.H2O.INTR.PC",
                    "Env_CO2emissions" = "EN.ATM.CO2E.PC",
                    "Env_ForestArea" = "AG.LND.FRST.ZS",
                    "Env_ArableLand" = "AG.LND.ARBL.ZS",
                    
                    "Econ_GdpPerCapita" = "NY.GDP.PCAP.CD",
                    "Econ_GdpGrowth" = "NY.GDP.MKTP.KD.ZG",
                    "Econ_GdpGrowthPerCapita" = "NY.GDP.PCAP.KD.ZG",
                    "Econ_Unemplyment" = "SL.UEM.TOTL.ZS",
                    "Econ_Inflation" = "FP.CPI.TOTL.ZG",
                    "Econ_LegalRightsIndex" = "IC.LGL.CRED.XQ",
                    "Econ_NetMigration" = "SM.POP.NETM",
                    "Econ_RealInterestRate" = "FR.INR.RINR",
                    "Econ_ImportGoodsAndServices" = "NE.IMP.GNFS.ZS",
                    "Econ_ExportGoodsAndServices" = "NE.EXP.GNFS.ZS",
                    "Econ_HighTechExport" = "TX.VAL.TECH.MF.ZS",
                    "Econ_FuelExport" = "TX.VAL.FUEL.ZS.UN",
                    "Econ_LogisticPerfomance" = "LP.LPI.OVRL.XQ",
                    "Econ_TaxRevenue" = "GC.TAX.TOTL.GD.ZS",
                    "Econ_MilitaryExpenditure" = "MS.MIL.XPND.GD.ZS",
                    "Econ_TaxAndContributionRate" = "IC.TAX.TOTL.CP.ZS",
                    "Econ_TimeToStartBusiness" = "IC.REG.DURS",
                    "Econ_GINI" = "SI.POV.GINI",
                    "Econ_GNIPerCapita" = "NY.GNP.PCAP.CD"
)

# at this stage we prepare a metadata frame with links to official methodology
metadata_links <- data.frame(indicator_number = 1:length(indicator_list), 
                             indicator_list,
                             sample_link_to_metadata = "https://databank.worldbank.org/reports.aspx?source=2&series=XX.XX.XX.XX&country=")

for(i in 1:nrow(metadata_links))
{
  metadata_links[i,"link"] <- gsub("XX.XX.XX.XX",
                                   metadata_links[i, "indicator_list"],
                                   metadata_links[i, "sample_link_to_metadata"])
}
metadata_links$sample_link_to_metadata <- NULL
# when you need a detailed methodology - go to metadata_links$link column and paste the link to the web-browser
head(metadata_links)
#write.table(metadata_links, "metadata.csv")

# here we use WDI package to download data
# based on country_codes vector
# and indicator_list vector
data.raw = WDI(indicator=indicator_list,
          #country=country_codes, 
          start=2019, end=2019)

# here we compute basic data quality metrics such as completeness
# higher score means higher quality
data_quality_byIndicator <- data.raw %>%
  gather(key = Indicator, value = Value, -iso2c, -country, -year) %>%
  group_by(Indicator) %>%
  dplyr::summarize(MeanCompleteScore = mean(ifelse(!is.na(Value),1,0))) %>%
  ungroup()

# Let's find columns, that are 90% filled (42 out of 80)
completeCols <- data_quality_byIndicator %>%
  filter(MeanCompleteScore>=0.67) %>% 
  select(Indicator) %>% unlist() %>% unname()

# Here we create a subset of dataframe with complete columns, 
# all NAs that are left are filled with mean 
data <- data.raw %>% 
  select(iso2c, country, unlist(completeCols)) %>%
  mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))


# first solution. 
# 1. PCA
# 1.1. Ordinary PCA
data.scaled <- scale(data[,3:ncol(data)])
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

# 1.2 Rotated loadings matrix
ncomp <- 10
rawLoadings     <- data.pca$rotation[,1:ncomp] %*% diag(data.pca$sdev, ncomp, ncomp)
rotatedLoadings <- varimax(rawLoadings)$loadings
rotatedLoadings
invLoadings     <- t(pracma::pinv(rotatedLoadings))
scores          <- data.scaled %*% invLoadings






