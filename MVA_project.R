# project on multivariate data analysis
# Professor Jorge Mendes
# 2021

# Installing packages, that are required for the project
install.packages('WDI')
install.packages('ggplot2')

# MAIN CODE STARTS HERE
# Let's attach some packages
library(WDI)
library(ggplot2)

# full list of country codes
country_codes <- c("BE","BG","CZ","DK","DE",
                   "EE","IE","GRC","ES","FR",
                   "HR","IT","CY","LV","LT",
                   "LU","HU","MT","NL","AT",
                   "PL","PT","RO","SI","SK",
                   "FI","SE", "CPV", "ZAF", "RU")

# short list of country codes overwrites the previous one for quicker testing
country_codes <- c("PT", "CPV", "ZAF", "RU")

# list of indicators, that would be used to develop an index
indicator_list <- c("Liv_LifeExp" = "SP.DYN.LE00.IN", 
                    "Liv_DeathRate" = "SP.DYN.CDRT.IN",
                    "Liv_FertilityRate" = "SP.DYN.TFRT.IN",
                    
                    "Edu_GovernExpend" = "SE.XPD.TOTL.GD.ZS",
                    "Edu_PupilTeacherRatio" = "SE.PRM.ENRL.TC.ZS",
                    "Edu_SchoolEnroll" = "SE.PRM.NENR",
                    
                    "Econ_GdpPerCapita" = "NY.GDP.PCAP.CD",
                    "Econ_CO2emissions" = "EN.ATM.CO2E.PC",
                    "Econ_Unemplyment" = "SL.UEM.TOTL.ZS"
)

cor(dat[,4:12], use = "complete")
# here we use WDI package to download data
# based on country_codes vector
# and indicator_list vector
dat = WDI(indicator= indicator_list,
          country=country_codes, 
          start=1960, end=2019)

# END OF MAIN CODE

# TEST ZONE: Just test some ideas or watch data here
# Let's draw Life Expectancy by country
ggplot(dat) + geom_line(aes(x = year, y = Liv_LifeExp, color = country)) + 
  xlab('Year') + ylab(attributes(dat$Liv_LifeExp)[[1]])



