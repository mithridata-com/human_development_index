# Project on multivariate data analysis </br> (Professor Jorge Mendes, 2021)
Topic: Developing of HDI index with a breakdown by country

## Group composition </br>
1: D. D. </br>
2: R. T. </br>
3: Z. M. </br>
4: P. C. </br>

</br>

## Goal
The goal of our research is developing of HDI indicator for 30 countries (all members of European Union + Cabo Verde, South Africa and Russia) based on publicly available data.

## Datasets
For now, our code parses following datasets in a form of time series with a breakdown by country.
* "Liv_LifeExp" = "SP.DYN.LE00.IN",
* "Liv_LifeExpMale" = "SP.DYN.LE00.MA.IN",
* "Liv_LifeExpFemale" = "SP.DYN.LE00.FE.IN",
* "Liv_DeathRate" = "SP.DYN.CDRT.IN",
* "Liv_FertilityRate" = "SP.DYN.TFRT.IN",
* "Liv_BirthRate" = "SP.DYN.CBRT.IN",
* "Liv_PopulationDensity" = "EN.POP.DNST",
* "Liv_UrbanPopulation" = "SP.URB.TOTL.IN.ZS",
* "Liv_RoadMortality" = "SH.STA.TRAF.P5",
* "Liv_ChildrenEmployment" = "SL.TLF.0714.ZS",
* "Liv_AgeDependency" = "SP.POP.DPND",
* "Liv_IntentionalHomicide" = "VC.IHR.PSRC.P5",
* "Liv_SlumHouses" = "EN.POP.SLUM.UR.ZS",
* "Liv_AccessToElectricity" = "EG.ELC.ACCS.ZS",
* "Liv_BroadbankInternet" = "IT.NET.BBND.P2",
* "Liv_Population" = "SP.POP.TOTL",
* "Liv_FemalePopulation" = "SP.POP.TOTL.FE.ZS",
* "Liv_PopulationGrowth" = "SP.POP.GROW",
* "Health_Immunization" = "SH.IMM.IDPT",
* "Health_Undernourishment" = "SN.ITK.DEFC.ZS",
* "Health_BirthBySkilledStaff" = "SH.STA.BRTC.ZS",
* "Health_DeathByInjury" = "SH.DTH.INJR.ZS",
* "Health_Diabites" = "SH.STA.DIAB.ZS",
* "Health_HospitalBeds" = "SH.MED.BEDS.ZS",
* "Health_InfantMortality" = "SP.DYN.IMRT.IN",
* "Health_Under5Mortality" = "SH.DYN.MORT",
* "Health_HIV" = "SH.DYN.AIDS.ZS",
* "Health_MaleHIV" = "SH.HIV.1524.MA.ZS",
* "Health_FemaleHIV" = "SH.HIV.1524.FE.ZS",
* "Social_FemaleParticipationInBusiness" = "IC.FRM.FEMO.ZS",
* "Social_WomenInParliament" = "SG.GEN.PARL.ZS",
* "Social_IncomeShareHighest10p" = "SI.DST.10TH.10",
* "Social_IncomeShareLowest10p" = "SI.DST.FRST.10",
* "Social_Income0_20p" = "SI.DST.FRST.20",
* "Social_Income21_40p" = "SI.DST.02ND.20",
* "Social_Income41_60p" = "SI.DST.03RD.20",
* "Social_Income61_80p" = "SI.DST.04TH.20",
* "Social_Income81_100p" = "SI.DST.05TH.20",
* "Biz_TaxAndContributionRate" = "IC.TAX.TOTL.CP.ZS",
* "Biz_BusinessDisclosure" = "IC.BUS.DISC.XQ",
* "Biz_TimeToStartBusiness" = "IC.REG.DURS",
* "Biz_DoingBusiness" = "IC.BUS.EASE.XQ",
* "Biz_CreditInformation" = "IC.CRD.INFO.XQ",
* "Edu_GovernExpend" = "SE.XPD.TOTL.GD.ZS",
* "Edu_GovernExpendPerStudent" = "SE.XPD.SECO.PC.ZS",
* "Edu_PupilTeacherRatio" = "SE.PRM.ENRL.TC.ZS",
* "Edu_SchoolEnroll" = "SE.PRM.NENR",
* "Edu_R&DExpenditure" = "GB.XPD.RSDV.GD.ZS",
* "Edu_ScientificPublications" = "IP.JRN.ARTC.SC",
* "Edu_LiteracyRate" = "SE.ADT.LITR.ZS",
* "Edu_PrimaryCompletionRate" = "SE.PRM.CMPT.ZS",
* "Edu_TrainTeachersPrimary" = "SE.PRM.TCAQ.ZS",
* "Edu_ProgressionToSecondary" = "SE.SEC.PROG.MA.ZS",
* "Edu_SchoolEnrollmentGenderParity" = "SE.ENR.PRSC.FM.ZS",
* "Env_FreshwaterPerCapita" = "ER.H2O.INTR.PC",
* "Env_CO2emissions" = "EN.ATM.CO2E.PC",
* "Env_ForestArea" = "AG.LND.FRST.ZS",
* "Env_ArableLand" = "AG.LND.ARBL.ZS",
* "Econ_GdpPerCapita" = "NY.GDP.PCAP.CD",
* "Econ_GdpGrowth" = "NY.GDP.MKTP.KD.ZG",
* "Econ_GdpGrowthPerCapita" = "NY.GDP.PCAP.KD.ZG",
* "Econ_Unemplyment" = "SL.UEM.TOTL.ZS",
* "Econ_Inflation" = "FP.CPI.TOTL.ZG",
* "Econ_LegalRightsIndex" = "IC.LGL.CRED.XQ",
* "Econ_NetMigration" = "SM.POP.NETM",
* "Econ_RealInterestRate" = "FR.INR.RINR",
* "Econ_ImportGoodsAndServices" = "NE.IMP.GNFS.ZS",
* "Econ_ExportGoodsAndServices" = "NE.EXP.GNFS.ZS",
* "Econ_HighTechExport" = "TX.VAL.TECH.MF.ZS",
* "Econ_FuelExport" = "TX.VAL.FUEL.ZS.UN",
* "Econ_LogisticPerfomance" = "LP.LPI.OVRL.XQ",
* "Econ_TaxRevenue" = "GC.TAX.TOTL.GD.ZS",
* "Econ_MilitaryExpenditure" = "MS.MIL.XPND.GD.ZS",
* "Econ_TaxAndContributionRate" = "IC.TAX.TOTL.CP.ZS",
* "Econ_BusinessDisclosure" = "IC.BUS.DISC.XQ",
* "Econ_TimeToStartBusiness" = "IC.REG.DURS",
* "Econ_DoingBusiness" = "IC.BUS.EASE.XQ",
* "Econ_CreditInformation" = "IC.CRD.INFO.XQ",
* "Econ_GINI" = "SI.POV.GINI",
* "Econ_GNIPerCapita" = "NY.GNP.PCAP.CD"

The data for HDI is currently represeneted by prefixes "Liv" for living and health conditions, "Edu" for educational situation, "Econ" for economical conditions, "Env" for enviroment, "Health" for health amd medical care, "Biz" for business 
