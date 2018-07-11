library(readr)
library(readxl)


# read and prepare ../Datasources for visulization ####
incomeAffordability<-read_xlsx("Data/incomeAffordability.xlsx")
saveRDS(incomeAffordability, "Data/rds/incomeAffordability.rds")

medianSale<-read.csv("Data/medianSale.csv")
saveRDS(medianSale, "Data/rds/medianSale.rds")

neighborhoodRent<-read.csv("Data/rentAve.csv")
saveRDS(neighborhoodRent, "Data/rds/neighborhoodRent.rds")

historicalVacancy<-read.csv("Data/vacancyHis.csv")
saveRDS(historicalVacancy, "Data/rds/historicalVacancy.rds")

incomeMed<-read.csv("Data/incomeMedian.csv")
saveRDS(incomeMed, "Data/rds/incomeMed.rds")

multi<-read_excel("Data/Multifamily.xlsx", sheet = "Multi-Family Listings" )
saveRDS(multi, "Data/rds/multi.rds")

constructionTrend<-read_xlsx("Data/yearly_construction_permit_total.xlsx")
saveRDS(constructionTrend, "Data/rds/constructionTrend.rds")


# ownerVsRenter<-read.csv("Data/housingStock_ownerVsRenter.csv")

allUnitsRatio<-read.csv("Data/allUnits.csv")
saveRDS(allUnitsRatio, "Data/rds/allUnitsRatio.rds")

ownersUnitsRatio<-read.csv("Data/ownersUnits.csv")
saveRDS(ownersUnitsRatio, "Data/rds/ownersUnitsRatio.rds")

rentersUnitsRatio<-read.csv("Data/rentersUnits.csv")
saveRDS(rentersUnitsRatio, "Data/rds/rentersUnitsRatio.rds")

houseAge <- read.csv("Data/houseAge.csv")
saveRDS(houseAge, "Data/rds/houseAge.rds")

costBurden <- read.csv("Data/costBurden.csv")
saveRDS(costBurden, "Data/rds/costBurden.rds")

incomeLevels <- read.csv("Data/incomeLevels.csv")
saveRDS(incomeLevels, "Data/rds/incomeLevels.rds")

averageRentsVsAffordability <- read_csv("Data/averageRentsVsAffordability.csv")
saveRDS(averageRentsVsAffordability, "Data/rds/averageRentsVsAffordability.rds")

wageIncreaseVsHomeSalePrice <- read.csv("Data/wageIncreaseVsHomeSalePrice.csv")
saveRDS(wageIncreaseVsHomeSalePrice, "Data/rds/wageIncreaseVsHomeSalePrice.rds")

wageIncreaseVsRent <- read.csv("Data/wageIncreaseVsRent.csv")
saveRDS(wageIncreaseVsRent, "Data/rds/wageIncreaseVsRent.rds")

#save.image(file = "SLCHousing.R../Data")
