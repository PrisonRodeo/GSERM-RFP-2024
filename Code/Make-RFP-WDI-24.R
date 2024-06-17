#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Intro things                                    ####
#
# GSERM - St. Gallen (2024)
#
# Analyzing Panel Data
# Prof. Christopher Zorn
#
# Code to grab & create data from the World Bank's
# _World Development Indicators_. These are
# the "running example" data that we use in the
# course.
#
# This code creates various dataframes that are 
# used as examples during the "Regression for 
# Publishing" course. See below for details.
#
# Your computer must have a working internet connection
# for this code to function properly.
#
# NOTE: This code can be somewhat slow to
# run, depending on how fast the World Bank's
# API is operating on any given day...
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Packages: This first code checks to see if the 
# packages needed are installed and made available. 
# If not, it installs and loads them; if so, it prints 
# a little smiley face. :) Either way, it loads all 
# the required packages necessary to create the data.

P<-c("RCurl","readr","data.table","countrycode","WDI",
     "readxl","countrycode")

for (i in 1:length(P)) {
  ifelse(!require(P[i],character.only=TRUE),install.packages(P[i]),
         print(":)"))
  library(P[i],character.only=TRUE)
}
rm(P)
rm(i)

# You probably want to run the first 40 lines of 
# this file 4-5 times, to make sure everything gets 
# correctly loaded.
#
# Also, be sure to set a working directory in here
# someplace, a la:
#
# setwd("~/AllMyStuff")
#
# or whatever.
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Grab the data...                              ####
#
# Get the relevant data / indicators (add variables
# as you wish...). There are something like 1400 
# variables in the WDI; these are only a few 
# selected ones:

wdi<-WDI(country="all",
         indicator=c("LandArea"="AG.LND.TOTL.K2", # Land area (sq. km)
                     "ArablePercent"="AG.LND.ARBL.ZS", # Arable Land (% of total land area)
                     "Population"="SP.POP.TOTL", # Popluation (in, like, people)
                     "PopGrowth"="SP.POP.GROW", # Population Growth (%)
                     "RuralPopulation"="SP.RUR.TOTL.ZS", # Rural Population (% of total)
                     "UrbanPopulation"="SP.URB.TOTL.IN.ZS", # Urban Population (% of total)
                     "BirthRatePer1K"="SP.DYN.CBRT.IN", # Birth Rate (births per 1K people)
                     "FertilityRate"="SP.DYN.TFRT.IN", # Fertility Rate (births per woman)
                     "PrimarySchoolAge"="SE.PRM.AGES", # Primary school starting age (years)
                     "LifeExpectancy"="SP.DYN.LE00.IN", # Life Expectancy at birth (years)
                     "AgeDepRatioOld"="SP.POP.DPND.OL", # Age Dependency Ratio (old), % working age population
                     "CO2Emissions"="EN.ATM.CO2E.PC", # CO2 Emissions (metric tons per capita)
                     "InfantMortality"="SP.DYN.IMRT.IN", # Infant mortality (deaths per 1K live births)
                     "ChildMortality"="SH.DYN.MORT", # Under-5 Child Mortality, deaths per 1K 
                     "DPTPercent"="SH.IMM.IDPT", # DPT immunization percentage
                     "GDP"="NY.GDP.MKTP.KD", # GDP, constant 2010 $US
                     "GDPPerCapita"="NY.GDP.PCAP.KD", # GDP per capita (constant 2010 $US)
                     "GDPPerCapGrowth"="NY.GDP.PCAP.KD.ZG", # GDP Per Capita Growth (%)
                     "Inflation"="FP.CPI.TOTL.ZG", # Inflation (CPI, annual %)
                     "TotalTrade"="NE.TRD.GNFS.ZS", # Total trade, % of GDP
                     "Exports"="NE.EXP.GNFS.ZS", # Exports, % of GDP
                     "Imports"="NE.IMP.GNFS.ZS", # Imports, % of GDP
                     "FDIIn"="BX.KLT.DINV.WD.GD.ZS", # FDI in, % of GDP
                     "AgriEmployment"="SL.AGR.EMPL.ZS", # Percent of total employment in agriculture
                     "NetAidReceived"="DT.ODA.ALLD.KD", # Net official dev. aid received (constant 2018 $US)
                     "MobileCellSubscriptions"="IT.CEL.SETS.P2", # Mobile / cellular subscriptions per 100 people
                     "NaturalResourceRents"="NY.GDP.TOTL.RT.ZS", # Total natural resource rents (% of GDP)
                     "MilitaryExpenditures"="MS.MIL.XPND.GD.ZS", # Military expenditures, % of GDP
                     "GovtExpenditures"="NE.CON.GOVT.ZS", # Government Expenditures, % of GDP
                     "PublicEdExpend"="SE.XPD.TOTL.GD.ZS", # Public expenditure on education (% of GDP)
                     "PublicHealthExpend"="SH.XPD.GHED.GD.ZS", # Public expenditure on health (% of GDP)
                     "HIVDeaths"="SH.DYN.AIDS.DH", # Deaths due to HIV/AIDS (UNAIDS estimate)
                     "WomenBusLawIndex"="SG.LAW.INDX", # Women Business & the Law Index Score
                     "WomenInLegislature"="SG.GEN.PARL.ZS", # Pct. Women in National Parliament
                     "PaidParentalLeave"="SH.PAR.LEVE.AL")) # Paid Parental Leave (0=no,1=yes)

# Remove aggregates (e.g., "World," "Arab World," etc.):

wdi$ISO3<-countrycode(wdi$iso2c,origin="iso2c",destination="iso3c")
wdi<-wdi[is.na(wdi$ISO3)==FALSE,]

# rename Year:

wdi$Year<-wdi$year
wdi$Year<-as.numeric(wdi$Year) # make numeric
wdi$year<-NULL

# Delete ISO2:

wdi$iso2c<-NULL

# Create a "region" variable

wdi$Region<-countrycode(wdi$ISO3,origin="iso3c",destination="region")

# Put ISO3 + Year + Region at the front of the data:

nc<-ncol(wdi)
sb<-seq(nc-2,nc)
se<-seq(1,(nc-3))
wdi<-wdi[,c(sb,se)]
rm(nc,sb,se)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Next, we can subset the data for the various days.
# Here is the Day One data:

WDI2018<-wdi[wdi$Year==2018,] # select only lines of data for which the
                           # "Year" variable equals 2018...
DayOne<-with(WDI2018, data.frame(ISO3=ISO3,Country=country,
                          Fertility=FertilityRate,
                          ChildMortality=ChildMortality,
                          InfantMortality=InfantMortality,
                          LifeExpectancy=LifeExpectancy,
                          DPTPercent=DPTPercent,
                          GDPPerCapita=GDPPerCapita,
                          FDIIn=FDIIn,
                          NaturalResourceRents=NaturalResourceRents,
                          UrbanPopulation=UrbanPopulation,
                          GovtExpenditures=GovtExpenditures,
                          PaidParentalLeave=PaidParentalLeave))

# For the Day One example, we also need to include the 
# POLITY IV measure of democracy (see details at 
# https://www.systemicpeace.org/inscrdata.html). 
#
# Grab those data:

url <- "http://www.systemicpeace.org/inscr/p5v2018.xls"
destfile <- "POLITY.xls"
curl::curl_download(url, destfile)
POLITY <- read_excel(destfile)
rm(url)

# Subset to 2018 ONLY:

POLITY <- POLITY[POLITY$year==2018,]

# Subset only the variables we need:

POLITY <- with(POLITY, data.frame(ID=scode,
                                  DemocScore=democ,
                                  AutocScore=autoc,
                                  POLITY=polity2))

# Harmonize the country identification codes, using
# the -countrycode- package:

POLITY$ISO3<-countrycode(POLITY$ID,"p5c","iso3c")
POLITY$ID<-NULL

# Merge in the POLITY data with the WDI data, matching
# on the ISO3 country code:

DayOne<-merge(DayOne,POLITY,by=c("ISO3"),all.x=TRUE,all.y=FALSE)

# Finally, write that to a .CSV as the "Day One" data file:

write_csv(DayOne,"WDI-2018-Day-One-24.csv") # write to CSV file

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Day Two: Cellphones and wealth...
#
# This one is just bivariate, so it's really 
# very small

DayTwo<-with(WDI2018, data.frame(ISO3=ISO3,Country=country,
                                 GDPPerCapita=GDPPerCapita,
                                 MobileCellSubscriptions=MobileCellSubscriptions))

# Delete missing data:

DayTwo<-DayTwo[complete.cases(DayTwo),]

# aaaand write it to a file:

write_csv(DayTwo,"WDI-2018-Day-Two-24.csv")

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

