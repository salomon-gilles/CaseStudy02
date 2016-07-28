
#-------------------------------------------------------------------------------
#                                                                              - 
#       Basic data analysis data                                               -
#-------------------------------------------------------------------------------

## @knitr analyzeBuzporfolio
par(mfrow=c(3,2))

plot(EfakAsIs , col="red",main="EfakAsIs")
plot(EfakPlan , col="red",main="EfakPlan")
plot(WugeAsIs, col="blue", main="WugeAsIs")
plot(WugePlan, col="blue", main="WugePlan")
plot(BlueEtelAsIs, col="orange", main="BlueEtelAsIs")
plot(BlueEtelPlan, col="orange", main="BlueEtelPlan")
plot(RedEtelAsIs, col="purple", main="RedEtelAsIs")
plot(RedEtelPlan, col="purple", main="RedEtelPlan")
plot(TotalEtelAsIs, col="green",main="TotalEtelAsIs")
plot(TotalEtelPlan, col="green",main="TotalEtelPlan")
plot(TotalAsIs, col="black", main="TotalAsIs")
plot(TotalPlan , col="black", main="TotalPlan")



#-------------------------------------------------------------------------------
#      Correlation between As Is and Plan data                                 -
#           linear relationship between AsIs and Plan                          -
#-------------------------------------------------------------------------------

## @knitr corrAsisPlandata
cor(TotalAsIs, TotalPlan )
cor(EfakAsIs , EfakPlan)
cor(WugeAsIs, WugePlan)
cor(TotalEtelAsIs, TotalEtelPlan)
cor(BlueEtelAsIs , BlueEtelPlan)
cor(RedEtelAsIs , RedEtelPlan)
cor(YearAsIs, YearPlan)

TotalAsIs_lm <- lm(TotalAsIs ~ TotalPlan , data = TotalAsIs)
summary(TotalAsIs_lm)

TotalAsIs_tslm <- tslm(TotalAsIs ~ TotalPlan )
summary(TotalAsIs_tslm)

#-------------------------------------------------------------------------------
#      Time series analysis  -  using the stl function                         -
#-------------------------------------------------------------------------------

## @knitr timeseriesStl
TotalAsIs_stl <- stl(TotalAsIs, s.window=5)
EfakAsIs_stl <- stl(EfakAsIs , s.window=5)
WugeAsIs_stl <- stl(WugeAsIs, s.window=5)
TotalEtelAsIs_stl <- stl(TotalEtelAsIs, s.window=5)
BlueEtelAsIs_stl <- stl(BlueEtelAsIs , s.window=5)
RedEtelAsIs_stl <- stl(RedEtelAsIs , s.window=5)

par(mfrow=c(3,2))
plot(TotalAsIs_stl, col="black", main="TotalAsIs_stl")
plot(EfakAsIs_stl, col="black", main="EfakAsIs_stl")
plot(WugeAsIs_stl, col="black", main="WugeAsIs_stl")
plot(TotalEtelAsIs_stl, col="black", main="TotalEtelAsIs_stl")
plot(BlueEtelAsIs_stl, col="black", main="BlueEtelAsIs_stl")
plot(RedEtelAsIs_stl, col="black", main="RedEtelAsIs_stl")
par(mfrow=c(3,2))

## @knitr timeseriesStlTrend
plot(TotalAsIs_stl$time.series[,"trend"], col="black")
plot(EfakAsIs_stl$time.series[,"trend"], col="red")
plot(WugeAsIs_stl$time.series[,"trend"], col="blue")
plot(TotalEtelAsIs_stl$time.series[,"trend"], col="green")
plot(BlueEtelAsIs_stl$time.series[,"trend"], col="orange")
plot(RedEtelAsIs_stl$time.series[,"trend"], col="purple")



#-------------------------------------------------------------------------------
#      component modification - seasonal component modification                -
#-------------------------------------------------------------------------------

## @knitr modifySeasonalMonthlyBase
monthplot(TotalAsIs_stl$time.series[,"seasonal"], main="", ylab="Seasonal")
monthplot(EfakAsIs_stl$time.series[,"seasonal"], main="", ylab="Seasonal")
monthplot(WugeAsIs_stl$time.series[,"seasonal"], main="", ylab="Seasonal")
monthplot(TotalEtelAsIs_stl$time.series[,"seasonal"], main="", ylab="Seasonal")
monthplot(BlueEtelAsIs_stl$time.series[,"seasonal"], main="", ylab="Seasonal")
monthplot(RedEtelAsIs_stl$time.series[,"seasonal"], main="", ylab="Seasonal")


#-------------------------------------------------------------------------------
#      Correlation  of external indicator                                      -
#-------------------------------------------------------------------------------

## @knitr corrCEPIData
CEPIVector <- c(ImportedIndicators[2:13,2],ImportedIndicators[2:13,3],ImportedIndicators[2:13,4],ImportedIndicators[2:13,5],ImportedIndicators[2:13,6],ImportedIndicators[2:13,7])
CEPI <- ts(CEPIVector , start=c(2008,1), end=c(2013,12), frequency=12)
plot(CEPI, main="CEPI")

cor(TotalAsIs, CEPI)
cor(EfakAsIs , CEPI)
cor(WugeAsIs, CEPI)
cor(TotalEtelAsIs, CEPI)
cor(BlueEtelAsIs , CEPI)
cor(RedEtelAsIs , CEPI)

## @knitr corrSIData
SIGovVector <- c(ImportedIndicators[16:27,2],ImportedIndicators[16:27,3],ImportedIndicators[16:27,4],ImportedIndicators[16:27,5],ImportedIndicators[16:27,6],ImportedIndicators[16:27,7])
SIGov <- ts(SIGovVector , start=c(2008,1), end=c(2013,12), frequency=12)
plot(SIGov, main="SIGov")

cor(TotalAsIs, SIGov)
cor(EfakAsIs , SIGov)
cor(WugeAsIs, SIGov)
cor(TotalEtelAsIs, SIGov)
cor(BlueEtelAsIs , SIGov)
cor(RedEtelAsIs , SIGov)

## @knitr corrAVGTmpData
TemperatureVector <- c(ImportedIndicators[30:41,2],ImportedIndicators[30:41,3],ImportedIndicators[30:41,4],ImportedIndicators[30:41,5],ImportedIndicators[30:41,6],ImportedIndicators[30:41,7])
Temperature <- ts(TemperatureVector, start=c(2008,1), end=c(2013,12), frequency=12)
plot(Temperature, main="Temperature")

cor(TotalAsIs, Temperature)
cor(EfakAsIs , Temperature)
cor(WugeAsIs, Temperature)
cor(TotalEtelAsIs, Temperature)
cor(BlueEtelAsIs , Temperature)
cor(RedEtelAsIs , Temperature)

## @knitr corrBirthsData
BirthsVector <- c(ImportedIndicators[44:55,2],ImportedIndicators[44:55,3],ImportedIndicators[44:55,4],ImportedIndicators[44:55,5],ImportedIndicators[44:55,6],ImportedIndicators[44:55,7])
Births <- ts(BirthsVector, start=c(2008,1), end=c(2013,12), frequency=12)
plot(Births, main="Births")

cor(TotalAsIs, Births)
cor(EfakAsIs , Births)
cor(WugeAsIs, Births)
cor(TotalEtelAsIs, Births)
cor(BlueEtelAsIs , Births)
cor(RedEtelAsIs , Births)

## @knitr corrXSImpData
SIExternVector <- c(ImportedIndicators[58:69,2],ImportedIndicators[58:69,3],ImportedIndicators[58:69,4],ImportedIndicators[58:69,5],ImportedIndicators[58:69,6],ImportedIndicators[58:69,7])
SIExtern <- ts(SIExternVector, start=c(2008,1), end=c(2013,12), frequency=12)
plot(SIExtern, main="SIExtern")

cor(TotalAsIs, SIExtern)
cor(EfakAsIs , SIExtern)
cor(WugeAsIs, SIExtern)
cor(TotalEtelAsIs, SIExtern)
cor(BlueEtelAsIs , SIExtern)
cor(RedEtelAsIs , SIExtern)

## @knitr corrUrbanoXportsData
UrbanoExportsVector <- c(ImportedIndicators[72:83,2],ImportedIndicators[72:83,3],ImportedIndicators[72:83,4],ImportedIndicators[72:83,5],ImportedIndicators[72:83,6],ImportedIndicators[72:83,7])
UrbanoExports <- ts(UrbanoExportsVector, start=c(2008,1), end=c(2013,12), frequency=12)
plot(UrbanoExports, main="UrbanoExports")

cor(TotalAsIs, UrbanoExports)
cor(EfakAsIs , UrbanoExports)
cor(WugeAsIs, UrbanoExports)
cor(TotalEtelAsIs, UrbanoExports)
cor(BlueEtelAsIs , UrbanoExports)
cor(RedEtelAsIs , UrbanoExports)

## @knitr corrGlobalizationData
GlobalisationPartyMembersVector <- c(ImportedIndicators[86:97,2],ImportedIndicators[86:97,3],ImportedIndicators[86:97,4],ImportedIndicators[86:97,5],ImportedIndicators[86:97,6],ImportedIndicators[86:97,7])
GlobalisationPartyMembers <- ts(GlobalisationPartyMembersVector, start=c(2008,1), end=c(2013,12), frequency=12)
plot(GlobalisationPartyMembers, main="GlobalisationPartyMembers")

cor(TotalAsIs, GlobalisationPartyMembers)
cor(EfakAsIs , GlobalisationPartyMembers)
cor(WugeAsIs, GlobalisationPartyMembers)
cor(TotalEtelAsIs, GlobalisationPartyMembers)
cor(BlueEtelAsIs , GlobalisationPartyMembers)
cor(RedEtelAsIs , GlobalisationPartyMembers)

## @knitr corrAVGXPIData
AEPIVector <- c(ImportedIndicators[100:111,2],ImportedIndicators[100:111,3],ImportedIndicators[100:111,4],ImportedIndicators[100:111,5],ImportedIndicators[100:111,6],ImportedIndicators[100:111,7])
AEPI <- ts(AEPIVector, start=c(2008,1), end=c(2013,12), frequency=12)
plot(AEPI, main="AEPI")

cor(TotalAsIs, AEPI)
cor(EfakAsIs , AEPI)
cor(WugeAsIs, AEPI)
cor(TotalEtelAsIs, AEPI)
cor(BlueEtelAsIs , AEPI)
cor(RedEtelAsIs , AEPI)

## @knitr corrPPIData
PPIEtelVector <- c(ImportedIndicators[114:125,2],ImportedIndicators[114:125,3],ImportedIndicators[114:125,4],ImportedIndicators[114:125,5],ImportedIndicators[114:125,6],ImportedIndicators[114:125,7])
PPIEtel <- ts(PPIEtelVector, start=c(2008,1), end=c(2013,12), frequency=12)
plot(PPIEtel, main="PPIEtel")

cor(TotalAsIs, PPIEtel)
cor(EfakAsIs , PPIEtel)
cor(WugeAsIs, PPIEtel)
cor(TotalEtelAsIs, PPIEtel)
cor(BlueEtelAsIs , PPIEtel)
cor(RedEtelAsIs , PPIEtel)

## @knitr corrNHData
NationalHolidaysVector <- c(ImportedIndicators[170:181,2],ImportedIndicators[170:181,3],ImportedIndicators[170:181,4],ImportedIndicators[170:181,5],ImportedIndicators[170:181,6],ImportedIndicators[170:181,7])
NationalHolidays <- ts(NationalHolidaysVector, start=c(2008,1), end=c(2013,12), frequency=12)
plot(NationalHolidays, main="NationalHolidays")

cor(TotalAsIs, NationalHolidays)
cor(EfakAsIs , NationalHolidays)
cor(WugeAsIs, NationalHolidays)
cor(TotalEtelAsIs, NationalHolidays)
cor(BlueEtelAsIs , NationalHolidays)
cor(RedEtelAsIs , NationalHolidays)

## @knitr corrCIData
ChulwalarIndexVector <- c(ImportedIndicators[128:139,2],ImportedIndicators[128:139,3],ImportedIndicators[128:139,4],ImportedIndicators[128:139,5],ImportedIndicators[128:139,6],ImportedIndicators[128:139,7])
ChulwalarIndex <- ts(ChulwalarIndexVector, start=c(2008,1), end=c(2013,12), frequency=12)
plot(ChulwalarIndex, main="ChulwalarIndex")

cor(TotalAsIs, ChulwalarIndex)
cor(EfakAsIs , ChulwalarIndex)
cor(WugeAsIs, ChulwalarIndex)
cor(TotalEtelAsIs, ChulwalarIndex)
cor(BlueEtelAsIs , ChulwalarIndex)
cor(RedEtelAsIs , ChulwalarIndex)

## @knitr corrMIRData
InflationVector <- c(ImportedIndicators[142:153,2],ImportedIndicators[142:153,3],ImportedIndicators[142:153,4],ImportedIndicators[142:153,5],ImportedIndicators[142:153,6],ImportedIndicators[142:153,7])
Inflation <- ts(InflationVector, start=c(2008,1), end=c(2013,12), frequency=12)
plot(Inflation, main="Inflation")

cor(TotalAsIs, Inflation)
cor(EfakAsIs , Inflation)
cor(WugeAsIs, Inflation)
cor(TotalEtelAsIs, Inflation)
cor(BlueEtelAsIs , Inflation)
cor(RedEtelAsIs , Inflation)

## @knitr corrPNHData
IndependenceDayPresentsVector <- c(ImportedIndicators[156:167,2],ImportedIndicators[156:167,3],ImportedIndicators[156:167,4],ImportedIndicators[156:167,5],ImportedIndicators[156:167,6],ImportedIndicators[156:167,7])
IndependenceDayPresents <- ts(IndependenceDayPresentsVector, start=c(2008,1), end=c(2013,12), frequency=12)
plot(IndependenceDayPresents, main="IndependenceDayPresents")

cor(TotalAsIs, IndependenceDayPresents)
cor(EfakAsIs , IndependenceDayPresents)
cor(WugeAsIs, IndependenceDayPresents)
cor(TotalEtelAsIs, IndependenceDayPresents)
cor(BlueEtelAsIs , IndependenceDayPresents)
cor(RedEtelAsIs , IndependenceDayPresents)

## @knitr corrINHData
InfluenceNationalHolidaysVector <- c(ImportedIndicators[184:195,2],ImportedIndicators[184:195,3],ImportedIndicators[184:195,4],ImportedIndicators[184:195,5],ImportedIndicators[184:195,6],ImportedIndicators[184:195,7])
InfluenceNationalHolidays <- ts(InfluenceNationalHolidaysVector, start=c(2008,1), end=c(2013,12), frequency=12)
plot(InfluenceNationalHolidays, main="InfluenceNationalHolidays")

cor(TotalAsIs, InfluenceNationalHolidays)
cor(EfakAsIs , InfluenceNationalHolidays)
cor(WugeAsIs, InfluenceNationalHolidays)
cor(TotalEtelAsIs, InfluenceNationalHolidays)
cor(BlueEtelAsIs , InfluenceNationalHolidays)
cor(RedEtelAsIs , InfluenceNationalHolidays)

# Check that the data import has worked

## @knitr corrCheckData
str(CEPIVector)
str(SIGovVector)  
str(TemperatureVector) 
str(BirthsVector)
str(SIExternVector)
str(UrbanoExportsVector) 
str(GlobalisationPartyMembersVector)
str(AEPIVector) 
str(PPIEtelVector) 
str(NationalHolidaysVector) 
str(ChulwalarIndexVector)
str(InflationVector) 
str(IndependenceDayPresentsVector)


#-------------------------------------------------------------------------------
#      Correlation  of indicators with one another                             -
#-------------------------------------------------------------------------------

## @knitr corrIndicatorWithEachOther
IndicatorsMatrix <-cbind(CEPIVector, SIGovVector, TemperatureVector, BirthsVector, SIGovVector, UrbanoExportsVector, GlobalisationPartyMembersVector, AEPIVector, PPIEtel, NationalHolidaysVector, ChulwalarIndexVector, InflationVector, IndependenceDayPresentsVector)

IndicatorsmatrixStandardised=scale(IndicatorsMatrix)
IndicatorsmatrixStandardised
NumberOfIndicators=dim(IndicatorsmatrixStandardised)[1]
NumberOfIndicators

IndicatorsCorrelationCoefficientMatrix=(1/(NumberOfIndicators-1))*t(IndicatorsmatrixStandardised)%*%IndicatorsmatrixStandardised
IndicatorsCorrelationCoefficientMatrix


#-------------------------------------------------------------------------------------------------------------------------
##        modelWithAllIndicators
#-------------------------------------------------------------------------------------------------------------------------
## @knitr modelWithAllIndicators
ModelWithAlllIndicators <- tslm(TotalAsIs ~ trend + season + CEPI + SIGov + Temperature + Births + SIExtern + UrbanoExports + GlobalisationPartyMembers + AEPI + PPIEtel + NationalHolidays + ChulwalarIndex + Inflation + IndependenceDayPresents)
summary(ModelWithAlllIndicators)
ModelWithCEPI <- tslm(TotalAsIs ~ trend + season + CEPI)
summary(ModelWithCEPI)    
ModelWithSIGov <- tslm(TotalAsIs ~ trend + season + SIGov)
summary(ModelWithSIGov)    
ModelWithTemperature <- tslm(TotalAsIs ~ trend + season + Temperature)
summary(ModelWithTemperature)
ModelWithBirths <- tslm(TotalAsIs ~ trend + season + Births)
summary(ModelWithBirths) 
ModelWithSIExtern <- tslm(TotalAsIs ~ trend + season + SIExtern)
summary(ModelWithSIExtern) 
ModelWithTotalUrbanoExports <- tslm(TotalAsIs ~ trend + season + UrbanoExports)
summary(ModelWithTotalUrbanoExports) 
ModelWithGlobalisationPartyMembers <- tslm(TotalAsIs ~ trend + season + GlobalisationPartyMembers)
summary(ModelWithGlobalisationPartyMembers) 
ModelWithAEPI <- tslm(TotalAsIs ~ trend + season + AEPI)
summary(ModelWithAEPI) 
ModelWithPPIEtel <- tslm(TotalAsIs ~ trend + season + PPIEtel)
summary(ModelWithPPIEtel)
ModelWithNationalHolidays <- tslm(TotalAsIs ~ trend + season + NationalHolidays)
summary(ModelWithNationalHolidays)
ModelWithChulwalarIndex <- tslm(TotalAsIs ~ trend + season + ChulwalarIndex)
summary(ModelWithChulwalarIndex) 
ModelWithInflation <- tslm(TotalAsIs ~ trend + season + Inflation)
summary(ModelWithInflation)
ModelWithIndependenceDayPresents <- tslm(TotalAsIs ~ trend + season + IndependenceDayPresents)
summary(ModelWithIndependenceDayPresents)
ModelWithInfluenceNationalHolidays <- tslm(TotalAsIs ~ trend + season + InfluenceNationalHolidays)
summary(ModelWithInfluenceNationalHolidays)
#-------------------------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------------------------------
## @knitr modelWithHighCorrelationIndicators
IndicatorsCorrelationCoefficientMatrix

ModelWithHighCorrelatingIndicators <- tslm(TotalAsIs ~ trend + season + CEPI + SIExtern + UrbanoExports + GlobalisationPartyMembers + AEPI)
summary(ModelWithHighCorrelatingIndicators) 

IndicatorsCorrelationCoefficientMatrix

ModelWithLowCorrelatingIndicators <- tslm(TotalAsIs ~ trend + season + NationalHolidays + UrbanoExports + GlobalisationPartyMembers)
summary(ModelWithLowCorrelatingIndicators) 
ModelWithTrendAndSeasonalityOnly <- tslm(TotalAsIs ~ trend + season)
summary(ModelWithTrendAndSeasonalityOnly)
ModelWithoutTrendAndSeasonality <- tslm(TotalAsIs ~ CEPI + SIExtern + UrbanoExports + GlobalisationPartyMembers + AEPI)
summary(ModelWithoutTrendAndSeasonality)
ModelWithEfakExportsIndicators <- tslm(EfakAsIs  ~ trend + season + CEPI + UrbanoExports + AEPI + GlobalisationPartyMembers)
summary(ModelWithEfakExportsIndicators)
ModelEfakSalesWithCEPI <- tslm(EfakAsIs  ~ trend + season + CEPI)
summary(ModelEfakSalesWithCEPI)
ModelEfakSalesWithTrendAnsSeasonalityOnly <- tslm(EfakAsIs  ~ trend + season)
summary(ModelEfakSalesWithTrendAnsSeasonalityOnly)
ModelWithCEPIOnly <- tslm(EfakAsIs  ~ CEPI)
summary(ModelWithCEPIOnly)
ModelWithWugeExportsIndicators <- tslm(WugeAsIs ~ trend + season + CEPI + UrbanoExports + AEPI)
summary(ModelWithWugeExportsIndicators)
ModelWugeWithCEPI <- tslm(WugeAsIs ~ trend + season + CEPI)
summary(ModelWugeWithCEPI)
ModelWugeWithTrendAndSeasonalityOnly <- tslm(WugeAsIs ~ trend + season)
summary(ModelWugeWithTrendAndSeasonalityOnly)
ModelTotalEtel <- tslm(TotalEtelAsIs~ trend + season)
summary(ModelTotalEtel)
ModelBlueEtel <- tslm(BlueEtelAsIs  ~ trend + season)
summary(ModelBlueEtel)
ModelRedEtel <- tslm(RedEtelAsIs  ~ trend + season)
summary(ModelRedEtel)
#-------------------------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#      Forecasting models                                                      -
#-------------------------------------------------------------------------------

## @knitr modelsForecasts
TotalAsIs_2012 <- ts(TotalAsIsVector , start=c(2008,1), end=c(2012,12), frequency=12)
EfakAsIs_2012 <- ts(EfakAsIsVector , start=c(2008,1), end=c(2012,12), frequency=12)
WugeAsIs_2012 <- ts(WugeAsIsVector, start=c(2008,1), end=c(2012,12), frequency=12)
TotalEtelAsIs_2012 <- ts(TotalEtelAsIsVector, start=c(2008,1), end=c(2012,12), frequency=12)
BlueEtelAsIs_2012 <- ts(BlueEtelAsIsVector, start=c(2008,1), end=c(2012,12), frequency=12)
RedEtelAsIs_2012 <- ts(RedEtelAsIsVector, start=c(2008,1), end=c(2012,12), frequency=12)
YearAsIs_2012 <- ts(YearAsIsVector, start=c(2008,1), end=c(2012,12), frequency=12)

CEPI_2012 <- ts(CEPIVector , start=c(2008,1), end=c(2012,12), frequency=12)
SIGov_2012 <- ts(SIGovVector , start=c(2008,1), end=c(2012,12), frequency=12)
Temperature_2012 <- ts(TemperatureVector, start=c(2008,1), end=c(2012,12), frequency=12)
Births_2012 <- ts(BirthsVector, start=c(2008,1), end=c(2012,12), frequency=12)
SIExtern_2012 <- ts(SIExternVector, start=c(2008,1), end=c(2012,12), frequency=12)
UrbanoExports_2012 <- ts(UrbanoExportsVector, start=c(2008,1), end=c(2012,12), frequency=12)
GlobalisationPartyMembers_2012 <- ts(GlobalisationPartyMembersVector, start=c(2008,1), end=c(2012,12), frequency=12)
AEPI_2012 <- ts(AEPIVector, start=c(2008,1), end=c(2012,12), frequency=12)
PPIEtel_2012 <- ts(PPIEtel, start=c(2008,1), end=c(2012,12), frequency=12)
NationalHolidays_2012 <- ts(NationalHolidaysVector, start=c(2008,1), end=c(2012,12), frequency=12)
ChulwalarIndex_2012 <- ts(ChulwalarIndexVector, start=c(2008,1), end=c(2012,12), frequency=12)
Inflation_2012 <- ts(InflationVector, start=c(2008,1), end=c(2012,12), frequency=12)
InfluenceNationalHolidays_2012 <- ts(InfluenceNationalHolidaysVector, start=c(2008,1), end=c(2012,12), frequency=12)

TotalAsIsVector_2013 <- c(ImportedAsIsData [2:13,7])
AsIsWugeAsIsVector_2013 <- c(ImportedAsIsData [16:27,7])
TotalAsIsGewuerzeVector_2013 <- c(ImportedAsIsData [30:41,7])
TotalEtelAsIsVector_2013 <- c(ImportedAsIsData [44:55,7])
BlueEtelAsIsVector_2013 <- c(ImportedAsIsData [58:69,7])
RedEtelAsIsVector_2013 <- c(ImportedAsIsData [72:83,7])
YearAsIsVector_2013 <- c(ImportedAsIsData [86,7])

TotalAsIs_2013 <- ts(TotalAsIsVector_2013, start=c(2013,1), end=c(2013,12), frequency=12)
EfakAsIs_2013 <- ts(AsIsWugeAsIsVector_2013, start=c(2013,1), end=c(2013,12), frequency=12)
WugeAsIs_2013 <- ts(TotalAsIsGewuerzeVector_2013, start=c(2013,1), end=c(2013,12), frequency=12)
TotalEtelAsIs_2013 <- ts(TotalEtelAsIsVector_2013, start=c(2013,1), end=c(2013,12), frequency=12)
BlueEtelAsIs_2013 <- ts(BlueEtelAsIsVector_2013, start=c(2013,1), end=c(2013,12), frequency=12)
RedEtelAsIs_2013 <- ts(RedEtelAsIsVector_2013, start=c(2013,1), end=c(2013,12), frequency=12)
YearAsIs_2013 <- ts(YearAsIsVector_2013, start=c(2013,1), end=c(2013,12), frequency=12)

PlanVector_2013 <- c(ImportedPlanData[2:13,7])
EfakPlanVector_2013 <- c(ImportedPlanData[16:27,7])
WugePlanVector_2013 <- c(ImportedPlanData[30:41,7])
TotalEtelPlanVector_2013 <- c(ImportedPlanData[44:55,7])
BlueEtelPlanVector_2013 <- c(ImportedPlanData[58:69,7])
RedEtelPlanVector_2013 <- c(ImportedPlanData[72:83,7])
YearPlanVector_2013 <- c(ImportedPlanData[86,7])

TotalPlan_2013 <- ts(PlanVector_2013, start=c(2013,1), end=c(2013,12), frequency=12)
EfakPlan_2013 <- ts(EfakPlanVector_2013, start=c(2013,1), end=c(2013,12), frequency=12)
WugePlan_2013 <- ts(WugePlanVector_2013, start=c(2013,1), end=c(2013,12), frequency=12)
TotalEtelPlan_2013 <- ts(TotalEtelPlanVector_2013, start=c(2013,1), end=c(2013,12), frequency=12)
BlueEtelPlan_2013 <- ts(BlueEtelPlanVector_2013, start=c(2013,1), end=c(2013,12), frequency=12)
RedEtelPlan_2013 <- ts(RedEtelPlanVector_2013, start=c(2013,1), end=c(2013,12), frequency=12)
YearPlan_2013 <- ts(YearPlanVector_2013, start=c(2013,1), end=c(2013,12), frequency=12)

CEPIVector_2013 <- c(ImportedIndicators[2:13,7])
CEPIVector_2014 <- c(ImportedIndicators[2:13,8])
SIGovVector_2013 <- c(ImportedIndicators[16:27,7])
SIGovVector_2014 <- c(ImportedIndicators[16:27,8])
TemperatureVector_2013 <- c(ImportedIndicators[30:41,7])
TemperatureVector_2014 <- c(ImportedIndicators[30:41,8])
BirthsVector_2013 <- c(ImportedIndicators[44:55,7])
BirthsVector_2014 <- c(ImportedIndicators[44:55,8])
SIExternVector_2013 <- c(ImportedIndicators[58:69,7])
SIExternVector_2014 <- c(ImportedIndicators[58:69,8])
UrbanoExportsVector_2013 <- c(ImportedIndicators[72:83,7])
UrbanoExportsVector_2014 <- c(ImportedIndicators[72:83,8])
GlobalisationPartyMembersVector_2013 <- c(ImportedIndicators[86:97,7])
GlobalisationPartyMembersVector_2014 <- c(ImportedIndicators[86:97,8])
AEPIVector_2013 <- c(ImportedIndicators[100:111,7])
AEPIVector_2014 <- c(ImportedIndicators[100:111,8])
PPIEtelVector_2013 <- c(ImportedIndicators[114:125,7])
PPIEtelVector_2014 <- c(ImportedIndicators[114:125,8])
NationalHolidaysVector_2013 <-c(ImportedIndicators[170:181,7])
NationalHolidaysVector_2014 <-c(ImportedIndicators[170:181,8])
ChulwalarIndexVector_2013 <- c(ImportedIndicators[128:139,7])
ChulwalarIndexVector_2014 <- c(ImportedIndicators[128:139,8])
InflationVector_2013 <- c(ImportedIndicators[142:153,7])
InflationVector_2014 <- c(ImportedIndicators[142:153,8])
InfluenceNationalHolidaysVector_2013 <-c(ImportedIndicators[184:195,7])
InfluenceNationalHolidaysVector_2014 <-c(ImportedIndicators[184:195,8])

CEPI_2013 <- ts(CEPIVector_2013 , start=c(2013,1), end=c(2013,12), frequency=12)
CEPI_2014 <- ts(CEPIVector_2014 , start=c(2013,1), end=c(2013,12), frequency=12)
SIGov_2013 <- ts(SIGovVector_2013 , start=c(2013,1), end=c(2013,12), frequency=12)
SIGov_2014 <- ts(SIGovVector_2014 , start=c(2013,1), end=c(2013,12), frequency=12)
Temperature_2013 <- ts(TemperatureVector_2013, start=c(2013,1), end=c(2013,12), frequency=12)
Temperature_2014 <- ts(TemperatureVector_2014, start=c(2013,1), end=c(2013,12), frequency=12)
Births_2013 <- ts(BirthsVector_2013, start=c(2013,1), end=c(2013,12), frequency=12)
Births_2014 <- ts(BirthsVector_2014, start=c(2013,1), end=c(2013,12), frequency=12)
SIExtern_2013 <- ts(SIExternVector_2013, start=c(2013,1), end=c(2013,12), frequency=12)
SIExtern_2014 <- ts(SIExternVector_2014, start=c(2013,1), end=c(2013,12), frequency=12)
UrbanoExports_2013 <- ts(UrbanoExportsVector_2013, start=c(2013,1), end=c(2013,12), frequency=12)
UrbanoExports_2014 <- ts(UrbanoExportsVector_2014, start=c(2013,1), end=c(2013,12), frequency=12)
GlobalisationPartyMembers_2013 <- ts(GlobalisationPartyMembersVector_2013, start=c(2013,1), end=c(2013,12), frequency=12)
GlobalisationPartyMembers_2014 <- ts(GlobalisationPartyMembersVector_2014, start=c(2013,1), end=c(2013,12), frequency=12)
AEPI_2013 <- ts(AEPIVector_2013, start=c(2013,1), end=c(2013,12), frequency=12)
AEPI_2014 <- ts(AEPIVector_2013, start=c(2013,1), end=c(2013,12), frequency=12)
PPIEtel_2013 <- ts(PPIEtelVector_2013, start=c(2013,1), end=c(2013,12), frequency=12)
PPIEtel_2014 <- ts(PPIEtelVector_2014, start=c(2013,1), end=c(2013,12), frequency=12)
NationalHolidays_2013 <- ts(NationalHolidaysVector_2013, start=c(2013,1), end=c(2013,12), frequency=12)
NationalHolidays_2014 <- ts(NationalHolidaysVector_2014, start=c(2014,1), end=c(2014,12), frequency=12)
ChulwalarIndex_2013 <- ts(ChulwalarIndexVector_2013, start=c(2013,1), end=c(2013,12), frequency=12)
ChulwalarIndex_2014 <- ts(ChulwalarIndexVector_2014, start=c(2013,1), end=c(2013,12), frequency=12)
Inflation_2013 <- ts(InflationVector_2013, start=c(2013,1), end=c(2013,12), frequency=12)
Inflation_2014 <- ts(InflationVector_2014, start=c(2013,1), end=c(2013,12), frequency=12)
InfluenceNationalHolidaysVector_2013 <- ts(InfluenceNationalHolidaysVector_2013, start=c(2013,1), end=c(2013,12), frequency=12)
InfluenceNationalHolidaysVector_2014 <- ts(InfluenceNationalHolidaysVector_2014, start=c(2013,1), end=c(2013,12), frequency=12)


#-------------------------------------------------------------------------------
#      Forecasting model: high correlation indicator                           -
#-------------------------------------------------------------------------------

## @knitr modelsForecastingWithHighCorrelIndicator
ModelWithHighCorrelatingIndicators_2012 <- tslm(TotalAsIs_2012 ~ trend + season + CEPI_2012 + SIExtern_2012 + UrbanoExports_2012 + GlobalisationPartyMembers_2012 + AEPI_2012)
summary(ModelWithHighCorrelatingIndicators_2012) 

ModelWithHighCorrelatingIndicators_Forecast <- forecast(ModelWithHighCorrelatingIndicators_2012,newdata=data.frame(CEPI_2012=CEPI_2013, SIExtern_2012=SIExtern_2013, UrbanoExports_2012= UrbanoExports_2013, GlobalisationPartyMembers_2012=GlobalisationPartyMembers_2013, AEPI_2012=AEPI_2013),h=12)
plot(ModelWithHighCorrelatingIndicators_Forecast, main="High Correlation Indicators' Model Forecast")
ModelWithHighCorrelatingIndicators_Forecast

ModelWithHighCorrelatingIndicators_Forecast_df <-as.data.frame(ModelWithHighCorrelatingIndicators_Forecast) 
ModelWithHighCorrelatingIndicators_PointForecast <- ts(ModelWithHighCorrelatingIndicators_Forecast_df$"Point Forecast", start=c(2013,1), end=c(2013,12), frequency=12)


cor(ModelWithHighCorrelatingIndicators_PointForecast, TotalAsIs_2013) 
cor(TotalAsIs_2013, TotalPlan_2013)

ModelWithHighCorrelatingIndicators_forecast_lm <- lm(TotalAsIs_2013 ~ ModelWithHighCorrelatingIndicators_PointForecast, data = TotalAsIs_2013)
TotalAsIs_2013_lm <- lm(TotalAsIs_2013 ~ TotalPlan_2013, data = TotalAsIs_2013)
summary(ModelWithHighCorrelatingIndicators_forecast_lm)
summary(TotalAsIs_2013_lm)



#-------------------------------------------------------------------------------
#      Forecasting model: Low correlation indicator                            -
#-------------------------------------------------------------------------------

## @knitr modelsForecastsWithLowCorrelIndicator
ModelWithLowCorrelatingIndicators_2012 <- tslm(TotalAsIs_2012 ~ trend + season + NationalHolidays_2012 + UrbanoExports_2012 + GlobalisationPartyMembers_2012)
summary(ModelWithLowCorrelatingIndicators_2012) 

ModelWithLowCorrelatingIndicators_Forecast <- forecast(ModelWithLowCorrelatingIndicators_2012,newdata=data.frame(NationalHolidays_2012=NationalHolidays_2013, UrbanoExports_2012= UrbanoExports_2013, GlobalisationPartyMembers_2012=GlobalisationPartyMembers_2013),h=12)
plot(ModelWithLowCorrelatingIndicators_Forecast, main="Forecast: Low Correlation Indicator")
ModelWithLowCorrelatingIndicators_Forecast

ModelWithLowCorrelatingIndicators_Forecast_df <-as.data.frame(ModelWithLowCorrelatingIndicators_Forecast) 
ModelWithLowCorrelatingIndicators_PointForecast <- ts(ModelWithLowCorrelatingIndicators_Forecast_df$"Point Forecast", start=c(2013,1), end=c(2013,12), frequency=12)

cor(ModelWithLowCorrelatingIndicators_PointForecast, TotalAsIs_2013) 
cor(TotalAsIs_2013, TotalPlan_2013)

ModelWithLowCorrelatingIndicators_forecast_lm <- lm(TotalAsIs_2013 ~ ModelWithLowCorrelatingIndicators_PointForecast, data = TotalAsIs_2013)
TotalAsIs_2013_lm <- lm(TotalAsIs_2013 ~ TotalPlan_2013, data = TotalAsIs_2013)
summary(ModelWithLowCorrelatingIndicators_forecast_lm)
summary(TotalAsIs_2013_lm)



#-------------------------------------------------------------------------------
#      Forecasting model: Trend And Seasonality                                -
#-------------------------------------------------------------------------------

## @knitr modelsForecastsWithTrendAndSeasonality
ModelWithTrendAndSeasonalityOnly_2012 <- tslm(TotalAsIs_2012 ~ trend + season)
summary(ModelWithTrendAndSeasonalityOnly_2012) 

ModelWithTrendAndSeasonalityOnly_Forecast <- forecast(ModelWithTrendAndSeasonalityOnly_2012,h=12)
plot(ModelWithTrendAndSeasonalityOnly_Forecast, main="Forecast: Trend And Seasonality Model")
ModelWithTrendAndSeasonalityOnly_Forecast

ModelWithTrendAndSeasonalityOnly_Forecast_df <-as.data.frame(ModelWithTrendAndSeasonalityOnly_Forecast) 
ModelWithTrendAndSeasonalityOnly_PointForecast <- ts(ModelWithTrendAndSeasonalityOnly_Forecast_df$"Point Forecast", start=c(2013,1), end=c(2013,12), frequency=12)

cor(ModelWithTrendAndSeasonalityOnly_PointForecast, TotalAsIs_2013) 
cor(TotalAsIs_2013, TotalPlan_2013)

ModelWithTrendAndSeasonalityOnly_Forecast_lm <- lm(TotalAsIs_2013 ~ ModelWithTrendAndSeasonalityOnly_PointForecast, data = TotalAsIs_2013)
TotalAsIs_2013_lm <- lm(TotalAsIs_2013 ~ TotalPlan_2013, data = TotalAsIs_2013)
summary(ModelWithTrendAndSeasonalityOnly_Forecast_lm)
summary(TotalAsIs_2013_lm)



#-------------------------------------------------------------------------------
#      Forecasting model: Efak Exports Indicators                              -
#-------------------------------------------------------------------------------

## @knitr modelsWithEfakeXportsIndicator
ModelWithEfakExportsIndicators_2012 <- tslm(EfakAsIs_2012 ~ trend + season + CEPI_2012 + UrbanoExports_2012 + AEPI_2012)
ModelEfakSalesWithCEPI_2012 <- tslm(EfakAsIs_2012 ~ trend + season + CEPI_2012)
ModelEfakSalesWithTrendAnsSeasonalityOnly_2012 <- tslm(EfakAsIs_2012 ~ trend + season)
ModelWithCEPIOnly_2012 <- tslm(EfakAsIs_2012 ~ CEPI_2012)
summary(ModelWithEfakExportsIndicators_2012) 
summary(ModelEfakSalesWithCEPI_2012) 
summary(ModelEfakSalesWithTrendAnsSeasonalityOnly_2012) 
summary(ModelWithCEPIOnly_2012)

ModelWithEfakExportsIndicators_Forecast <- forecast(ModelWithEfakExportsIndicators_2012, newdata=data.frame(CEPI_2012=CEPI_2013, UrbanoExports_2012 = UrbanoExports_2013, AEPI_2012 = AEPI_2013),h=12)
ModelEfakSalesWithCEPI_Forecast <- forecast(ModelEfakSalesWithCEPI_2012, newdata=data.frame(CEPI_2012=CEPI_2013), h=12)
ModelEfakSalesWithTrendAnsSeasonalityOnly_Forecast <- forecast(ModelEfakSalesWithTrendAnsSeasonalityOnly_2012,h=12)
ModelWithCEPIOnly_Forecast <- forecast(ModelWithCEPIOnly_2012, newdata=data.frame(CEPI_2012=CEPI_2013), h=12)

par(mfrow=c(2,2))

plot(ModelWithEfakExportsIndicators_Forecast, main="Model Efak Exports Indicators")
plot(ModelEfakSalesWithCEPI_Forecast, main="Model Efak Sales: CEPI Forecast")
plot(ModelEfakSalesWithTrendAnsSeasonalityOnly_Forecast, main="Model Efak Sales: Trend Ans Seasonality Forecast")
plot(ModelWithCEPIOnly_Forecast, main="Model With CEPI Forecast")

ModelWithEfakExportsIndicators_Forecast_df <-as.data.frame(ModelWithEfakExportsIndicators_Forecast) 
ModelEfakSalesWithCEPI_Forecast_df <-as.data.frame(ModelEfakSalesWithCEPI_Forecast) 
ModelEfakSalesWithTrendAnsSeasonalityOnly_Forecast_df <-as.data.frame(ModelEfakSalesWithTrendAnsSeasonalityOnly_Forecast) 
ModelWithCEPIOnly_Forecast_df <-as.data.frame(ModelWithCEPIOnly_Forecast) 
ModelWithEfakExportsIndicators_PointForecast <- ts(ModelWithEfakExportsIndicators_Forecast_df$"Point Forecast", start=c(2013,1), end=c(2013,12), frequency=12)
ModelEfakSalesWithCEPI_PointForecast <- ts(ModelEfakSalesWithCEPI_Forecast_df$"Point Forecast", start=c(2013,1), end=c(2013,12), frequency=12)
ModelEfakSalesWithTrendAnsSeasonalityOnly_PointForecast <- ts(ModelEfakSalesWithTrendAnsSeasonalityOnly_Forecast_df$"Point Forecast", start=c(2013,1), end=c(2013,12), frequency=12)
ModelWithCEPIOnly_PointForecast <- ts(ModelWithCEPIOnly_Forecast_df$"Point Forecast", start=c(2013,1), end=c(2013,12), frequency=12)

cor(ModelWithEfakExportsIndicators_PointForecast, EfakAsIs_2013) 
cor(ModelEfakSalesWithCEPI_PointForecast, EfakAsIs_2013) 
cor(ModelEfakSalesWithTrendAnsSeasonalityOnly_PointForecast, EfakAsIs_2013) 
cor(ModelWithCEPIOnly_PointForecast, EfakAsIs_2013)
cor(EfakAsIs_2013, EfakPlan_2013)




#-------------------------------------------------------------------------------
#      Forecasting model: Wuge Exports Indicators                              -
#-------------------------------------------------------------------------------

## @knitr modelsWithWugeXportIndicator
ModelWithWugeExportsIndicators_2012 <- tslm(WugeAsIs_2012 ~ trend + season + CEPI_2012 + UrbanoExports_2012 + AEPI_2012 + GlobalisationPartyMembers_2012)
ModelWugeWithCEPI_2012 <- tslm(WugeAsIs_2012 ~ trend + season + CEPI_2012)
ModelWugeWithTrendAndSeasonalityOnly_2012 <- tslm(WugeAsIs_2012 ~ trend + season)
summary(ModelWithWugeExportsIndicators_2012) 
summary(ModelWugeWithCEPI_2012) 
summary(ModelWugeWithTrendAndSeasonalityOnly_2012) 

ModelWithWugeExportsIndicators_Forecast <- forecast(ModelWithWugeExportsIndicators_2012, newdata=data.frame(CEPI_2012=CEPI_2013, UrbanoExports_2012 = UrbanoExports_2013, AEPI_2012 = AEPI_2013, GlobalisationPartyMembers_2012 = GlobalisationPartyMembers_2013),h=12)
ModelWugeWithCEPI_Forecast <- forecast(ModelWugeWithCEPI_2012, newdata=data.frame(CEPI_2012=CEPI_2013), h=12)
ModelWugeWithTrendAndSeasonalityOnly_Forecast <- forecast(ModelWugeWithTrendAndSeasonalityOnly_2012,h=12)

plot(ModelWithWugeExportsIndicators_Forecast, main="Model: Wuge Exports Indicators")
plot(ModelWugeWithCEPI_Forecast, main="Model: Wuge EPI Forecast")
plot(ModelWugeWithTrendAndSeasonalityOnly_Forecast, main="Model: Wuge Trend And Seasonality Forecast")

ModelWithWugeExportsIndicators_Forecast_df <-as.data.frame(ModelWithWugeExportsIndicators_Forecast) 
ModelWugeWithCEPI_Forecast_df <-as.data.frame(ModelWugeWithCEPI_Forecast) 
ModelWugeWithTrendAndSeasonalityOnly_Forecast_df <-as.data.frame(ModelWugeWithTrendAndSeasonalityOnly_Forecast)
ModelWithWugeExportsIndicators_PointForecast <- ts(ModelWithWugeExportsIndicators_Forecast_df$"Point Forecast", start=c(2013,1), end=c(2013,12), frequency=12)
ModelWugeWithCEPI_PointForecast <- ts(ModelWugeWithCEPI_Forecast_df$"Point Forecast", start=c(2013,1), end=c(2013,12), frequency=12)
ModelWugeWithTrendAndSeasonalityOnly_PointForecast <- ts(ModelWugeWithTrendAndSeasonalityOnly_Forecast_df$"Point Forecast", start=c(2013,1), end=c(2013,12), frequency=12)

cor(ModelWithWugeExportsIndicators_PointForecast, WugeAsIs_2013) 
cor(ModelWugeWithCEPI_PointForecast, WugeAsIs_2013) 
cor(ModelWugeWithTrendAndSeasonalityOnly_PointForecast, WugeAsIs_2013) 
cor(WugeAsIs_2013, WugePlan_2013)




#-------------------------------------------------------------------------------
#      Forecasting model: Total Etel                                           -
#-------------------------------------------------------------------------------

## @knitr forcastModelTotalEtel
ModelTotalEtel_2012 <- tslm(TotalEtelAsIs_2012 ~ trend + season)
ModelBlueEtel_2012 <- tslm(BlueEtelAsIs_2012 ~ trend + season)
ModelRedEtel_2012 <- tslm(RedEtelAsIs_2012 ~ trend + season)
summary(ModelTotalEtel_2012) 
summary(ModelBlueEtel_2012) 
summary(ModelRedEtel_2012) 

ModelTotalEtel_Forecast <- forecast(ModelTotalEtel_2012,h=12)
ModelBlueEtel_Forecast <- forecast(ModelBlueEtel_2012,h=12)
ModelRedEtel_Forecast <- forecast(ModelRedEtel_2012,h=12)

plot(ModelTotalEtel_Forecast,main="Model: Total Etel Forecast")
plot(ModelBlueEtel_Forecast,main="Model: Blue Etel Forecast")
plot(ModelRedEtel_Forecast,main="Model: Red Etel Forecast")

ModelTotalEtel_Forecast_df <-as.data.frame(ModelTotalEtel_Forecast) 
ModelBlueEtel_Forecast_df <-as.data.frame(ModelBlueEtel_Forecast) 
ModelRedEtel_Forecast_df <-as.data.frame(ModelRedEtel_Forecast) 
ModelTotalEtel_PointForecast <- ts(ModelTotalEtel_Forecast_df$"Point Forecast", start=c(2013,1), end=c(2013,12), frequency=12)
ModelBlueEtel_PointForecast <- ts(ModelBlueEtel_Forecast_df$"Point Forecast", start=c(2013,1), end=c(2013,12), frequency=12)
ModelRedEtel_PointForecast <- ts(ModelRedEtel_Forecast_df$"Point Forecast", start=c(2013,1), end=c(2013,12), frequency=12)

cor(ModelTotalEtel_PointForecast, TotalEtelAsIs_2013) 
cor(TotalEtelPlan_2013, TotalEtelAsIs_2013)
cor(ModelBlueEtel_PointForecast, BlueEtelAsIs_2013) 
cor(BlueEtelPlan_2013, BlueEtelAsIs_2013)
cor(ModelRedEtel_PointForecast, RedEtelAsIs_2013) 
cor(RedEtelPlan_2013, RedEtelAsIs_2013)


#-------------------------------------------------------------------------------
#      Forecasting model: Total Urban exports                                  -
#-------------------------------------------------------------------------------

## @knitr forcastModelWithTotalUrbanoExports
ModelWithTotalUrbanoExports_2012 <- tslm(TotalAsIs_2012 ~ trend + season + UrbanoExports_2012)
summary(ModelWithTotalUrbanoExports_2012) 

ModelWithTotalUrbanoExports_Forecast <- forecast(ModelWithTotalUrbanoExports_2012, newdata=data.frame(UrbanoExports_2012=UrbanoExports_2013), h=12)
plot(ModelWithTotalUrbanoExports_Forecast,main="Model: Total Urbano Exports Forecast")
ModelWithTotalUrbanoExports_Forecast

ModelWithTotalUrbanoExports_Forecast_df <-as.data.frame(ModelWithTotalUrbanoExports_Forecast) 
ModelWithTotalUrbanoExports_PointForecast <- ts(ModelWithTotalUrbanoExports_Forecast_df$"Point Forecast", start=c(2013,1), end=c(2013,12), frequency=12)

cor(ModelWithTotalUrbanoExports_PointForecast, TotalAsIs_2013) 
cor(TotalAsIs_2013, TotalPlan_2013)


#-------------------------------------------------------------------------------
#      Forecasting model: National Holidays                                    -
#-------------------------------------------------------------------------------

## @knitr forcastModelWithNationalHolidays
ModelWithNationalHolidays_2012 <- tslm(TotalAsIs_2012 ~ trend + season + NationalHolidays_2012)
summary(ModelWithNationalHolidays_2012) 

ModelWithNationalHolidays_Forecast <- forecast(ModelWithNationalHolidays_2012, newdata=data.frame(NationalHolidays_2012=NationalHolidays_2013), h=12)
plot(ModelWithNationalHolidays_Forecast,main="Model: National Holidays Forecast")
ModelWithNationalHolidays_Forecast

ModelWithNationalHolidays_Forecast_df <-as.data.frame(ModelWithNationalHolidays_Forecast) 
ModelWithNationalHolidays_PointForecast <- ts(ModelWithNationalHolidays_Forecast_df$"Point Forecast", start=c(2013,1), end=c(2013,12), frequency=12)

cor(ModelWithNationalHolidays_PointForecast, TotalAsIs_2013) 


#-------------------------------------------------------------------------------
#      Forecasting model: Influencing National Holidays                        -
#-------------------------------------------------------------------------------

## @knitr forcastModelWithInfluenceNationalHolidays
ModelWithInfluenceNationalHolidays_2012 <- tslm(TotalAsIs_2012 ~ trend + season + InfluenceNationalHolidays_2012)
summary(ModelWithInfluenceNationalHolidays_2012) 

ModelWithInfluenceNationalHolidays_Forecast <- forecast(ModelWithInfluenceNationalHolidays_2012, newdata=data.frame(InfluenceNationalHolidays_2012=InfluenceNationalHolidaysVector_2013), h=12)
plot(ModelWithInfluenceNationalHolidays_Forecast,main="Model: Influencing National Holidays Forecast")
ModelWithInfluenceNationalHolidays_Forecast

ModelWithInfluenceNationalHolidays_Forecast_df <-as.data.frame(ModelWithInfluenceNationalHolidays_Forecast) 
ModelWithInfluenceNationalHolidays_PointForecast <- ts(ModelWithInfluenceNationalHolidays_Forecast_df$"Point Forecast", start=c(2013,1), end=c(2013,12), frequency=12)

cor(ModelWithInfluenceNationalHolidays_PointForecast, TotalAsIs_2013) 
cor(TotalAsIs_2013, TotalPlan_2013)


#-------------------------------------------------------------------------------
#      Forecast: 2014                                                           -
#-------------------------------------------------------------------------------

## @knitr forcastfor2014
IndicatorsCorrelationCoefficientMatrix

ModelWithLowCorrelatingIndicators <- tslm(TotalAsIs ~ trend + season + NationalHolidays + UrbanoExports + GlobalisationPartyMembers)
summary(ModelWithLowCorrelatingIndicators) 

ModelWithTrendAndSeasonalityOnly <- tslm(TotalAsIs ~ trend + season)
summary(ModelWithTrendAndSeasonalityOnly)

ModelWithoutTrendAndSeasonality <- tslm(TotalAsIs ~ CEPI + SIExtern + UrbanoExports + GlobalisationPartyMembers + AEPI)
summary(ModelWithoutTrendAndSeasonality)

summary(ModelWithLowCorrelatingIndicators) 
Forecast_ModelWithLowCorrelatingIndicators_2014 <- forecast(ModelWithLowCorrelatingIndicators,newdata=data.frame(NationalHolidays=NationalHolidays_2014, UrbanoExports= UrbanoExports_2014, GlobalisationPartyMembers=GlobalisationPartyMembers_2014),h=12)
plot(Forecast_ModelWithLowCorrelatingIndicators_2014, main="Forecast_2014")
Forecast_ModelWithLowCorrelatingIndicators_2014_df <-as.data.frame(Forecast_ModelWithLowCorrelatingIndicators_2014) 
PointForecast_ModelWithLowCorrelatingIndicators_2014 <- ts(Forecast_ModelWithLowCorrelatingIndicators_2014_df$"Point Forecast", start=c(2014,1), end=c(2014,12), frequency=12)
PointForecast_ModelWithLowCorrelatingIndicators_2014

cor(TotalAsIs_2014,TotalPlan_2014)
cor(TotalAsIs_2014,PointForecast_ModelWithLowCorrelatingIndicators_2014)

summary(ModelWithTrendAndSeasonalityOnly) 
Forecast_2014 <- forecast(ModelWithTrendAndSeasonalityOnly,h=12)
plot(Forecast_2014, main="2014 Forecast")

Forecast_2014_df <-as.data.frame(Forecast_2014) 
PointForecast_TrendAndSeasonality_2014 <- ts(Forecast_2014_df$"Point Forecast", start=c(2014,1), end=c(2014,12), frequency=12)

cor(TotalAsIs_2014,TotalPlan_2014)
cor(TotalAsIs_2014,PointForecast_TrendAndSeasonality_2014)

summary(ModelWithNationalHolidays)
Forecast_2014_alternative <- forecast(ModelWithNationalHolidays, newdata=data.frame(NationalHolidays=NationalHolidays_2014),h=12)
plot(Forecast_2014_alternative,main="2014 Altertive Forecast")

Forecast_2014_alternative_df <-as.data.frame(Forecast_2014_alternative) 
PointForecast_2014_alternative <- ts(Forecast_2014_alternative_df$"Point Forecast", start=c(2014,1), end=c(2014,12), frequency=12)


#-------------------------------------------------------------------------------
#      Forecasting model: Simple expontential smoothing                        -
#-------------------------------------------------------------------------------

## @knitr SimpleXponentialSmoothing
Model_ses <- ses(TotalAsIs, h=12)
summary(Model_ses)
plot(Model_ses)

plot(Model_ses, plot.conf=FALSE, ylab="Exports Chulwalar  )", xlab="Year", main="", fcol="white", type="o")
lines(fitted(Model_ses), col="green", type="o")
lines(Model_ses$mean, col="blue", type="o")
legend("topleft",lty=1, col=c(1,"green"), c("data", expression(alpha == 0.671)),pch=1)



#-------------------------------------------------------------------------------
#      Forecasting model: Efak Exports Indicators                              -
#-------------------------------------------------------------------------------

## @knitr holtLinearTrendMethod
Model_holt_1 <- holt(TotalAsIs,h=12)
summary(Model_holt_1)
plot(Model_holt_1)

Model_holt_2<- holt(TotalAsIs, exponential=TRUE,h=12)
summary(Model_holt_2)
plot(Model_holt_2)


Model_holt_3 <- holt(TotalAsIs, damped=TRUE,h=12)
summary(Model_holt_3)
plot(Model_holt_3)


Model_holt_4 <- holt(TotalAsIs, exponential=TRUE, damped=TRUE,h=12)
summary(Model_holt_4)
plot(Model_holt_4)


plot(Model_holt_1$model$state)
plot(Model_holt_2$model$state)
plot(Model_holt_3$model$state)
plot(Model_holt_4$model$state)

plot(Model_holt_1, plot.conf=FALSE, ylab="Exports Chulwalar  )", xlab="Year", main="", fcol="white", type="o")
lines(fitted(Model_ses), col="purple", type="o")
lines(fitted(Model_holt_1), col="blue", type="o")
lines(fitted(Model_holt_2), col="red", type="o")
lines(fitted(Model_holt_3), col="green", type="o")
lines(fitted(Model_holt_4), col="orange", type="o")
lines(Model_ses$mean, col="purple", type="o")
lines(Model_holt_1$mean, col="blue", type="o")
lines(Model_holt_2$mean, col="red", type="o")
lines(Model_holt_3$mean, col="green", type="o")
lines(Model_holt_4$mean, col="orange", type="o")
legend("topleft",lty=1, col=c(1,"purple","blue","red","green","orange"), c("data", "SES","Holts auto", "Exponential", "Additive Damped", "Multiplicative Damped"),pch=1)


#-------------------------------------------------------------------------------
#      Forecasting model: Efak Exports Indicators                              -
#-------------------------------------------------------------------------------

## @knitr holtWinterSeasonalMethod
Model_hw_1 <- hw(TotalAsIs ,seasonal="additive",h=12)
summary(Model_hw_1)
plot(Model_hw_1)

Model_hw_2 <- hw(TotalAsIs ,seasonal="multiplicative",h=12)
summary(Model_hw_2)
plot(Model_hw_2)

plot(Model_hw_1, ylab="Exports Chulwalar  ", plot.conf=FALSE, type="o", fcol="white", xlab="Year")
lines(fitted(Model_hw_1), col="red", lty=2)
lines(fitted(Model_hw_2), col="green", lty=2)
lines(Model_hw_1$mean, type="o", col="red")
lines(Model_hw_2$mean, type="o", col="green")
legend("topleft",lty=1, pch=1, col=1:3, c("data","Holt Winters' Additive","Holt Winters' Multiplicative"))

Model_hw_1_df <-as.data.frame(Model_hw_1) 
Model_hw_1_PointForecast <- ts(Model_hw_1_df$"Point Forecast", start=c(2014,1), end=c(2014,12), frequency=12)
Model_hw_2_df <-as.data.frame(Model_hw_2) 
Model_hw_2_PointForecast <- ts(Model_hw_2_df$"Point Forecast", start=c(2014,1), end=c(2014,12), frequency=12)

