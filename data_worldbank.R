# World Bank Data

# install.packages("WDI")
library(WDI)


# Searching available datasets
new_cache = WDIcache()
WDIsearch('population', cache=new_cache)


# CO2 (Yearly)
emissions <- WDI(indicator='EN.ATM.CO2E.KT')  
intensity <- WDI(indicator = "EN.ATM.CO2E.EG.ZS") # per kg of oil use
# "EN.ATM.CO2E.PC" - CO2 metric ton per capita


# Filters example 
gdp <- WDI(indicator = "5.51.01.10.gdp", country = c('US'), start = '2012M01', end = '2020M08')



# Other codes of interest (from https://data.worldbank.org/indicator): 

# Demographics 
# "EG.ELC.ACCS.ZS" - "Access to electricity (% of population)"
# "EN.POP.DNST" - "Population density (people per sq. km of land area)"    
# "IN.EC.POP.TOTL" - "Population (Thousands)"  
# "IN.EC.POP.RURL.PCT"  - "Population, Rural (%)"                                                                                                                                                                                                                                              
# "IN.EC.POP.URBN.PCT" - "Population, Urban (%)" 
# "SP.POP.GROW" - Population Growth

# "VC.IDP.NWDS" - Displacement from disasters 
# "SM.POP.REFG.OR" - Refugee population 
# "DT.ODA.ODAT.GN.ZS" - Net official development assistance (ODA) received (% of GNI)

# Agriculture 
# "AG.YLD.CREL.KG" - Cereal Yield 
# "AG.LND.AGRI.ZS" - Agricultural Land 

# Energy and Renewables 
# "EG.USE.PCAP.KG.OE" - Energy Use (kg oil equivalent per capita)
# "EG.USE.COMM.FO.ZS" - Fossil fuel energy consumption (% of total)
# "EG.ELC.RNWX.KH" - Electricity production from renewables (kWh)
# "EG.ELC.NUCL.ZS" - Electricity production from nuclear (% of total)
# "EG.ELC.RNEW.ZS" - Renewable electricity output (% of total)


# Income inequality - patchy depending on country 
# "SI.DST.10TH.10" - Income held by top 10%
# "SI.DST.FRST.10" - Income held by bottom 10%
# "SI.POV.GAPS" - Poverty gap 
# "SI.POV.GINI" - GINI index (World Bank Estimate of inequality)
# "SI.POV.NAHC" - Poverty headcount 

