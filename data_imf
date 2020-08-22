# IMF Social Economic Data #####

# install.packages('IMFData')
library(IMFData)

# Search parameters 
database_id <- "IFS"
start_date <- "2000-01-01"
end_date <- "2020-06-30"

# Get dimension code of IFS dataset
IFS_codes <- DataStructureMethod("IFS")

# Available dimension code
names(IFS_codes)

# Find Geographic Codes
IFS_codes[[2]] %>% 
  filter(CodeText == 'United States')

# Search economic indicator 
CodeSearch(IFS_codes, "CL_INDICATOR_IFS", "employment")


# NGDP_R_XDC - GDP: Real, Domestic Currency
query_filter <- list(CL_FREA = "", 
                    CL_AREA_IFS = "US", 
                    CL_INDICATOR_IFS = c("NGDP_XDC"))
gdp_query <- CompactDataMethod(database_id, query_filter, start_date, end_date, FALSE)

# Available time series
gdp_query[, 1:5]


# Observations 
gdp <- gdp_query$Obs[[2]]
colnames(gdp)[2] <- "GDP"
head(gdp)

# LUR_PT - Unemployment Rate
query_filter <- list(CL_FREA = "", 
                     CL_AREA_IFS = "US", 
                     CL_INDICATOR_IFS = c("LUR_PT"))
unemp_query <- CompactDataMethod(database_id, query_filter, start_date, end_date, FALSE)
unemp_query[, 1:5]

unemployment <- unemp_query$Obs[[2]]
colnames(unemployment)[2] <- "UR"


# PCPI_IX - Consumer Price Index 
query_filter <- list(CL_FREA = "", 
                     CL_AREA_IFS = "US", 
                     CL_INDICATOR_IFS = c("PCPI_IX"))
cpi_query <- CompactDataMethod(database_id, query_filter, start_date, end_date, FALSE)
cpi_query[, 1:5]

cpi <- cpi_query$Obs[[1]]
colnames(cpi)[2] <- "CPI"


# Merge Datasets 
data_sets <- list(gdp, cpi, unemployment)
df <- Reduce(function(x, y) merge(x, y, by = "@TIME_PERIOD", all.x = TRUE), data_sets)
df["COUNTRY"] <- "US"


