# install.packages("Quandl")
library(Quandl)

# Note - 50 calls allowed per day w/o API key. Set up an API key on quandl.com and run:
# Quandl.api_key("YOUR_API_KEY_HERE")

# FRED (Federal Reserve Economic Data)

# Quarterly
gdp <- Quandl("FRED/GDP")

# Monthly 
cpi <- Quandl("FRED/CPIAUCSL")        # CPI
unem_rate <- Quandl("FRED/UNRATE")    # Unemployment Rate
part_rate <- Quandl("FRED/CIVPART")   # Participation Rate
nonfarm <- Quandl("FRED/PAYEMS")      # Non-farm Payrolls
savings <- Quandl("FRED/PSAVERT")     # Personal Saving Rate
disp_income <- Quandl("FRED/DSPIC96") # Real Disposable Personal Income

# Weekly
initial <- Quandl("FRED/ICSA")      # Initial Claims 

 
# Wiki Continuous Futures
# CHRIS/{EXCHANGE}_{CODE}{NUMBER}
# {NUMBER} = "depth", use 1 for front month contract

# Search contracts at:  
# https://www.quandl.com/data/CHRIS-Wiki-Continuous-Futures?keyword=
corn <- Quandl("CHRIS/LIFFE_EMA1")


# FX Rates - Bank of England
# Codes at https://blog.quandl.com/api-for-currency-data
usdaud <- Quandl("BOE/XUDLADD")


