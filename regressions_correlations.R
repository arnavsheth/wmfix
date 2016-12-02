# Computing 
# 1. Equity Returns & Volatility
# 2. FX Returns & Volatility
# 3. Regressions of Equity & FX
# 4. Regressions of VIX & Momentum
# 5. Correlations of Equity & FX

# Template to install any package
# install.packages('dplyr', dependencies=TRUE, repos='http://cran.rstudio.com/')

# Import required libraries after installation using above command template
suppressWarnings(library(xts))
suppressWarnings(library(quantmod))
suppressWarnings(library(plyr))
suppressWarnings(library(dplyr))
suppressWarnings(library(magrittr))

# Import FX Data files downloaded from HistData
setwd("/Users/SaranyaGanapa/Dropbox/vishu-sharu/R/HistData/1minute-2010-15/")

# Use same start and end years for FX and equity
# Note : Runtime 5minutes / year
startYear <- 2012
endYear <- 2015
version <- 3.2

# ===========================
# ===========================
# ===========================
# =========EQUITY============
# ===========================
# ===========================

# Helper function to fetch equity data
# Fetch Equity data from Yahoo Finance using quantmod library
getDataForSymbols <- function(tickers, dataSource) {
  for (i in 1:length(tickers)) {
    cat(tickers[i],i,"\n")
    
    # getSymbols will save the fetched data in a variable automatically.
    getSymbols(tickers[i],
               src = dataSource,
               auto.assign = getOption("getSymbols.auto.assign",TRUE),
               env = parent.frame())
  }
}

# Equity : 21 days Rolling returns & Standard deviation
getEquityReturnsAndStdDev <- function() {
  # convert to data frame
  equity <<- data.frame(date=index(equity), coredata(equity))
  colnames(equity) <- equityColumnNames
  
  equityTemp <- equity %>%
    mutate(Year = as.numeric(substr(as.character(Date), 1, 4))) %>%
    mutate(Month = as.numeric(substr(as.character(Date), 6, 7))) %>%
    mutate(Day = as.numeric(substr(as.character(Date), 9, 10)))
  equity <- equityTemp
  rownames(equity) <- NULL
  
  # Required data only from startYear-endYear
  equityTemp <- equity %>%
    filter(Year>=startYear, Year<=endYear) %>%
    select(Date, Open, High, Low, Close, Volume, Year, Month, Day) %>%
    mutate(Date = as.numeric(paste(substr(as.character(Date), 1, 4),
                                   substr(as.character(Date), 6, 7),
                                   substr(as.character(Date), 9, 10)
                                   ,sep="")))
  equity <- equityTemp

  # Creating empty Vectors - will fill in values later
  equityReturns <- data.frame()
  equityDailyReturns <- data.frame()
  equityDailyStandardDev <- data.frame()
  
  # Start from 22nd day
  for(i in 22:length(unique(equity$Date))) {
    equityReturns[i, 'Equity_Returns'] <- as.numeric((equity$Open[i] - equity$Open[i-21]) / equity$Open[i-21]);
    equityReturns[i, 'Date'] <- as.numeric(equity$Date[i]);
  }
  
  tail(equityReturns)
  # Equity Standard Deviation-Volatility
  # Step 1 : Daily returns for equity
  for(i in 2:length(unique(equity$Date))) {
    equityDailyReturns[i, 'Equity_Daily_Returns'] <- as.numeric((equity$Open[i] - equity$Open[i-1]) / equity$Open[i-1]);
    equityDailyReturns[i, 'Date'] <- as.numeric(equity$Date[i]);
  }
  
  # Step 2- Rolling 21 Day Standard Deviation for equity
  for(i in 22:length(unique(equityDailyReturns[, 'Date']))) {
    startIndex <- i-21
    equityDailyStandardDev[i, 'Equity_Standard_Deviation'] <- sd(equityDailyReturns[startIndex:i, 'Equity_Daily_Returns'])
    equityDailyStandardDev[i, 'Date'] <- as.numeric(equity$Date[i]);
  }
  # Create returns and standard deviation data frame
  equityDF <- merge(equityReturns, equityDailyStandardDev, by="Date")
  
  print("Returning equityDF from func")
  print(head(equityDF))

  return(equityDF)
}

# Fetch Equity data by Symbols
equitySource <- c("yahoo")
equityIndices <- c("^GSPC", "^GDAXI", "^SSMI", "^N225", "^GSPTSE", "^AORD", "^NZ50", "^OMX", "^FTSE", "^MXX")
equityColumnNames <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
suppressWarnings(getDataForSymbols(equityIndices, equitySource))

# Compute returns
equity <- GSPC
GSPC_RV <- getEquityReturnsAndStdDev()
equity <- GDAXI
GDAXI_RV <- getEquityReturnsAndStdDev()
equity <- SSMI
SSMI_RV <- getEquityReturnsAndStdDev()
equity <- N225
N225_RV <- getEquityReturnsAndStdDev()
equity <- GSPTSE
GSPTSE_RV <- getEquityReturnsAndStdDev()
equity <- AORD
AORD_RV <- getEquityReturnsAndStdDev()
equity <- NZ50
NZ50_RV <- getEquityReturnsAndStdDev()
equity <- OMX
OMX_RV <- getEquityReturnsAndStdDev()
equity <- FTSE
FTSE_RV <- getEquityReturnsAndStdDev()
equity <- MXX
MXX_RV <- getEquityReturnsAndStdDev()

# ===========================
# ===========================
# ===========================
# ============FX=============
# ===========================
# ===========================


# FX Data Helpers
# Read data from 4 years and merge into one data frame
getCurrencyMergedData <- function(currencyPair) {
  mergedCurrencyPair <- data.frame()
  for(i in startYear:endYear) {''
    dynamicYearCurrencyPair <- read.table(paste(currencyPair, "/HISTDATA_COM_ASCII_", currencyPair, "_M1", i, "/DAT_ASCII_", currencyPair, "_M1_", i, ".csv", sep=""), sep=";")
    mergedCurrencyPair <- rbind(mergedCurrencyPair, dynamicYearCurrencyPair)
  }
  
  return(mergedCurrencyPair)
}

formatDateTime <- function(currencyPairData) {
  colnames(currencyPairData) <- c("Date", "Open", "High", "Low", "Close", "Volume");
  
  # Format Date column into Time, Year, Month, Day
  # Example Date column initial value : 20120102 020000
  currencyPairTemp  <- currencyPairData %>%
    select(Date, Open, High, Low, Close) %>%
    mutate(Time = as.numeric(ldply(strsplit(as.character(Date), " "))[, 2])) %>%
    mutate(Date = as.numeric(ldply(strsplit(as.character(Date), " "))[, 1])) %>%
    mutate(Year = as.numeric(substr(as.character(Date), 1, 4))) %>%
    mutate(Month = as.numeric(substr(as.character(Date), 5, 6))) %>%
    mutate(Day = as.numeric(substr(as.character(Date), 7, 8)))
  currencyPairData <- currencyPairTemp

  return(currencyPairData)
}

getFXReturnsAndStdDev <- function(currencyPair) {
  # get csv's merged from startyear till endyear
  currencyPairData <- getCurrencyMergedData(currencyPair)
  # format the datetime string to separate columns
  currencyPairData <- formatDateTime(currencyPairData)
  
  print('FX data fetched, merged & formatted')
  
  # PART 1 : Compute FX Returns
  # Group by Date and compute (11am - 10am / 10am)
  # To handle discrepancy between count of 10oClock values & 11oClock values
  # Take 10oClocks separately, divide and update fxReturns values
  # Group by Date and subtract 10oClock from 11oClock
  fxReturns <- currencyPairData %>%
    select(everything()) %>%
    filter(Time >= 100000 & Time <= 110000) %>%
    group_by(Date) %>%
    summarise(fxReturns = (sum(Close[which(Time == 110000)], -Close[which(Time == 100000)])))
  
  # Get only 10oClock rows
  fxReturnsTen <- currencyPairData %>%
    select(everything()) %>%
    filter(Time == 100000)
  
  # merge by Date to ignore the dates where either 11/10oClock are missing
  mergedFxReturns <- merge(fxReturns, fxReturnsTen, by="Date")
  # To the Grouped by Date values divide by 10oClock to get fxReturns
  fxReturns <- mergedFxReturns %>%
    mutate(Date = Date, fxReturns = fxReturns / Close) %>%
    select(Date, fxReturns)

  colnames(fxReturns) <- c("Date", "FX_Returns")

  tail(fxReturns)
  
  # PART 2 : Compute FX Standard Deviation
  # STEP 1 : Compute 5 minute rolling returns for time between 10am - 11:30am
  fx5MinuteRollingReturns <- c(1)
  fx5MinuteRollingReturnsDate <- c(1)
  
  currencyPair10_1130 <- currencyPairData %>% filter(Time >= 095600 & Time <= 113000)
  head(currencyPair10_1130)
  print(paste("Computing 5 minute rolling returns for FXSD...Row Count : ", length(currencyPair10_1130$Time)), sep="")
  currencyPair10_1130_Close <- currencyPair10_1130$Close
  currencyPair10_1130_Date <- currencyPair10_1130$Date
  currencyPair10_1130_Time <- currencyPair10_1130$Time
  for(i in 1:length(currencyPair10_1130$Time)) {
    if(currencyPair10_1130_Time[i] < 100000) {
      # Store zeros from (9:56am - 9:59am)
      fx5MinuteRollingReturns[i] <- 0
      fx5MinuteRollingReturnsDate[i] <- 0
    } else {
      fiveMinuteRollingClose <- currencyPair10_1130_Close[i-4]
      fx5MinuteRollingReturns[i] <- (currencyPair10_1130_Close[i] - fiveMinuteRollingClose) / fiveMinuteRollingClose
      fx5MinuteRollingReturnsDate[i] <- currencyPair10_1130_Date[i]
    }
  }

  fx5MinuteRollingReturnsDF <- data.frame(fx5MinuteRollingReturnsDate)
  fx5MinuteRollingReturnsDF$Five_Minute_Returns <- fx5MinuteRollingReturns
  colnames(fx5MinuteRollingReturnsDF) <- c("Date", "Five_Minute_Returns")
  head(fx5MinuteRollingReturnsDF)

  # STEP 2 : Standard deviation from 5 minute rolling returns
  # Group by Date and compute standard deviation from 5 minute returns between 10am - 11:30am
  fxStandardDeviation <- data.frame()
  fxDFReturnsStandardDeviation <- data.frame()
  
  fxStandardDeviation<- fx5MinuteRollingReturnsDF %>%
    group_by(Date) %>%
    summarise(FX_Standard_Deviation = sd(as.numeric(Five_Minute_Returns)))

  # Merge FX Returns and FX Standard deviation data frames
  fxDFReturnsStandardDeviation <- merge(fxReturns, fxStandardDeviation, by="Date")
  print("Tail : FX 5 minute rolling return + SD")
  print(tail(fxDFReturnsStandardDeviation))
  
  return(fxDFReturnsStandardDeviation)
}

# Get FX Returns data for 10 currencies
EURUSD_RV <- getFXReturnsAndStdDev("EURUSD")
USDCHF_RV <- getFXReturnsAndStdDev("USDCHF")
USDJPY_RV <- getFXReturnsAndStdDev("USDJPY")
USDCAD_RV <- getFXReturnsAndStdDev("USDCAD")
AUDUSD_RV <- getFXReturnsAndStdDev("AUDUSD")
NZDUSD_RV <- getFXReturnsAndStdDev("NZDUSD")
USDSEK_RV <- getFXReturnsAndStdDev("USDSEK")
USDNOK_RV <- getFXReturnsAndStdDev("USDNOK")
GBPUSD_RV <- getFXReturnsAndStdDev("GBPUSD")
USDMXN_RV <- getFXReturnsAndStdDev("USDMXN")

# Check Returns and Volatility for a single Equity and FX
# print('Check Returns % SD for Equity & FX for one pair')
# tail(EURUSD_RV)
# head(GSPC_RV)


# ===========================
# ===========================
# ===========================
# =========REGRESSION========
# ===========================
# ===========================
# ===========================

#Regression Helpers
mergeAndFormatRVs <- function(equityRV, fxRV) {
  fxEquityReturnsVolatility <- merge(fxRV, equityRV, by="Date")
  
  # Add YTD
  fxEquityReturnsVolatility$Year <- as.numeric(substr(as.character(fxEquityReturnsVolatility$Date),1,4));
  fxEquityReturnsVolatility$Month <- as.numeric(substr(as.character(fxEquityReturnsVolatility$Date),5,6));
  fxEquityReturnsVolatility$Day <- as.numeric(substr(as.character(fxEquityReturnsVolatility$Date),7,8));
  
  return(fxEquityReturnsVolatility)
}

# Regressions of FX Returns & Equity Returns
# do.call : converts list to vector
# as.list : converts numeric, string, etc into list type
# lm accepts only vectors.
fillFXR_EQR_Regression <- function(fxEquityReturnsVolatility, equityIndex, fx) {
  fxReturns <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$FX_Returns)))
  eqReturns <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$Equity_Returns)))
  reg_fxr_eqr <- lm(fxReturns~eqReturns, data=fxEquityReturnsVolatility)
  reg_fxr_eqr_summary <- summary(reg_fxr_eqr)
  # print(reg_fxr_eqr_summary)
  
  alpha <- reg_fxr_eqr_summary$coefficients[1]
  alpha_t_stat <- reg_fxr_eqr_summary$coefficients[1,3]
  beta <- reg_fxr_eqr_summary$coefficients[2]
  beta_t_stat <- reg_fxr_eqr_summary$coefficients[2,3]
  adjusted_r_squared <- reg_fxr_eqr_summary$adj.r.squared
  
  FXR_EQR_DF[equityIndex, fx] <<- alpha
  FXR_EQR_DF[equityIndex+1, fx] <<- alpha_t_stat
  FXR_EQR_DF[equityIndex+2, fx] <<- beta
  FXR_EQR_DF[equityIndex+3, fx] <<- beta_t_stat
  FXR_EQR_DF[equityIndex+4, fx] <<- adjusted_r_squared
  
  return(FXR_EQR_DF)
}

# Regressions of FX Returns & Equity Volatility (SD) 
fillFXR_EQV_Regression <- function(fxEquityReturnsVolatility, equityIndex, fx) {
  fxReturns <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$FX_Returns)))
  eqVolatility <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$Equity_Standard_Deviation)))
  reg_fxr_eqv <- lm(fxReturns~eqVolatility, data=fxEquityReturnsVolatility)
  reg_fxr_eqv_summary <- summary(reg_fxr_eqv)
  # print(reg_fxr_eqv_summary)
  
  alpha <- reg_fxr_eqv_summary$coefficients[1]
  alpha_t_stat <- reg_fxr_eqv_summary$coefficients[1,3]
  beta <- reg_fxr_eqv_summary$coefficients[2]
  beta_t_stat <- reg_fxr_eqv_summary$coefficients[2,3]
  adjusted_r_squared <- reg_fxr_eqv_summary$adj.r.squared
  
  FXR_EQV_DF[equityIndex, fx] <- alpha
  FXR_EQV_DF[equityIndex+1, fx] <- alpha_t_stat
  FXR_EQV_DF[equityIndex+2, fx] <- beta
  FXR_EQV_DF[equityIndex+3, fx] <- beta_t_stat
  FXR_EQV_DF[equityIndex+4, fx] <- adjusted_r_squared
  
  return(FXR_EQV_DF)
}

# Regressions of FX Volatility & Equity Returns (SD) 
fillFXV_EQR_Regression <- function(fxEquityReturnsVolatility, equityIndex, fx) {
  fxVolatility <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$FX_Standard_Deviation)))
  eqReturns <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$Equity_Returns)))
  reg_fxv_eqr <- lm(fxVolatility~eqReturns, data=fxEquityReturnsVolatility)
  reg_fxv_eqr_summary <- summary(reg_fxv_eqr)
  # print(reg_fxv_eqr_summary)
  
  alpha <- reg_fxv_eqr_summary$coefficients[1]
  alpha_t_stat <- reg_fxv_eqr_summary$coefficients[1,3]
  beta <- reg_fxv_eqr_summary$coefficients[2]
  beta_t_stat <- reg_fxv_eqr_summary$coefficients[2,3]
  adjusted_r_squared <- reg_fxv_eqr_summary$adj.r.squared
  
  FXV_EQR_DF[equityIndex, fx] <- alpha
  FXV_EQR_DF[equityIndex+1, fx] <- alpha_t_stat
  FXV_EQR_DF[equityIndex+2, fx] <- beta
  FXV_EQR_DF[equityIndex+3, fx] <- beta_t_stat
  FXV_EQR_DF[equityIndex+4, fx] <- adjusted_r_squared
  
  return(FXV_EQR_DF)
}

# Regressions of FX Volatility & Equity Volatility (SD) 
fillFXV_EQV_Regression <- function(fxEquityReturnsVolatility, equityIndex, fx) {
  fxVolatility <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$FX_Standard_Deviation)))
  eqVolatility <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$Equity_Standard_Deviation)))
  reg_fxv_eqv <- lm(fxVolatility~eqVolatility, data=fxEquityReturnsVolatility)
  reg_fxv_eqv_summary <- summary(reg_fxv_eqv)
  # print(reg_fxv_eqv_summary)
  
  alpha <- reg_fxv_eqv_summary$coefficients[1]
  alpha_t_stat <- reg_fxv_eqv_summary$coefficients[1,3]
  beta <- reg_fxv_eqv_summary$coefficients[2]
  beta_t_stat <- reg_fxv_eqv_summary$coefficients[2,3]
  adjusted_r_squared <- reg_fxv_eqv_summary$adj.r.squared
  
  FXV_EQV_DF[equityIndex, fx] <<- alpha
  FXV_EQV_DF[equityIndex+1, fx] <<- alpha_t_stat
  FXV_EQV_DF[equityIndex+2, fx] <<- beta
  FXV_EQV_DF[equityIndex+3, fx] <<- beta_t_stat
  FXV_EQV_DF[equityIndex+4, fx] <<- adjusted_r_squared
  
  return(FXV_EQV_DF)
}

getCoEffs <- function(equityRV, fxRV, equityIndex, fx) {
  # Merge RV data frames by Date
  fxEquityReturnsVolatility <<- mergeAndFormatRVs(equityRV, fxRV)
  
  # Regressions of FX Volatility & Equity Returns (SD)
  FXV_EQR_DF <<- fillFXV_EQR_Regression(fxEquityReturnsVolatility, equityIndex, fx)
  
  # Regressions of FX Volatility & Equity Volatility (SD) 
  FXV_EQV_DF <<- fillFXV_EQV_Regression(fxEquityReturnsVolatility, equityIndex, fx)
  
  # Regressions of FX Returns & Equity Returns
  FXR_EQR_DF <<- fillFXR_EQR_Regression(fxEquityReturnsVolatility, equityIndex, fx)
  
  # Regressions of FX Returns & Equity Volatility (SD) 
  FXR_EQV_DF <<- fillFXR_EQV_Regression(fxEquityReturnsVolatility, equityIndex, fx)
}

# Build empty coefficient+p-values tables
# ===========================
# ======== Snapshot =========
# ===========================
#              EURUSD  USDCHF  ...
# GSPC  Alpha   ....    ....
#       t-stat  ....    ....
#       beta    ....    ....
#       tstat   ....    ....
# GDAXI 
# ...   ...     ...
# ===========================

prepareRegressionTable <- function() {
  # Symbols & Currencies vectors for column and row names
  EquitySymbols <- c('GSPC','','','','','GDAXI','','','','', 'SSMI','','','','', 'N225','','','','', 'GSPTSE','','','','', 'AORD','','','','', 'NZ50','','','','', 'OMX','','','','', 'FTSE','','','','', 'MXX','','','','')
  FxCurrencies <- c('Equities', 'Variables', 'EURUSD', 'USDCHF', 'USDJPY', 'USDCAD', 'AUDUSD', 'NZDUSD', 'USDSEK', 'USDNOK', 'GBPUSD', 'USDMXN')
  
  # 5 Regression variables for each equity
  RegressionVariables <- c('Alpha', 't-stat', 'Beta', 't-stat', 'R^2')
  # 5 regression variables * 10 equities
  regressionTable <- data.frame(matrix(0, ncol = 12, nrow = 10*5))
  colnames(regressionTable) <- FxCurrencies
  # Assign equity names in first column (1 for every 5 rows)
  regressionTable[,1] <- EquitySymbols
  # Loop through second column to fill with 5 regression varaibles for every equity
  for(i in 1:nrow(regressionTable)) {
    if(i%%5 == 1) {
      regressionTable[i:(i+4),2] <- RegressionVariables
    }
  }
  
  return(regressionTable)
}

# X axis -> FX Volatility
# Y axis -> EQUITY Returns
FXV_EQR_DF <- prepareRegressionTable()

# X axis -> FX Volatility
# Y axis -> EQUITY Volatility
FXV_EQV_DF <- prepareRegressionTable()

# X axis -> FX Returns
# Y axis -> EQUITY Returns
FXR_EQR_DF <- prepareRegressionTable()

# X axis -> FX Returns
# Y axis -> EQUITY Volatility
FXR_EQV_DF <- prepareRegressionTable()

# Compute & fill tables
# GSPC -> FXs
# 'GSPC' : Index 1
getCoEffs(GSPC_RV, EURUSD_RV, 1, 'EURUSD')
getCoEffs(GSPC_RV, USDCHF_RV, 1, 'USDCHF')
getCoEffs(GSPC_RV, USDJPY_RV, 1, 'USDJPY')
getCoEffs(GSPC_RV, USDCAD_RV, 1, 'USDCAD')
getCoEffs(GSPC_RV, AUDUSD_RV, 1, 'AUDUSD')
getCoEffs(GSPC_RV, NZDUSD_RV, 1, 'NZDUSD')
getCoEffs(GSPC_RV, USDSEK_RV, 1, 'USDSEK')
getCoEffs(GSPC_RV, USDNOK_RV, 1, 'USDNOK')
getCoEffs(GSPC_RV, USDMXN_RV, 1, 'USDMXN')
getCoEffs(GSPC_RV, GBPUSD_RV, 1, 'GBPUSD')

# 'GDAXI' : Index 2
getCoEffs(GDAXI_RV, EURUSD_RV, 6, 'EURUSD')
getCoEffs(GDAXI_RV, USDCHF_RV, 6, 'USDCHF')
getCoEffs(GDAXI_RV, USDJPY_RV, 6, 'USDJPY')
getCoEffs(GDAXI_RV, USDCAD_RV, 6, 'USDCAD')
getCoEffs(GDAXI_RV, AUDUSD_RV, 6, 'AUDUSD')
getCoEffs(GDAXI_RV, NZDUSD_RV, 6, 'NZDUSD')
getCoEffs(GDAXI_RV, USDSEK_RV, 6, 'USDSEK')
getCoEffs(GDAXI_RV, USDNOK_RV, 6, 'USDNOK')
getCoEffs(GDAXI_RV, USDMXN_RV, 6, 'USDMXN')
getCoEffs(GDAXI_RV, GBPUSD_RV, 6, 'GBPUSD')


# SSMI -> FXs
# 'SSMI' : Index 3
getCoEffs(SSMI_RV, EURUSD_RV, 11, 'EURUSD')
getCoEffs(SSMI_RV, USDCHF_RV, 11, 'USDCHF')
getCoEffs(SSMI_RV, USDJPY_RV, 11, 'USDJPY')
getCoEffs(SSMI_RV, USDCAD_RV, 11, 'USDCAD')
getCoEffs(SSMI_RV, AUDUSD_RV, 11, 'AUDUSD')
getCoEffs(SSMI_RV, NZDUSD_RV, 11, 'NZDUSD')
getCoEffs(SSMI_RV, USDSEK_RV, 11, 'USDSEK')
getCoEffs(SSMI_RV, USDNOK_RV, 11, 'USDNOK')
getCoEffs(SSMI_RV, USDMXN_RV, 11, 'USDMXN')
getCoEffs(SSMI_RV, GBPUSD_RV, 11, 'GBPUSD')


# N225 -> FXs
# 'N225' : Index 1
getCoEffs(N225_RV, EURUSD_RV, 16, 'EURUSD')
getCoEffs(N225_RV, USDCHF_RV, 16, 'USDCHF')
getCoEffs(N225_RV, USDJPY_RV, 16, 'USDJPY')
getCoEffs(N225_RV, USDCAD_RV, 16, 'USDCAD')
getCoEffs(N225_RV, AUDUSD_RV, 16, 'AUDUSD')
getCoEffs(N225_RV, NZDUSD_RV, 16, 'NZDUSD')
getCoEffs(N225_RV, USDSEK_RV, 16, 'USDSEK')
getCoEffs(N225_RV, USDNOK_RV, 16, 'USDNOK')
getCoEffs(N225_RV, USDMXN_RV, 16, 'USDMXN')
getCoEffs(N225_RV, GBPUSD_RV, 16, 'GBPUSD')

# GSPTSE -> FXs
# 'GSPTSE' : Index 1
getCoEffs(GSPTSE_RV, EURUSD_RV, 21, 'EURUSD')
getCoEffs(GSPTSE_RV, USDCHF_RV, 21, 'USDCHF')
getCoEffs(GSPTSE_RV, USDJPY_RV, 21, 'USDJPY')
getCoEffs(GSPTSE_RV, USDCAD_RV, 21, 'USDCAD')
getCoEffs(GSPTSE_RV, AUDUSD_RV, 21, 'AUDUSD')
getCoEffs(GSPTSE_RV, NZDUSD_RV, 21, 'NZDUSD')
getCoEffs(GSPTSE_RV, USDSEK_RV, 21, 'USDSEK')
getCoEffs(GSPTSE_RV, USDNOK_RV, 21, 'USDNOK')
getCoEffs(GSPTSE_RV, USDMXN_RV, 21, 'USDMXN')
getCoEffs(GSPTSE_RV, GBPUSD_RV, 21, 'GBPUSD')

# AORD -> FXs
# 'AORD' : Index 1
getCoEffs(AORD_RV, EURUSD_RV, 26, 'EURUSD')
getCoEffs(AORD_RV, USDCHF_RV, 26, 'USDCHF')
getCoEffs(AORD_RV, USDJPY_RV, 26, 'USDJPY')
getCoEffs(AORD_RV, USDCAD_RV, 26, 'USDCAD')
getCoEffs(AORD_RV, AUDUSD_RV, 26, 'AUDUSD')
getCoEffs(AORD_RV, NZDUSD_RV, 26, 'NZDUSD')
getCoEffs(AORD_RV, USDSEK_RV, 26, 'USDSEK')
getCoEffs(AORD_RV, USDNOK_RV, 26, 'USDNOK')
getCoEffs(AORD_RV, USDMXN_RV, 26, 'USDMXN')
getCoEffs(AORD_RV, GBPUSD_RV, 26, 'GBPUSD')


# NZ50 -> FXs
# 'NZ50' : Index 1
getCoEffs(NZ50_RV, EURUSD_RV, 31, 'EURUSD')
getCoEffs(NZ50_RV, USDCHF_RV, 31, 'USDCHF')
getCoEffs(NZ50_RV, USDJPY_RV, 31, 'USDJPY')
getCoEffs(NZ50_RV, USDCAD_RV, 31, 'USDCAD')
getCoEffs(NZ50_RV, AUDUSD_RV, 31, 'AUDUSD')
getCoEffs(NZ50_RV, NZDUSD_RV, 31, 'NZDUSD')
getCoEffs(NZ50_RV, USDSEK_RV, 31, 'USDSEK')
getCoEffs(NZ50_RV, USDNOK_RV, 31, 'USDNOK')
getCoEffs(NZ50_RV, USDMXN_RV, 31, 'USDMXN')
getCoEffs(NZ50_RV, GBPUSD_RV, 31, 'GBPUSD')


# OMX -> FXs
# 'OMX' : Index 1
getCoEffs(OMX_RV, EURUSD_RV, 36, 'EURUSD')
getCoEffs(OMX_RV, USDCHF_RV, 36, 'USDCHF')
getCoEffs(OMX_RV, USDJPY_RV, 36, 'USDJPY')
getCoEffs(OMX_RV, USDCAD_RV, 36, 'USDCAD')
getCoEffs(OMX_RV, AUDUSD_RV, 36, 'AUDUSD')
getCoEffs(OMX_RV, NZDUSD_RV, 36, 'NZDUSD')
getCoEffs(OMX_RV, USDSEK_RV, 36, 'USDSEK')
getCoEffs(OMX_RV, USDNOK_RV, 36, 'USDNOK')
getCoEffs(OMX_RV, USDMXN_RV, 36, 'USDMXN')
getCoEffs(OMX_RV, GBPUSD_RV, 36, 'GBPUSD')


# FTSE -> FXs
# 'FTSE' : Index 1
getCoEffs(FTSE_RV, EURUSD_RV, 41, 'EURUSD')
getCoEffs(FTSE_RV, USDCHF_RV, 41, 'USDCHF')
getCoEffs(FTSE_RV, USDJPY_RV, 41, 'USDJPY')
getCoEffs(FTSE_RV, USDCAD_RV, 41, 'USDCAD')
getCoEffs(FTSE_RV, AUDUSD_RV, 41, 'AUDUSD')
getCoEffs(FTSE_RV, NZDUSD_RV, 41, 'NZDUSD')
getCoEffs(FTSE_RV, USDSEK_RV, 41, 'USDSEK')
getCoEffs(FTSE_RV, USDNOK_RV, 41, 'USDNOK')
getCoEffs(FTSE_RV, USDMXN_RV, 41, 'USDMXN')
getCoEffs(FTSE_RV, GBPUSD_RV, 41, 'GBPUSD')


# MXX -> FXs
# 'MXX' : Index 1
getCoEffs(MXX_RV, EURUSD_RV, 46, 'EURUSD')
getCoEffs(MXX_RV, USDCHF_RV, 46, 'USDCHF')
getCoEffs(MXX_RV, USDJPY_RV, 46, 'USDJPY')
getCoEffs(MXX_RV, USDCAD_RV, 46, 'USDCAD')
getCoEffs(MXX_RV, AUDUSD_RV, 46, 'AUDUSD')
getCoEffs(MXX_RV, NZDUSD_RV, 46, 'NZDUSD')
getCoEffs(MXX_RV, USDSEK_RV, 46, 'USDSEK')
getCoEffs(MXX_RV, USDNOK_RV, 46, 'USDNOK')
getCoEffs(MXX_RV, USDMXN_RV, 46, 'USDMXN')
getCoEffs(MXX_RV, GBPUSD_RV, 46, 'GBPUSD')

print("FXV_EQR_DF Regression table")
FXV_EQR_DF
print("FXV_EQV_DF Regression table")
FXV_EQV_DF
print("FXR_EQR_DF Regression table")
FXR_EQR_DF
print("FXR_EQV_DF Regression table")
FXR_EQV_DF

# ===========================
# ===========================
# ===========================
# =======VIX & MOMENTUM======
# ===========================
# ===========================

# Fetch Equity data by Symbols
vixSource <- c("yahoo")
vixIndex <- c("^VIX")
vixColumnNames <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
suppressWarnings(getDataForSymbols(vixIndex, vixSource))

# Convert to data frame and assign column names
VIX <<- data.frame(date=index(VIX), coredata(VIX))
colnames(VIX) <- vixColumnNames
VIX <- VIX %>%
  select(Date, Close)

# Add Year, Month & Date columns 
vixTemp <- VIX %>%
    mutate(Year = as.numeric(substr(as.character(Date), 1, 4))) %>%
    mutate(Date = as.character(Date))

VIX <- vixTemp
rownames(VIX) <- NULL

# Remove "-" in Date string
vixTemp <- VIX %>%
    filter(Year>=startYear, Year<=endYear) %>%
    select(Date, Close) %>%
    mutate(Date = as.numeric(paste(substr(as.character(Date), 1, 4),
                                   substr(as.character(Date), 6, 7),
                                   substr(as.character(Date), 9, 10)
                                   ,sep="")))
VIX <- vixTemp

print("VIX data snapshot")
head(VIX)

# switch directory to read momentum CSV
getwd()
setwd("../Momentum")

momentum <- data.frame()
momentum <- read.table("F-F_Momentum_Factor_daily.csv", sep=",")
colnames(momentum) <- c("Date", "Momentum")
#Delete last two rows in Kenneth table : Has author name in CSV
momentum <- momentum[1:(nrow(momentum)-2), ]
# Filter data from start year and end year
momentum <- momentum %>%
  mutate(Year = as.numeric(substr(as.character(Date), 1, 4))) %>%
  filter(Year >= startYear & Year <= endYear) %>%
  mutate(Date = as.numeric(as.character(Date))) %>%   # Convert factor to numeric datatype "sharu" style
  mutate(MOM_Factor = Momentum / 100) %>%
  select(Date, MOM_Factor)
  
print("Momentum data snapshot")
head(momentum)

# Merge VIX, Momentum with FX Returns
# For large tables in R dplyr's function inner_join() is much faster than merge()
VIX_MOM_EURUSD <- inner_join(VIX, EURUSD_RV, by="Date") %>%
	inner_join(., momentum, by="Date") %>%
	mutate(VIX = Close) %>%
	select(Date, VIX, MOM_Factor, FX_Returns, FX_Standard_Deviation)

VIX_MOM_USDCHF <- inner_join(VIX, USDCHF_RV, by="Date") %>%
	inner_join(., momentum, by="Date") %>%
	mutate(VIX = Close) %>%
	select(Date, VIX, MOM_Factor, FX_Returns, FX_Standard_Deviation)

VIX_MOM_USDJPY <- inner_join(VIX, USDJPY_RV, by="Date") %>%
	inner_join(., momentum, by="Date") %>%
	mutate(VIX = Close) %>%
	select(Date, VIX, MOM_Factor, FX_Returns, FX_Standard_Deviation)

VIX_MOM_USDCAD <- inner_join(VIX, USDCAD_RV, by="Date") %>%
	inner_join(., momentum, by="Date") %>%
	mutate(VIX = Close) %>%
	select(Date, VIX, MOM_Factor, FX_Returns, FX_Standard_Deviation)

VIX_MOM_AUDUSD <- inner_join(VIX, AUDUSD_RV, by="Date") %>%
	inner_join(., momentum, by="Date") %>%
	mutate(VIX = Close) %>%
	select(Date, VIX, MOM_Factor, FX_Returns, FX_Standard_Deviation)

VIX_MOM_NZDUSD <- inner_join(VIX, NZDUSD_RV, by="Date") %>%
	inner_join(., momentum, by="Date") %>%
	mutate(VIX = Close) %>%
	select(Date, VIX, MOM_Factor, FX_Returns, FX_Standard_Deviation)

VIX_MOM_USDSEK <- inner_join(VIX, USDSEK_RV, by="Date") %>%
	inner_join(., momentum, by="Date") %>%
	mutate(VIX = Close) %>%
	select(Date, VIX, MOM_Factor, FX_Returns, FX_Standard_Deviation)

VIX_MOM_USDNOK <- inner_join(VIX, USDNOK_RV, by="Date") %>%
 	inner_join(., momentum, by="Date") %>%
 	mutate(VIX = Close) %>%
 	select(Date, VIX, MOM_Factor, FX_Returns, FX_Standard_Deviation)

VIX_MOM_GBPUSD <- inner_join(VIX, GBPUSD_RV, by="Date") %>%
	inner_join(., momentum, by="Date") %>%
	mutate(VIX = Close) %>%
	select(Date, VIX, MOM_Factor, FX_Returns, FX_Standard_Deviation)

VIX_MOM_USDMXN <- inner_join(VIX, USDMXN_RV, by="Date") %>%
	inner_join(., momentum, by="Date") %>%
	mutate(VIX = Close) %>%
	select(Date, VIX, MOM_Factor, FX_Returns, FX_Standard_Deviation)

print('VIX - MOM - FX snapshot')
head(VIX_MOM_EURUSD)

prepareFactorRegressionTable <- function() {
  # Symbols & Currencies vectors for column and row names
  FactorSymbols <- c('VIX','','','','','MOM','','','','')
  FxCurrencies <- c('Factors', 'Variables', 'EURUSD', 'USDCHF', 'USDJPY', 'USDCAD', 'AUDUSD', 'NZDUSD', 'USDSEK', 'USDNOK', 'GBPUSD', 'USDMXN')
  
  # 5 Regression variables for each factor
  RegressionVariables <- c('Alpha', 't-stat', 'Beta', 't-stat', 'R^2')
  # 5 regression variables * 2 factors
  regressionTable <- data.frame(matrix(0, ncol = 12, nrow = 2*5))
  colnames(regressionTable) <- FxCurrencies
  # Assign factor names in first column (1 for every 5 rows)
  regressionTable[,1] <- FactorSymbols
  # Loop through second column to fill with 5 regression varaibles for every equity
  for(i in 1:nrow(regressionTable)) {
    if(i%%5 == 1) {
      regressionTable[i:(i+4),2] <- RegressionVariables
    }
  }
  
  return(regressionTable)
}

# X axis -> FX Returns
# Y axis -> VIX Factor
VIX_MOM_FXR_DF <- prepareFactorRegressionTable()

# X axis -> FX Volatility
# Y axis -> VIX Factor
VIX_MOM_FXV_DF <- prepareFactorRegressionTable()

print("Prepared empty factor regression table layout snapshot")
VIX_MOM_FXV_DF

# Regressions of VIX & FX Returns
fillVIX_MOM_FXR_Regression <- function(vixMomentum, fx) {
  fxReturns <- c(do.call("cbind", as.list(vixMomentum$FX_Returns)))
  vixFactor <- c(do.call("cbind", as.list(vixMomentum$VIX)))
  momFactor <- c(do.call("cbind", as.list(vixMomentum$MOM_Factor)))
  
  # VIX - FXR
  factorIndex <- 1
  reg_vix_fxr <- lm(fxReturns~vixFactor, data=vixMomentum)
  reg_vix_fxr_summary <- summary(reg_vix_fxr)
  
  alpha <- reg_vix_fxr_summary$coefficients[1]
  alpha_t_stat <- reg_vix_fxr_summary$coefficients[1,3]
  beta <- reg_vix_fxr_summary$coefficients[2]
  beta_t_stat <- reg_vix_fxr_summary$coefficients[2,3]
  adjusted_r_squared <- reg_vix_fxr_summary$adj.r.squared
  
  VIX_MOM_FXR_DF[factorIndex, fx] <- alpha
  VIX_MOM_FXR_DF[factorIndex+1, fx] <- alpha_t_stat
  VIX_MOM_FXR_DF[factorIndex+2, fx] <- beta
  VIX_MOM_FXR_DF[factorIndex+3, fx] <- beta_t_stat
  VIX_MOM_FXR_DF[factorIndex+4, fx] <- adjusted_r_squared
  
  # MOM - FXR
  factorIndex <- 6
  reg_mom_fxr <- lm(fxReturns~momFactor, data=vixMomentum)
  reg_mom_fxr_summary <- summary(reg_mom_fxr)
  
  alpha <- reg_mom_fxr_summary$coefficients[1]
  alpha_t_stat <- reg_mom_fxr_summary$coefficients[1,3]
  beta <- reg_mom_fxr_summary$coefficients[2]
  beta_t_stat <- reg_mom_fxr_summary$coefficients[2,3]
  adjusted_r_squared <- reg_mom_fxr_summary$adj.r.squared
  
  VIX_MOM_FXR_DF[factorIndex, fx] <- alpha
  VIX_MOM_FXR_DF[factorIndex+1, fx] <- alpha_t_stat
  VIX_MOM_FXR_DF[factorIndex+2, fx] <- beta
  VIX_MOM_FXR_DF[factorIndex+3, fx] <- beta_t_stat
  VIX_MOM_FXR_DF[factorIndex+4, fx] <- adjusted_r_squared
  
  return(VIX_MOM_FXR_DF)
}

# Regressions of VIX & FX Volatility (SD)
fillVIX_MOM_FXV_Regression <- function(vixMomentum, fx) {
  fxVolatility <- c(do.call("cbind", as.list(vixMomentum$FX_Standard_Deviation)))
  vixFactor <- c(do.call("cbind", as.list(vixMomentum$VIX)))
  momFactor <- c(do.call("cbind", as.list(vixMomentum$MOM_Factor)))
  
  # VIX - FXV
  factorIndex <- 1
  reg_vix_fxv <- lm(fxVolatility~vixFactor, data=vixMomentum)
  reg_vix_fxv_summary <- summary(reg_vix_fxv)
  
  alpha <- reg_vix_fxv_summary$coefficients[1]
  alpha_t_stat <- reg_vix_fxv_summary$coefficients[1,3]
  beta <- reg_vix_fxv_summary$coefficients[2]
  beta_t_stat <- reg_vix_fxv_summary$coefficients[2,3]
  adjusted_r_squared <- reg_vix_fxv_summary$adj.r.squared
  
  VIX_MOM_FXV_DF[factorIndex, fx] <- alpha
  VIX_MOM_FXV_DF[factorIndex+1, fx] <- alpha_t_stat
  VIX_MOM_FXV_DF[factorIndex+2, fx] <- beta
  VIX_MOM_FXV_DF[factorIndex+3, fx] <- beta_t_stat
  VIX_MOM_FXV_DF[factorIndex+4, fx] <- adjusted_r_squared
  
  # MOM - FXV
  factorIndex <- 6
  reg_mom_fxv <- lm(fxVolatility~momFactor, data=vixMomentum)
  reg_mom_fxv_summary <- summary(reg_mom_fxv)
  
  alpha <- reg_mom_fxv_summary$coefficients[1]
  alpha_t_stat <- reg_mom_fxv_summary$coefficients[1,3]
  beta <- reg_mom_fxv_summary$coefficients[2]
  beta_t_stat <- reg_mom_fxv_summary$coefficients[2,3]
  adjusted_r_squared <- reg_mom_fxv_summary$adj.r.squared
  
  VIX_MOM_FXV_DF[factorIndex, fx] <- alpha
  VIX_MOM_FXV_DF[factorIndex+1, fx] <- alpha_t_stat
  VIX_MOM_FXV_DF[factorIndex+2, fx] <- beta
  VIX_MOM_FXV_DF[factorIndex+3, fx] <- beta_t_stat
  VIX_MOM_FXV_DF[factorIndex+4, fx] <- adjusted_r_squared

  return(VIX_MOM_FXV_DF)
}

# wrapper function to compute all 4 regressions and fill tables accordingly
getFactorCoEffs <- function(vixMomentum, fx) {
  # Regressions of VIX+MOM over FX Returns
  VIX_MOM_FXR_DF <<- fillVIX_MOM_FXR_Regression(vixMomentum, fx)
  
  # Regressions of VIX+MOM over FX Volatility (SD) 
  VIX_MOM_FXV_DF <<- fillVIX_MOM_FXV_Regression(vixMomentum, fx)
}

print('Compute & fill factor regression values')
getFactorCoEffs(VIX_MOM_EURUSD, 'EURUSD')
getFactorCoEffs(VIX_MOM_USDCHF, 'USDCHF')
getFactorCoEffs(VIX_MOM_USDJPY, 'USDJPY')
getFactorCoEffs(VIX_MOM_USDCAD, 'USDCAD')
getFactorCoEffs(VIX_MOM_AUDUSD, 'AUDUSD')
getFactorCoEffs(VIX_MOM_NZDUSD, 'NZDUSD')
getFactorCoEffs(VIX_MOM_USDSEK, 'USDSEK')
getFactorCoEffs(VIX_MOM_USDNOK, 'USDNOK')
getFactorCoEffs(VIX_MOM_USDMXN, 'USDMXN')
getFactorCoEffs(VIX_MOM_GBPUSD, 'GBPUSD')


print("Factor computations completed")

print("VIX, Momentum over FX Returns Regression table")
VIX_MOM_FXR_DF
print("VIX, Momentum over FX Volatility Regression table")
VIX_MOM_FXV_DF

# ===========================
# ===========================
# ===========================
# =======CORRELATIONS========
# ===========================
# ===========================

prepareCorrelationTable <- function() {
  # Symbols & Currencies vectors for column and row names
  EquitySymbols <- c('GSPC', 'GDAXI', 'SSMI', 'N225', 'GSPTSE', 'AORD', 'NZ50', 'OMX', 'FTSE', 'MXX')
  FxCurrencies <- c('Equities', 'EURUSD', 'USDCHF', 'USDJPY', 'USDCAD', 'AUDUSD', 'NZDUSD', 'USDSEK', 'USDNOK', 'GBPUSD', 'USDMXN')
  
  correlationTable <- data.frame(matrix(0, ncol = 11, nrow = 10))
  colnames(correlationTable) <- FxCurrencies
  correlationTable[,1] <- EquitySymbols
  return(correlationTable)
}

fillFXV_EQR_Correlation <- function(fxEquityReturnsVolatility, equityIndex, fx) {
  fxVolatility <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$FX_Standard_Deviation)))
  eqReturns <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$Equity_Returns)))
  correl_fxv_eqr <- cor.test(fxVolatility, eqReturns)
  Cor_FXV_EQR_DF[equityIndex, fx]  <- correl_fxv_eqr$estimate[1]
  return(Cor_FXV_EQR_DF)
}

fillFXV_EQV_Correlation <- function(fxEquityReturnsVolatility, equityIndex, fx) {
  fxVolatility <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$FX_Standard_Deviation)))
  eqVolatility <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$Equity_Standard_Deviation)))
  correl_fxv_eqv <- cor.test(fxVolatility, eqVolatility)
  Cor_FXV_EQV_DF[equityIndex, fx]  <- correl_fxv_eqv$estimate[1]
  return(Cor_FXV_EQV_DF)
}  

fillFXR_EQR_Correlation <- function(fxEquityReturnsVolatility, equityIndex, fx) {
  fxReturns <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$FX_Returns)))
  eqReturns <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$Equity_Returns)))
  correl_fxr_eqr <- cor.test(fxReturns, eqReturns)
  Cor_FXR_EQR_DF[equityIndex, fx]  <- correl_fxr_eqr$estimate[1]
  return(Cor_FXR_EQR_DF)
}  

fillFXR_EQV_Correlation <- function(fxEquityReturnsVolatility, equityIndex, fx) {
  fxReturns <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$FX_Returns)))
  eqVolatility <- c(do.call("cbind", as.list(fxEquityReturnsVolatility$Equity_Standard_Deviation)))
  correl_fxr_eqv <- cor.test(fxReturns, eqVolatility)
  Cor_FXR_EQV_DF[equityIndex, fx]  <- correl_fxr_eqv$estimate[1]
  return(Cor_FXR_EQV_DF)
} 


getCor <- function(equityRV, fxRV, equityIndex, fx) {
  # Merge RV data frames by Date
  fxEquityReturnsVolatility <<- mergeAndFormatRVs(equityRV, fxRV)
  
  # Correlations of FX Volatility & Equity Returns (SD)
  Cor_FXV_EQR_DF <<- fillFXV_EQR_Correlation(fxEquityReturnsVolatility, equityIndex, fx)
  
  # Correlations of FX Volatility & Equity Volatility (SD) 
  Cor_FXV_EQV_DF <<- fillFXV_EQV_Correlation(fxEquityReturnsVolatility, equityIndex, fx)
  
  # Correlations of FX Returns & Equity Returns
  Cor_FXR_EQR_DF <<- fillFXR_EQR_Correlation(fxEquityReturnsVolatility, equityIndex, fx)
  
  # Correlations of FX Returns & Equity Volatility (SD) 
  Cor_FXR_EQV_DF <<- fillFXR_EQV_Correlation(fxEquityReturnsVolatility, equityIndex, fx)
}

# X axis -> FX Volatility
# Y axis -> EQUITY Returns
Cor_FXV_EQR_DF <- prepareCorrelationTable()

# X axis -> FX Volatility
# Y axis -> EQUITY Volatility
Cor_FXV_EQV_DF <- prepareCorrelationTable()

# X axis -> FX Returns
# Y axis -> EQUITY Returns
Cor_FXR_EQR_DF <- prepareCorrelationTable()

# X axis -> FX Returns
# Y axis -> EQUITY Volatility
Cor_FXR_EQV_DF <- prepareCorrelationTable()


# Compute & fill tables
# GSPC -> FXs
# 'GSPC' : Index 1
getCor(GSPC_RV, EURUSD_RV, 1, 'EURUSD')
getCor(GSPC_RV, USDCHF_RV, 1, 'USDCHF')
getCor(GSPC_RV, USDJPY_RV, 1, 'USDJPY')
getCor(GSPC_RV, USDCAD_RV, 1, 'USDCAD')
getCor(GSPC_RV, AUDUSD_RV, 1, 'AUDUSD')
getCor(GSPC_RV, NZDUSD_RV, 1, 'NZDUSD')
getCor(GSPC_RV, USDSEK_RV, 1, 'USDSEK')
getCor(GSPC_RV, USDNOK_RV, 1, 'USDNOK')
getCor(GSPC_RV, USDMXN_RV, 1, 'USDMXN')
getCor(GSPC_RV, GBPUSD_RV, 1, 'GBPUSD')

# 'GDAXI' : Index 2
getCor(GDAXI_RV, EURUSD_RV, 2, 'EURUSD')
getCor(GDAXI_RV, USDCHF_RV, 2, 'USDCHF')
getCor(GDAXI_RV, USDJPY_RV, 2, 'USDJPY')
getCor(GDAXI_RV, USDCAD_RV, 2, 'USDCAD')
getCor(GDAXI_RV, AUDUSD_RV, 2, 'AUDUSD')
getCor(GDAXI_RV, NZDUSD_RV, 2, 'NZDUSD')
getCor(GDAXI_RV, USDSEK_RV, 2, 'USDSEK')
getCor(GDAXI_RV, USDNOK_RV, 2, 'USDNOK')
getCor(GDAXI_RV, USDMXN_RV, 2, 'USDMXN')
getCor(GDAXI_RV, GBPUSD_RV, 2, 'GBPUSD')


# SSMI -> FXs
# 'SSMI' : Index 3
getCor(SSMI_RV, EURUSD_RV, 3, 'EURUSD')
getCor(SSMI_RV, USDCHF_RV, 3, 'USDCHF')
getCor(SSMI_RV, USDJPY_RV, 3, 'USDJPY')
getCor(SSMI_RV, USDCAD_RV, 3, 'USDCAD')
getCor(SSMI_RV, AUDUSD_RV, 3, 'AUDUSD')
getCor(SSMI_RV, NZDUSD_RV, 3, 'NZDUSD')
getCor(SSMI_RV, USDSEK_RV, 3, 'USDSEK')
getCor(SSMI_RV, USDNOK_RV, 3, 'USDNOK')
getCor(SSMI_RV, USDMXN_RV, 3, 'USDMXN')
getCor(SSMI_RV, GBPUSD_RV, 3, 'GBPUSD')


# N225 -> FXs
# 'N225' : Index 1
getCor(N225_RV, EURUSD_RV, 4, 'EURUSD')
getCor(N225_RV, USDCHF_RV, 4, 'USDCHF')
getCor(N225_RV, USDJPY_RV, 4, 'USDJPY')
getCor(N225_RV, USDCAD_RV, 4, 'USDCAD')
getCor(N225_RV, AUDUSD_RV, 4, 'AUDUSD')
getCor(N225_RV, NZDUSD_RV, 4, 'NZDUSD')
getCor(N225_RV, USDSEK_RV, 4, 'USDSEK')
getCor(N225_RV, USDNOK_RV, 4, 'USDNOK')
getCor(N225_RV, USDMXN_RV, 4, 'USDMXN')
getCor(N225_RV, GBPUSD_RV, 4, 'GBPUSD')

# GSPTSE -> FXs
# 'GSPTSE' : Index 1
getCor(GSPTSE_RV, EURUSD_RV, 5, 'EURUSD')
getCor(GSPTSE_RV, USDCHF_RV, 5, 'USDCHF')
getCor(GSPTSE_RV, USDJPY_RV, 5, 'USDJPY')
getCor(GSPTSE_RV, USDCAD_RV, 5, 'USDCAD')
getCor(GSPTSE_RV, AUDUSD_RV, 5, 'AUDUSD')
getCor(GSPTSE_RV, NZDUSD_RV, 5, 'NZDUSD')
getCor(GSPTSE_RV, USDSEK_RV, 5, 'USDSEK')
getCor(GSPTSE_RV, USDNOK_RV, 5, 'USDNOK')
getCor(GSPTSE_RV, USDMXN_RV, 5, 'USDMXN')
getCor(GSPTSE_RV, GBPUSD_RV, 5, 'GBPUSD')

# AORD -> FXs
# 'AORD' : Index 1
getCor(AORD_RV, EURUSD_RV, 6, 'EURUSD')
getCor(AORD_RV, USDCHF_RV, 6, 'USDCHF')
getCor(AORD_RV, USDJPY_RV, 6, 'USDJPY')
getCor(AORD_RV, USDCAD_RV, 6, 'USDCAD')
getCor(AORD_RV, AUDUSD_RV, 6, 'AUDUSD')
getCor(AORD_RV, NZDUSD_RV, 6, 'NZDUSD')
getCor(AORD_RV, USDSEK_RV, 6, 'USDSEK')
getCor(AORD_RV, USDNOK_RV, 6, 'USDNOK')
getCor(AORD_RV, USDMXN_RV, 6, 'USDMXN')
getCor(AORD_RV, GBPUSD_RV, 6, 'GBPUSD')


# NZ50 -> FXs
# 'NZ50' : Index 1
getCor(NZ50_RV, EURUSD_RV, 7, 'EURUSD')
getCor(NZ50_RV, USDCHF_RV, 7, 'USDCHF')
getCor(NZ50_RV, USDJPY_RV, 7, 'USDJPY')
getCor(NZ50_RV, USDCAD_RV, 7, 'USDCAD')
getCor(NZ50_RV, AUDUSD_RV, 7, 'AUDUSD')
getCor(NZ50_RV, NZDUSD_RV, 7, 'NZDUSD')
getCor(NZ50_RV, USDSEK_RV, 7, 'USDSEK')
getCor(NZ50_RV, USDNOK_RV, 7, 'USDNOK')
getCor(NZ50_RV, USDMXN_RV, 7, 'USDMXN')
getCor(NZ50_RV, GBPUSD_RV, 7, 'GBPUSD')


# OMX -> FXs
# 'OMX' : Index 1
getCor(OMX_RV, EURUSD_RV, 8, 'EURUSD')
getCor(OMX_RV, USDCHF_RV, 8, 'USDCHF')
getCor(OMX_RV, USDJPY_RV, 8, 'USDJPY')
getCor(OMX_RV, USDCAD_RV, 8, 'USDCAD')
getCor(OMX_RV, AUDUSD_RV, 8, 'AUDUSD')
getCor(OMX_RV, NZDUSD_RV, 8, 'NZDUSD')
getCor(OMX_RV, USDSEK_RV, 8, 'USDSEK')
getCor(OMX_RV, USDNOK_RV, 8, 'USDNOK')
getCor(OMX_RV, USDMXN_RV, 8, 'USDMXN')
getCor(OMX_RV, GBPUSD_RV, 8, 'GBPUSD')


# FTSE -> FXs
# 'FTSE' : Index 1
getCor(FTSE_RV, EURUSD_RV, 9, 'EURUSD')
getCor(FTSE_RV, USDCHF_RV, 9, 'USDCHF')
getCor(FTSE_RV, USDJPY_RV, 9, 'USDJPY')
getCor(FTSE_RV, USDCAD_RV, 9, 'USDCAD')
getCor(FTSE_RV, AUDUSD_RV, 9, 'AUDUSD')
getCor(FTSE_RV, NZDUSD_RV, 9, 'NZDUSD')
getCor(FTSE_RV, USDSEK_RV, 9, 'USDSEK')
getCor(FTSE_RV, USDNOK_RV, 9, 'USDNOK')
getCor(FTSE_RV, USDMXN_RV, 9, 'USDMXN')
getCor(FTSE_RV, GBPUSD_RV, 9, 'GBPUSD')


# MXX -> FXs
# 'MXX' : Index 1
getCor(MXX_RV, EURUSD_RV, 10, 'EURUSD')
getCor(MXX_RV, USDCHF_RV, 10, 'USDCHF')
getCor(MXX_RV, USDJPY_RV, 10, 'USDJPY')
getCor(MXX_RV, USDCAD_RV, 10, 'USDCAD')
getCor(MXX_RV, AUDUSD_RV, 10, 'AUDUSD')
getCor(MXX_RV, NZDUSD_RV, 10, 'NZDUSD')
getCor(MXX_RV, USDSEK_RV, 10, 'USDSEK')
getCor(MXX_RV, USDNOK_RV, 10, 'USDNOK')
getCor(MXX_RV, USDMXN_RV, 10, 'USDMXN')
getCor(MXX_RV, GBPUSD_RV, 10, 'GBPUSD')

print("Correlations computations completed")

print("Filled Correlations FXV_EQR Table")
Cor_FXV_EQR_DF

print("Filled Correlations FXV_EQV Table")
Cor_FXV_EQV_DF

print("Filled Correlations FXR_EQR Table")
Cor_FXR_EQR_DF

print("Filled FXR_EQV Table")
Cor_FXR_EQV_DF

# Save to CSV's
getwd()
setwd('../../WM_Fix_Results/')

# Save Regression results into CSVs
write.table(FXV_EQR_DF, file = "Reg_FXV_EQR_DF.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(FXV_EQV_DF, file = "Reg_FXV_EQV_DF.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(FXR_EQR_DF, file = "Reg_FXR_EQR_DF.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(FXR_EQV_DF, file = "Reg_FXR_EQV_DF.csv", sep = ",", col.names = NA, qmethod = "double")

# Save VIX MOM results into CSVs
write.table(VIX_MOM_FXR_DF, file = "VIX_MOM_FXR_DF.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(VIX_MOM_FXV_DF, file = "VIX_MOM_FXV_DF.csv", sep = ",", col.names = NA, qmethod = "double")

# Save Correlation results into CSVs
write.table(Cor_FXV_EQR_DF, file = "Cor_FXV_EQR_DF.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(Cor_FXV_EQV_DF, file = "Cor_FXV_EQV_DF.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(Cor_FXR_EQR_DF, file = "Cor_FXR_EQR_DF.csv", sep = ",", col.names = NA, qmethod = "double")
write.table(Cor_FXR_EQV_DF, file = "Cor_FXR_EQV_DF.csv", sep = ",", col.names = NA, qmethod = "double")



