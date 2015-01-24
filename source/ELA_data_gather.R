################################################################################
# Gather data on crisis costs, banking system size, and discount rate
# Christopher Gandrud
# 23 January 2015
# MIT License
################################################################################

# Load packages
library(dplyr)
library(WDI)
library(repmis)
library(DataCombine)
library(xlsx)
library(tidyr)
library(lubridate)
library(countrycode)
library(quantmod)

# Set working directory. Change as needed.
setwd('/git_repositories/ela_fiscal_costs/source/')

#### Gather Data ###############################################################
## World Bank Developement Indicators
indicators <- c('GFDD.DI.06', 'FS.AST.DOMS.GD.ZS', 'GFDD.DI.02', 
                'FB.BNK.CAPA.ZS')

wdi <- WDI(indicator = indicators, start = 1990, end = 2013, extra = T) %>%
        rename(CentralBankAssetsGDP = GFDD.DI.06,
               DomesticCreditGDP = FS.AST.DOMS.GD.ZS,
               DepositBankAssetsGDP = GFDD.DI.02,
               CapitalAssets = FB.BNK.CAPA.ZS) %>% 
        select(-iso3c, -capital, -longitude, -latitude, -income, - lending) %>%
        filter(region != 'Aggregates')

## Bank assets %GDP
assets <- data.table::fread('data/raw/Bank Assets (As % Of GDP).csv') %>%
            select(1, 3:62)
class(assets) <- 'data.frame'
names(assets) <- c('country', 1960:2019)

assets_gathered <- gather(assets, year, BankAssetsGDP, 2:ncol(assets))

# For years when BankAssetsGDP == 0, then 0.1
assets_gathered$BankAssetsGDP[assets_gathered$BankAssetsGDP == 0] <- NA

assets_gathered$iso2c <- countrycode(assets_gathered$country, 
                                     origin = 'country.name', 
                                     destination = 'iso2c')

## Eurozone Membership
eu <- 'http://bit.ly/1yRvycq' %>%
        source_data() %>%
        select(iso2c, year)
eu$eurozone <- 1

## Download LV crisis costs
costs <-
    'https://raw.githubusercontent.com/christophergandrud/Keefer2007Replication/master/data/KefferFiscal.csv' %>%
    source_data() %>%
    select(iso2c, year, LV2012.Fiscal) %>%
    DropNA('LV2012.Fiscal')

## Discount rate
discount_rate <- read.xlsx('data/raw/IMF_discount_rate.xlsx',
                    sheetIndex = 1)
dr_gathered <- discount_rate %>%
                gather(year, discount_rate, 2:ncol(discount_rate)) %>%
                rename(country = c..Interest.Rates..Discount..Percent.per.annum..Zimbabwe....Interest.Rates..Discount..Percent.per.annum..Zambia...)
dr_gathered$country <- gsub('Interest Rates, Discount, Percent per annum, ',
                                       '', dr_gathered$country)
dr_gathered$year <- gsub('X', '', dr_gathered$year) %>%
                                as.numeric()
dr_gathered <- subset(dr_gathered, !is.na(discount_rate))

# Combined discount rate for Euro area countries
dr_euro <- subset(dr_gathered, country == 'Euro Area') %>% select(-country)
dr_euro$eurozone <- 1

# Insert discount rate for the United Kingdom
dr_uk <- read.csv('data/raw/BoE_annual_discount_rate.csv',
                    stringsAsFactors = FALSE) %>%
                rename(year = DATE, discount_rate = IUAABEDR)
dr_uk$year <- dr_uk$year %>% dmy() %>% year()
dr_uk$country <- 'United Kingdom'

dr_gathered <- bind_rows(dr_gathered, dr_uk)

# Insert Norwegian key rate
dr_no <- read.csv('data/raw/norway_key_rate.csv',
                  stringsAsFactors = FALSE) %>%
            rename(year = date, discount_rate = rate)
dr_no$country <- 'Norway'

dr_gathered <- bind_rows(dr_gathered, dr_no)

# Insert Swedish discount/reference rate
dr_se <- read.csv('data/raw/sweden_annual_reference_discount_rate.csv',
                  stringsAsFactors = FALSE) %>%
            rename(year = date, discount_rate = rate)
dr_se$country <- 'Sweden'

dr_gathered <- bind_rows(dr_gathered, dr_se)

# Insert Russian discount rate from FRED
dr_ru <- as.data.frame(
    getSymbols(Symbols = 'INTDSRRUM193N', src = 'FRED', auto.assign = F))
dr_ru$date <- row.names(dr_ru)
dr_ru$year <- dr_ru$date %>% ymd() %>% year()
dr_ru <- dr_ru %>% group_by(year) %>%
            mutate(discount_rate = mean(INTDSRRUM193N))
dr_ru <- dr_ru %>% select(year, discount_rate)
dr_ru <- dr_ru[!duplicated(dr_ru[, 'year']), ]
dr_ru$country <- 'Russia'

dr_gathered <- bind_rows(dr_gathered, dr_ru)

# Add iso2c
dr_gathered$iso2c <- countrycode(dr_gathered$country,
                                            origin = 'country.name',
                                            destination = 'iso2c')
dr_gathered <- select(dr_gathered, -country) %>%
                            filter(year >= 1990)

#### Merge #####################################################################
comb <- merge(wdi, assets_gathered, all.x = TRUE)
comb <- merge(comb, costs, all.x = TRUE)
comb <- merge(comb, dr_gathered, all.x = TRUE)
comb <- merge(eu, comb, all = TRUE)

# Clean post-merge
comb <- comb[!duplicated(comb[, c('iso2c', 'year')]), ]

comb$eurozone[is.na(comb$eurozone)] <- 0

comb <- FillIn(comb, dr_euro, Var1 = 'discount_rate',
               KeyVar = c('eurozone', 'year')) %>%
            arrange(iso2c, year)

temp_assets <- comb[, c('iso2c', 'year', 'DepositBankAssetsGDP')]
comb <- FillIn(comb, temp_assets, Var1 = 'BankAssetsGDP', 
               Var2 = 'DepositBankAssetsGDP')

#### Clean Up, lag, log ########################################################
# Create Major Currency
comb$major_currency <- 'Other'
comb$major_currency[comb$eurozone == 1] <- 'CH/DK/Euro/GB/JP/NO/SE/US'
major_currencies <- c('CH', 'DK', 'GB', 'JP', 'NO', 'SE', 'US')
comb$major_currency[comb$iso2c %in% major_currencies] <- 'CH/DK/Euro/GB/JP/NO/SE/US'
    
comb$major_currency <- factor(comb$major_currency)

# Create 1 year lags
for (i in c('CentralBankAssetsGDP', 'BankAssetsGDP', 'DepositBankAssetsGDP',
            'CapitalAssets')){
    comb <- slide(comb, Var = i, GroupVar = 'iso2c', slideBy = -1,
                  NewVar = paste0(i, '_lag1'))
}

# Create 3 year lead average for discount rate 
## i.e. average discount rate over the course of the 'crisis'
comb <- slideMA(comb, Var = 'discount_rate', GroupVar = 'iso2c',
                             periodBound = 2, offset = 0, 
                             NewVar = 'discount_rate_ma3')

# Domestic credit change and 3 year moving average
comb <- PercChange(comb, Var = 'DomesticCreditGDP', GroupVar = 'iso2c',
                   NewVar = 'DomesticCredit_change')
comb <- slideMA(comb, Var = 'DomesticCredit_change', GroupVar = 'iso2c',
                periodBound = -3, offset = 1, 
                NewVar = 'DomesticCredit_change_ma3')

# Keep only crisis years
comb_sub <- subset(comb, !is.na(LV2012.Fiscal))

# Standardise costs by banking system size
comb_sub$costs_deviation <- comb_sub$LV2012.Fiscal / comb_sub$BankAssetsGDP_lag1

# Create log variables
vars <- c('CentralBankAssetsGDP_lag1', 'BankAssetsGDP_lag1', 
          'DepositBankAssetsGDP_lag1', 'DomesticCredit_change_ma3',
          'discount_rate', 'discount_rate_ma3', 'LV2012.Fiscal', 
          'costs_deviation')
for (i in vars) {
    comb_sub[, paste0(i, '_log')] <- log(comb_sub[, i])
}

# Final clean up
# rmExcept('comb_sub')

comb_sub <- MoveFront(comb_sub, Var = c('iso2c', 'country', 'year',
                                        'major_currency', 'eurozone'))
comb_sub <- comb_sub %>% select(-region)

# Save data as CSV
write.csv(comb_sub, file = 'data/main_data.csv', row.names = FALSE)
