# ---------------------------------
# Liquidity components from IMF International Financial Statistics
# Christopher Gandrud
# MIT License
# ---------------------------------

# Load required packages
library(imfr)
library(rio)
library(dplyr)
library(lubridate)
library(countrycode)
library(ggplot2)
theme_set(theme_bw())

# Set working directory. Change as needed.
setwd('/git_repositories/ela_fiscal_costs/source/')

# List of Euro area members -----------
euro <- import('https://raw.githubusercontent.com/christophergandrud/euro_membership/master/data/euro_membership_data.csv')

euro_iso2c <- unique(euro$iso2c)

# Find IMF codes ----------
imf_codelist('ifs')
ifs_codes <- imf_codes('CL_INDICATOR_IFS')
ifs_geo <- imf_codes('CL_AREA_IFS')

# Divide up currencies and download data ----------
data("all_iso2c")
not_eur_us <- subset(all_iso2c, !(iso2c %in% c('US', euro_iso2c)))

# FASAD_* Central Bank Survey, Claims on Other Depository Corporations
# FOST_* Other Depository Corporations Survey, Transferable Deposits Included In Broad Money
# 25____* Non-standardized Presentation (Deposit Money Banking), Time, Savings, and Foreign Currency Deposits
# * indicates position of currency identifier

# U2 Euro area (member states + ECB)

indicators_to_download <- c('FASAD_XDC', 'FOST_XDC', '25____XDC')
same_names <- c('iso2c', 'month_year', 'cb_claims', 'transf_deposits')

# Non-Euro/US currencies
claims_not_euro_us <- imf_data(database_id = 'IFS',
                   indicator = c('FASAD_XDC', 'FOST_XDC'),
                   start = 1980, end = 2016, freq = 'M',
                   country = not_eur_us$iso2c) %>%
    setNames(same_names)

# Euro area (poor coverage for 25___)
claims_eur <- imf_data(database_id = 'IFS',
                        c('FASAD_EUR', 'FOST_EUR'),
                        start = 1980, end = 2016, freq = 'M',
                        country = c(euro_iso2c, 'U2')) %>%
    setNames(same_names)

# USD denominated
claims_usd <- imf_data(database_id = 'IFS',
                       indicator = c('FASAD_USD', 'FOST_USD'),
                       start = 1980, end = 2016, freq = 'M',
                       country = 'US') %>%
    setNames(same_names)

# Combine
comb <- rbind(claims_not_euro_us, claims_eur)
comb <- rbind(comb, claims_usd)

# Cleanup date variable
comb$month_year <- paste0(comb$month_year, '-01')

# Find liquidity provision (CB claims on depository / Transferable Deposits)
comb$liquidity <- (comb$cb_claims / comb$transf_deposits) * 100

# Save data --------------------
export(comb, file = 'data/raw/ifs_cb_liquidity.csv')
comb <- import('data/raw/ifs_cb_liquidity.csv')

comb$month_year <- ymd(comb$month_year)

# Visually examine -------------
comb$country <- countrycode(comb$iso2c, origin = 'iso2c', 
                            destination = 'country.name')
comb$country[comb$iso2c == 'U2'] <- 'Euro Area'


sub_countries <- c('U2', 'IE', 'DE', 'IT', 'US', 'KR', 'FR', 'AT', 'SI',
                   'ES', 'PT', 'IS', 'DK', 'JP', 'GR', 'AU', 'BE', 'JP',
                   'EE')
sub_countries <- sub_countries[order(sub_countries)]

sub <- comb %>% filter(iso2c %in% sub_countries)
sub <- subset(sub, !is.na(liquidity))

ggplot(sub, aes(month_year, liquidity)) +
    facet_wrap(~ country, scales = 'free') +
    geom_line() +
    xlab('') + ylab('CB Claims on Deposit Corps. to Transferable Deposits (%)\n')

ggsave(filename = 'figures/liquidity_sample.pdf')

sub_countries <- c('IE', 'DE', 'SI')
sub <- comb %>% filter(iso2c %in% sub_countries)
sub <- subset(sub, !is.na(liquidity))

ggplot(sub, aes(month_year, liquidity, group = country)) +
    geom_line() +
    xlab('') + ylab('CB Claims on Deposit Corps. to Transferable Deposits (%)\n')

# Euro area
sub_countries <- euro_iso2c
sub_countries <- sub_countries[order(sub_countries)]

sub <- comb %>% filter(iso2c %in% sub_countries)
sub <- subset(sub, !is.na(liquidity))

ggplot(sub, aes(month_year, liquidity)) +
    facet_wrap(~ iso2c) +
    geom_line() +
    xlab('') + ylab('CB Claims on Deposit Corps. to Transferable Deposits (%)\n')



# FinStress
URL <- 'https://raw.githubusercontent.com/christophergandrud/EIUCrisesMeasure/master/data/FinStress.csv'
finstress_index <- rio::import(URL) %>% setNames(c('iso2c', 'country', 
                                                   'month_year', 'finstress'))

comb <- merge(comb, finstress_index, by = c('iso2c', 'month_year'))

m1 <- lm(liquidity ~ finstress + as.factor(iso2c), data = comb)
