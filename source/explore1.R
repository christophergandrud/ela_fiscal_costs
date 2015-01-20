################################################################################
# Explore relationship between crisis costs, banking system size, and
# discount rate
# Christopher Gandrud
# 20 January 2015
# MIT License
################################################################################

# Load required packages
library(ggplot2)

# Set working directory. Change as needed.
setwd('/git_repositories/ela_fiscal_costs/')

# Load data created by ELA_data_gather.R
comb_sub <- read.csv('source/data/main_data.csv')

#### Exploratory plots ####
png('figures/Size_Costs.png')
ggplot(comb_sub, aes(BankAssetsGDP_lag1_log, LV2012.Fiscal,
                     label = iso2c, color = grouping)) +
    geom_text() +
    scale_color_brewer(palette = 'Set1') +
    stat_smooth(method = 'lm', se = F) +
    xlab('\nDeposit Bank Assets/GDP (log, 1 year lag)') +
    ylab('Fiscal Costs/GDP (Laven & Valencia)\n') +
    theme_bw()
dev.off()

png('figures/DiscountRate_CostsStnd.png')
ggplot(comb_sub, aes(discount_rate_ma3_log, costs_deviation_log,
                     label = iso2c)) +
    geom_text() +
    stat_smooth(method = 'lm', se = F) +
    xlab('\nDiscount rate (log, 3 year average lead)') +
    ylab('Fiscal costs / Bank Assets (log, 1 year lag)\n') +
    theme_bw()
dev.off()

#### Exploritory regressions
m1 <- lm(LV2012.Fiscal ~ discount_rate_log + BankAssetsGDP_lag1_log,
         data = comb_sub)

m2 <- lm(LV2012.Fiscal ~ CentralBankAssetsGDP_lag1_log,
         data = comb_sub)

m3 <- lm(costs_deviation_log ~ discount_rate_ma3_log,
         data = comb_sub)
