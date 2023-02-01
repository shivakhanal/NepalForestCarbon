# read in the AGB and SOC data
agb <- read.csv("outputs/plotwise_biomass_t_ha.csv")
soc <- read.csv("outputs/plotwise_SOC_t_ha.csv")
names(soc)[ncol(soc)] <- "plot"

# select the columns from the AGB data
agb <- agb[, 2:5]

agb0 <- soc[ is.na(soc$AGB),]
# rename the SOC column to biomass_ha
names(agb0)[2] <- "biomass_ha"
# append the plots with 0 AGB but some SOC to the AGB data
agb0 <- agb0[, names(agb)]

# merge the AGB data with the SOC data
agball <- rbind(agb, agb0)
findat <- merge(agball, soc[, c(2, 8, 10)], by = "plot")

# select the columns to be written to the output file
findat <- findat[, c(1:4, 6)]
names(findat) <- c("plot_id", "lon", "lat", "AGB_tha", "SOC_tha")

# check the spatial locations of the plots
with(findat, plot(lon, lat))
write.csv(findat, "outputs/Nepal_forest_AGB_SOC_data.csv")

