
# ## Calculation steps

#### Sample dry mass, bulk density and C Store calculation includes few steps
# Fresh mass from field form, g ………………. (A)
# Tray empty mass g ………………………………..(B)
# Sample+ tray dry mass g ……………………….(C) 
# Nr of volumetric samples, if none give 0 ……………….. (D)
# Total volume of non-corer subsamples, ml …………….. (E) 
# Total volume of  composite sample cm3  (Corer radius 18.5 mm ) ………….(S)
# Pi * 18.5 *18.5 *100/1000 * D + E
# Soil water content g g-1 ……………………………… (A – C + B)/A
# Soil water content ml ml-1 at 20C-25C …………. 1/0.998 * (A – C + B)/S
# Volume of stones in sample ml ………………………..V=  Coarse fraction volume (water and stones ml) – coarse fraction volume (water added ml)
# % of OC ……………………………O 
# Bulk density g cm-3 ……………………………………F= (C-B)/S
# C concentration  g cm-3……………………………….. G =  F * (S-V)/S*O/100
# Layer C store Kg m-2…………………………………… H = G*1000*100/1000
# Layer C store t ha-1…………………………………… I = H*10000/1000

#### Titration steps
# Titrant consumption, ml …… J= Initial – Final 
# Blank reading (ml) ……………… K
# FAS Normality, N…………….. L=  10/K
# Sample mass, g ……………….. M 
# % of OC………………………. O = (K- J)* L * 0.003 *1.3 * 100/M

#read in the data
dat <- read.csv("testData_massBD.csv")
#replace NA values with 0
dat$Total.volume.of.non.corer.subsamples..ml [ is.na(dat$Total.volume.of.non.corer.subsamples..ml)] <- 0
dat$Nr.of.volumetric.samples..if.none.give.0 [ is.na(dat$Nr.of.volumetric.samples..if.none.give.0)] <- 0
#calculate the sample volume
dat$S <-  pi * 18.5 * 18.5 * 100/1000 * (dat$Total.volume.of.non.corer.subsamples..ml + dat$Nr.of.volumetric.samples..if.none.give.0)
#calculate the soil water content
dat$swc <- (dat$Fresh.mass.from.field.form..g - dat$Tray.empty.mass.g.1 + dat$Tray.empty.mass.g)/dat$Fresh.mass.from.field.form..g
#calculate the soil water content per ml
dat$swcml <- 1/0.998 *(dat$Fresh.mass.from.field.form..g - dat$Tray.empty.mass.g.1 + dat$Tray.empty.mass.g)/dat$S
#calculate the volume of water and stones
dat$V <-  dat$water.and.stones.ml - dat$Water.added.ml
#calculate the tray empty mass
dat$C <- dat$Tray.empty.mass.g.1
dat$B <- dat$Tray.empty.mass.g
#calculate the bulk density
dat$F <- (dat$C-dat$B)/dat$S
#remove layer column
dat <- dat[ ,!colnames(dat) %in% "Layer"]

#merge the two data sets
#read in the data from the titration sheet
dato <- read.csv("testData_titration.csv")
dat <- merge(dat, dato[, c("Lab.code", "X..of.O.C.", "Layer", 
                           "Column", "Row", "Plot_number")], by.x= "Stand_number",by.y = "Lab.code" )
#remove litter if exists
dat <- dat[ !(dat$Layer) %in% "Litter",]
#calculate the C concentration
dat$O <- dat$X..of.O.C.
dat$G <- dat$F * (dat$S-dat$V)/dat$S*dat$O/100
#calculate the layer C store
dat$H <- dat$G * 1000 * 100/1000
#calculate the layer C store per ha
dat$I <- dat$H * 10000/1000

################ Now aggregate to total 
dat$Layer <- as.character(dat$Layer)
unique(dat$Layer)
dat <- dat[, c("Column", "Row", "Plot_number", "Stand_number",
               "Layer", "I")]

dat$tha <- as.numeric(as.character(dat$I))
dat$tha <- abs(dat$I)
# load the plyr package
library(plyr)
# Summarise the plots and layer SOC estimates 
ddata <- ddply(dat, c("Column", "Row", "Plot_number", "Layer"), summarise,
               tot = sum(tha, na.rm=T))
ddata

#load the tidyr package
library(tidyr)
#spread the data from long to wide format
aaa <-spread(ddata, Layer, tot)
#print the first 10 rows of the data
aaa[1:10,]
#mean of the values in columns 4-6
aaa$total <- rowMeans(aaa[,c(4:6)], na.rm = T) 
# combine col, row and plot number in a single column
aaa$plot_id <- paste(aaa$Column, aaa$Row, aaa$Plot_number, sep="-")
write.csv(aaa, "outputs/plotwise_SOC_t_ha.csv")

############################

# validate against the SOC that I used
soc <- read.csv("../../3_EnvironmentalCorrelates/1_paper1wileySim/data/finaldat.csv")

# test 1
plotid <- "30-82-3"
soc[ soc$plt_d %in% plotid,]
aaa[ aaa$plot_id %in% plotid,]

library(ggplot2)
ggplot(soc, aes(AGB, SOC)) + geom_hex()

names(soc)[ncol(soc)] <- "plot"
hist(soc$SOC, col= 2)
names(aaa)[8] <- "plot"

table(aaa$plot %in% soc$plot)
plotid <- "30-82-3"
plotid <- "31-79-4"
aaa[ aaa$plot %in% plotid,]$total == soc[ soc$plot %in% plotid,]$SOC 

# it seems, the former used litter in the total 
xxx <- merge(aaa, soc[ , 8:10], by = "plot")
with(xxx, plot(total, SOC))

