# read in the tree level data with calculated tree level volume
mdtrees <- read.csv("outputs/4_alltree_height_imputed_diam_volume.csv")
# -- *volume_ratio      
# -- ratio for broken trees not used for vol here, use when calculating ha values and mass !!!


# biomass includes Stem, Branch and Foliage components 
#####   Stem Biomass (M. ton-air dry)/ha		##### 
volBiomass <- read.delim("Cal_data/join_all_vol_densty_biomass_ratio_Feb6_2015", sep=";")
denPara <- read.delim("Cal_data/ratio_density_Feb6_2015", sep=";")

# calculate the mass of the stem
massCalc <- function(species, vol){
  number <- volBiomass[ volBiomass$fra_species_code == species,]$density_model
  dens <- denPara[ denPara$number == number, ]$density
  vol * dens
}

mdtrees$mass_stem <- NA
for (row in 1:nrow(mdtrees)) { 
  mdtrees$mass_stem[row] <- massCalc(mdtrees$species[row], mdtrees$vol[row]) 
}

# calculate the mass of the stem per hectare
mdtrees$mass_stem_ha <- mdtrees$mass_stem * mdtrees$trees_ha/1000 

#####  5 Branch Biomass (M. ton- air dry)/ha	##### 
# The thresholds for small, medium and big trees are < 25 cm, 25 -50 cm and > 50 cm dbh respectively

volBiomass <- read.delim("Cal_data/join_all_vol_densty_biomass_ratio_Feb6_2015", sep=";")
# calculate the mass of the branch
branchRat <- read.delim("Cal_data/ratio_branch_foliage_Feb6_2015", sep=";")

branchBioCalc <- function(species, diameter_p, mass_stem){
  number <- volBiomass[ volBiomass$fra_species_code == species,]$branch_foliage_model
  branchRatSub <- branchRat[ branchRat$number == number, ]
  if(diameter_p < 25 ){
    tot.price <- branchRatSub$bps_repr_pole  * mass_stem
  } else if(diameter_p > 25 & diameter_p < 50){
    tot.price <- branchRatSub$bps_small_timber  * mass_stem
  } else {
    tot.price <- branchRatSub$bps_large_timber * mass_stem
  }
}

mdtrees$mass_branch <- NA

# calculate the mass of the branch for each tree
for (row in 1:nrow(mdtrees)) { 
  mdtrees$mass_branch[row] <- branchBioCalc(mdtrees$species[row], mdtrees$diameter_p[row], mdtrees$mass_stem[row]) 
}

# calculate the mass of the branch per hectare
mdtrees$mass_branch_ha <- mdtrees$mass_branch	* mdtrees$trees_ha/1000


#####  6 Foliage (M. ton-air dry)/ha		##### 

# calculate the mass of the foliage
foliageBioCalc <- function(species, diameter_p, mass_stem){
  number <- volBiomass[ volBiomass$fra_species_code == species,]$branch_foliage_model
  branchRatSub <- branchRat[ branchRat$number == number, ]
  if(diameter_p < 25 ){
    branchRatSub$fps_repr_pole  * mass_stem
  } else if(diameter_p > 25 & diameter_p < 50){
    branchRatSub$fps_small_timber  * mass_stem
  } else {
    branchRatSub$fps_large_timber * mass_stem
  }
}

mdtrees$mass_foliage <- NA
# calculate the mass of the foliage for each tree
for (row in 1:nrow(mdtrees)) { 
  mdtrees$mass_foliage[row] <- foliageBioCalc(mdtrees$species[row], mdtrees$diameter_p[row], mdtrees$mass_stem[row]) 
}

#mdtrees$mass_branch <- mdtrees$vol_ha * mdtrees$trees_ha
# calculate the mass of the foliage per hectare
mdtrees$mass_foliage_ha <- mdtrees$mass_foliage	* mdtrees$trees_ha/1000

#####  7 mass_stump_ha(M. ton-air dry)/ha		##### 
# calculate the volume of the stump per hectare
mdtrees$vol_stump =(pi*(1.2*mdtrees$diameter_p)^2)/40000*0.15*1.5

mdtrees$vol_stump_ha =mdtrees$vol_stump*mdtrees$trees_ha

# calculate the mass of the stump
massCalc <- function(species, vol){
  number <- volBiomass[ volBiomass$fra_species_code == species,]$density_model
  dens <- denPara[ denPara$number == number, ]$density
  vol * dens
}

# calculate the mass of the stump per hectare
mdtrees$mass_stump <- NA
for (row in 1:nrow(mdtrees)) { 
  mdtrees$mass_stump[row] <- massCalc(mdtrees$species[row], mdtrees$vol_stump[row]) 
}


mdtrees$mass_stump_ha <- mdtrees$mass_stump * mdtrees$trees_ha/1000 

### Test example
# diameter = 44.7
# height = 0.15
# species = 0
# #vol_stump	vol_stump_ha	mass_stump	mass_stump_ha
# #0.050845197	0.404613225	34.26966305	272.7093137
# volCalc(0, 44.7, 0.15 )


#####  8 Total Biomass(M. ton-air dry)/ha		##### 
# remove foliage proportion from dead standing trees 
mdtrees[mdtrees$crown_class == 7,][ c("mass_branch", "mass_branch_ha", "mass_foliage", "mass_foliage_ha")] <- 0

# Total Biomass(M. ton-air dry)/ha	= Stem Biomass/ha (M. ton) +	Branch Biomass/ha (M. ton) +	Foliage Biomass/ha (M. ton)
mdtrees$total_mass_air_dry <-   mdtrees$mass_stem_ha + mdtrees$mass_branch_ha + mdtrees$mass_foliage_ha + mdtrees$mass_stump_ha
mdtrees$total_oven_dry_mass <- mdtrees$total_mass_air_dry/1.1
mdtrees$carbon <- mdtrees$total_oven_dry_mass *0.47

#### 9 Aggregate to plot-level forest AGB #####
##### read AGB, add coordinates and save 
# remove the trees that are not in the crown class
mdtrees <-  mdtrees[mdtrees$crown_class != 10,]

# split the data by plot
trees.by.plot <- split(mdtrees, mdtrees$plot)

# create a list to store the data
mydat <- list()
# loop through the list of plots
for(i in 1:length(trees.by.plot)) { 
  # get the plot number
  a<- trees.by.plot[[i]]$plot[1] 
  # get the total biomass
  b<- (sum((trees.by.plot[[i]]$total_mass_air_dry)))
  # get the mean height
  c<- (mean((trees.by.plot[[i]]$height_p)))
  # store the data in the list
  mydat[[i]] <- c(a, b, c)
}

mybind <- do.call("rbind", mydat)
mybind<- data.frame(mybind)

# add column names
colnames(mybind)<- c("plot", "biomass_ha", "height")

# convert the biomass and height columns to numeric
mybind[,2] <- as.numeric(mybind[,2])
mybind[,3] <- as.numeric(mybind[,3])

# read the plot coordinates
aa<- read.csv("Cal_data/Phase1_data_Feb6_2015_csv.csv")
# create a plot number column
aa$plot <- paste(aa$col , aa$row , aa$plot_number , sep="-")
# keep only the plot number and coordinates
aa<- aa[, c("plot", "lon", "lat")]
# merge the plot coordinates with the biomass data
findat<- merge(aa, mybind)
# write the data to a csv file
write.csv(findat, "outputs/plotwise_biomass_t_ha.csv")
