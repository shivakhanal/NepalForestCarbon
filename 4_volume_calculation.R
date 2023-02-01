
######### Calculate tree volume  ##################
#   Volumes & timber ratios & Biomass values are calculated using allometric models published by Sharma&Pukkala,1990
# c) replace missing heights in height_m with height_p
finaltree <- read.csv("outputs/3_alltree_height_imputed_diam_treesha.csv")
finaltree2 <- finaltree
finaltree2[ finaltree2$height <= 0,]$height <- NA
finaltree2$height_m <- ifelse(is.na(finaltree2$height), 
                              finaltree2$height_p,
                              finaltree2$height_m)

#####  3 Volume(cu.m.)/ha	##### 
# Volume and basal area calculation can be adapted from sql once tree/ha and ba/ha is sorted out
#mdtrees$vol	<- exp()      #=((EXP(EW$27+EX$27*LN(AP2)+EY$27*LN(AS2)))/1000)
mdtrees <- finaltree2
mdtrees$vol	<- NA
#mdtrees[ (mdtrees$sample_tree < 3 & mdtrees$crown_class <6 & mdtrees$height_m > 1.3),]$vol <- 

volBiomass <- read.delim("Cal_data/join_all_vol_densty_biomass_ratio_Feb6_2015", sep=";")
volPara <- read.delim("Cal_data/ratio_volume_Feb6_2015", sep=";")
# first equation is the equation for volume with bark 
volPara <- volPara[ volPara$equation ==1, ]

# testFunc <- function(a, b) a + b
# apply(dat[,c('x','z')], 1, function(y) testFunc(y['z'],y['x']))

volCalc <- function(species, diameter_p, height_m){
  number <- volBiomass[ volBiomass$fra_species_code == species,]$vol_model
  volumeabc <- volPara[ volPara$number == number, ]
  (exp(volumeabc$a + volumeabc$b*log(diameter_p) + volumeabc$c*log(height_m))/1000)
}

# Class 10 is climber
table(mdtrees$crown_class)
mdtrees <- mdtrees[ mdtrees$crown_class < 10,]

#-- bamboo species (FRA code 457 between 466)
#-- bamboos where height is measured in the field and not broken
# remove bamboos
mdtrees <- mdtrees[ (mdtrees$species < 457 | mdtrees$species > 467),] 
mdtrees <- mdtrees[ mdtrees$species != 466, ]
# "538";"Dendrocalamus hamiltonii"
mdtrees <- mdtrees[ mdtrees$species != 538, ]
mdtrees <- mdtrees[ mdtrees$species != 459, ]

#xx <- apply(mdtrees[,c('species','diameter_p', 'height_m')], 1, 
#            function(y) volCalc(y['species'],y['diameter_p'],y['height_m']))
mdtrees$vol <- NA
for (row in 1:nrow(mdtrees)) { 
  mdtrees$vol[row] <- volCalc(mdtrees$species[row], mdtrees$diameter_p[row], mdtrees$height_m[row]) 
}

mdtrees$vol_ha <- mdtrees$vol* mdtrees$trees_ha

write.csv(mdtrees, "outputs/4_alltree_height_imputed_diam_volume.csv", row.names = FALSE)

# somechecks
summary(mdtrees[ mdtrees$plot %in% "29-96-6",]$vol)

