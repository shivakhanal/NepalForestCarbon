
mdtrees <- read.csv("outputs/2_alltree_height_imputed_diam.csv")

mdtrees$plot <- paste(mdtrees$col , mdtrees$row , mdtrees$plot_number , sep="-")
mdtrees$diameter_p <- mdtrees$diameter
#trees.by.plot <- split(mdtrees, mdtrees$plot)
######## 1 No. of trees/ha	 ###################
#- from the plot with the radius of 20 m (r1, area: 1256.6 m 2 ) all big-size trees with diameter 
#at breast height equal to or greater than 30 cm are measured; 

#- from the second largest plot with the radius of 15 m (r2, area: 706.9 m 2 ) trees with diameter 
#at breast height from 20 to less than 30 cm are measured;

#- from the third largest plot with the radius of 8 m (r3, area: 201.1 m 2 ) trees with diameter 
#at breast height from 10 to less than 20 cm are measured; and

#- from the plot with the radius of 4 m (r4, area: 50.3 m 2 ) trees with diameter at breast height 
#from 5 to less than10 cm are measured

# Function to apply expansion factor  
expansionFac <- function(diameter_p){
  if(diameter_p >= 30 ){
    10000 / (pi * 20^2)
  } else if(diameter_p >= 20 & diameter_p < 30){
    10000 / (pi * 15^2)
  } else if(diameter_p >= 10 & diameter_p < 20){
    10000 / (pi * 8^2)
  } else {
    10000 / (pi * 4^2)
  }
}

mdtrees$trees_ha <- NA
for (row in 1:nrow(mdtrees)) { 
  mdtrees$trees_ha[row] <- expansionFac(mdtrees$diameter_p[row]) 
}

# but for trees measured on partial plots (i.e part of the plot not accessible)
completeAM <- c(50.2654824574367, 201.061929829747, 706.858347057704, 1256.63706143592) 

mdtrees$trees_ha <- ifelse(mdtrees$areaMeasured %in% completeAM,
                           mdtrees$trees_ha, 10000/mdtrees$areaMeasured
)
#trees.by.plot <- split(mdtrees, mdtrees$plot)
#vol<-lapply(trees.by.plot, sum(total_mass_air_dry * EXPF) )
#plot.sums <-data.frame(t(sapply(trees.by.plot, get.plot.sums)))

#####  2 B/A(Sq.m.)/ha	##### 
mdtrees$ba =pi*mdtrees$diameter_p^2/40000
mdtrees$ba_ha = mdtrees$ba * mdtrees$trees_ha

# remove basal area for stumps, recode with 0 to match with DFRS
mdtrees[ mdtrees$crown_class == 9,]$ba <- 0 
mdtrees[ mdtrees$crown_class == 9,]$ba_ha <- 0 

write.csv(mdtrees, "outputs/3_alltree_height_imputed_diam_treesha.csv", row.names = FALSE)
