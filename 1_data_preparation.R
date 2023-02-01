

# This code does the basic calculation of individual tree measurements for each sampled plots and aggregates the foest AGB estimates to plot level. 

### 1 read tree level data data ########
tree <- read.table("Tree_Feb6_2015", sep=";", header=TRUE)
tree <- tree[, 1:34]

# remove climbers
# to maintain tree numbers lets keep climbers
#tree <- tree[ tree$crown_class != 10, ]

## H$h variable to have only sample trees heights
# -- other tallied trees should have NA in modelling, just to be sure
H<-tree 
H$height_m<-ifelse(H$height>0 & 
                     (H$sample_tree==1 | H$sample_tree==2) , H$height, NA)

H$height_m <- ifelse( H$base > 0, sqrt(H$height_m^2 + H$base^2),
                      H$height_m)

## select suspicious trees to subset if needed (just an example)
susp_trees <- subset(H, H$diameter_p >=5 & H$h <= 0.25* H$diameter_p, )
# none good?

### 2 missing tree height imputation ########

######## 2 Plot fits to evaluate height fits #############################################################
# This code applies function from lmfor package to impute missing tree heights and also plots the 
# residual variance of the model for some (could be many!!!) species the model would not converge 
# so the try argument will look for those that converge and hopefully provide some examples to put in report 
# annex 

dir.create("outputs")
pdf("outputs/ALL_residual_plots.pdf", onefile=TRUE)
for(i in unique(H$species))try({
  H_x<- subset (H,H$species==i) #6651 is an example that works without error
  #H_6615<- subset (H_6615,H_6615$stratum==5)
  
  #lets set all the trees less thatn 1.3m height to NA
  # this will of course include missing ones coded as -9999 in the database
  H_x[(H_x$height <= 1.3),]$height <- NA
  #H_6615[(H_6615$height == -9999),]$height <- NA
  
  # I am renaming my dataset and variable so that I do not mess up anything and
  # follow the clear example provided by Lauri Mehtatalo.
  spati<-H_x
  spati$plot <-spati$plot_id
  
  spati$d <-spati$diameter
  spati$h <-spati$height
  
  #lmeControl(msMaxIter = 50, msVerbose = TRUE)
  
  #windows()
  ImpFixed <-ImputeHeights(spati$d,spati$h,spati$plot,level=0)
  ImpRandom<-ImputeHeights(spati$d,spati$h,spati$plot,level=1,makeplot=FALSE)
  ImpRanRes<-ImputeHeights(spati$d,spati$h,spati$plot,level=1,addResidual=TRUE,makeplot=FALSE)
  
  par(mfcol=c(2,2), oma=c(2,0,2,0))
  
  plot(spati$d[!is.na(spati$h)],
       spati$h[!is.na(spati$h)],
       col=spati$plot[!is.na(spati$h)],
       main="Observations", xlab="d, cm", ylab="h, m",
       ylim=c(0,30))
  
  plot(spati$d[ImpFixed$imputed],
       ImpFixed$h[ImpFixed$imputed],
       col=spati$plot[ImpFixed$imputed],
       main="Imputed, Naslund, Fixed", xlab="d, cm", ylab="h, m",
       ylim=c(0,30))
  
  plot(spati$d[ImpRandom$imputed],
       ImpRandom$h[ImpRandom$imputed],
       col=spati$plot[ImpRandom$imputed],
       main="Imputed, Naslund, Fixed + Plot", xlab="d, cm", ylab="h, m",
       ylim=c(0,30))
  
  title(paste( "species", i, sep="-"), outer=TRUE)
  
})

dev.off()

######## 3 Functon to derive modelled height #############################################################
#T_6296<-cbind.data.frame(H_6296$id,H_6296$col,H_6296$row,H_6296$plot_number,H_6296$tree_number,H_6296$species,H_6296$d,H_6296$h,ImpRandom_6296$hpred)
#names(T_6296)=c("id", "col", "row", "plot_number", "tree_number","species", "d", "h", "hpred")
# i =13, species code 36 is climber
#"36";"Bauhinia vahlii"
#H_Group_1<- subset (H,H$species==6278|H$species==6280|H$species==6281 |H$species==6277)
#H_Group_2<- subset (H,H$species==6560|H$species==6568|H$species==6565 |H$species==6564
##                    |H$species==6563|H$species==6561 |H$species==6559 |H$species==6558)

xx <- sort(table(H$species))
H$species_coded <- ifelse(H$species %in% as.numeric(names(xx[ xx < 150])), 
                          99999, H$species)

# the code will fail for all species as only one model will converge for some species
# I found that the code fails for # fails <- c(6, 9, 12, 13, 20, 25, 42)

sppList <- unique(H$species_coded)
#sppList1 <- sppList

# I do not rememver the logic behind using two groups so
sppList1 <- sppList#[ ! (1:length(sppList) %in% c(16)) ]
#sppList2 <- sppList[ c(16)]

# so first subset is:
H1 <- H#[ ! (H$species_coded %in% sppList[c(16)]), ]
#H2 <- H[ H$species_coded %in% sppList[c(16)], ]

# for climbers keep the heights but is excluded later anyway
#i <- 13 # specis group code 36
#subset (H1,H1$species_coded==sppList1[i])

# 6564 alone fails so lets recode it into 6565
H1[ H1$species_coded == 6564,]$species_coded <- 6565
sppList1 <- unique(H1$species_coded)

library(lmfor)
dat <- list()
for(i in 1:length(sppList1)) try({
  cat(i, "\n")
  H_x<- subset (H1,H1$species_coded==sppList1[i]) #6651 is an example that works without error
  #lets set all the trees less than 1.3m height to NA
  # this will of course include missing ones coded as -9999 in the database
  #H_x[(H_x$height_m <= 1.3),]$height_m <- NA
  
  #H_6615[(H_6615$height == -9999),]$height <- NA
  spati<-H_x
  #spati$plot <-spati$plot_id
  #spati$d <-spati$diameter
  #spati$h <-spati$height
  
  models <- c("naslund", "curtis")
  
  om<- try(ImpRandom<- ImputeHeights(spati$diameter,spati$height_m,spati$plot_id,
                                     level=1,makeplot=FALSE, modelName = 'naslund',
                                     control = nlmeControl (maxIter = 300,
                                                            minScale = 0.01,
                                                            pnlsMaxIter = 10,
                                                            msMaxIter = 200,
                                                            tolerance = 0.01 )))
  
  if("try-error" %in% class(om)) ImpRandom<- ImputeHeights(spati$diameter,spati$height_m,spati$plot_id,
                                                           level=1,makeplot=FALSE, modelName = 'curtis',
                                                           control = nlmeControl (maxIter = 300,
                                                                                  minScale = 0.01,
                                                                                  pnlsMaxIter = 10,
                                                                                  msMaxIter = 200,
                                                                                  tolerance = 0.01 ))
  spati$height_p <- ImpRandom$hpred
  dat[[i]] <- spati
  # rm(ImpRandom1) 
  # rm(ImpRandom2) 
  rm(ImpRandom)
  
})

finaltree1 <- do.call("rbind", dat)
#put back height fo r topbroken 
finaltree1$height_m<-ifelse(finaltree1$sample_tree == 3, finaltree1$height, finaltree1$height_m)
write.csv(finaltree1, "outputs/1_alltree_height_imputed.csv", row.names = FALSE)
