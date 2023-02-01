#===============================================================================================
# Polynomial taper curve by Heinonen et al. for plantation tree species in Zambia
#
# Used here to determine volumes of broken trees: v = vf * (vcut.int/vtot.int),
#           where: v        = predicted stem volume (dm³)
#                  vf       = volume (dm³) predicted using models of Sharma and Pukkala (1990) and
#                             total height predicted using height generalisation model for FRA
#                  vcut,int = stem volume (dm³) from stump height (15 cm) to the cut point of tree
#                             integrated taper curve model and using height generalisation model for FRA
#                  vtot,int = stem volume (dm³) from stump height (15 cm) to the tip of tree
#                             integrated taper curve model and using height generalisation model for FRA
#                  a1-a3    = parameters of correction polynomial
#                  b1-b8    = parameters of the relative taper curve (population mean) model, i.e. so called Fibonacci curve 
#
#  (c) Kalle Eerikainen, 24th August 2011                           #
#                                                                   #
#  Last update by:        Dr. Kalle Eerikäinen                      #
#  Last update date:      09/02/2012                                #
#
#      Modified version:
#        - modifications for Terai -dataset when using L. Methtatalo's R package lmfor for height modeling
#        -- 23/9/2013 / Heikki P.
#        - modifications for Siwalik -dataset when using L. Methtatalo's R package lmfor for height modeling
#        -- 26.02.2014/Ananda
#        - modifications for Midhills -dataset when using L. Methtatalo's R package lmfor for height modeling
#        -- 21.07.2014/Ananda
# -------------------------------------------------------------------------
# Fibonacci function
a.par=c(0,0,0)
b.par=c(2.05502,-0.89331,-1.50615,3.47354,-3.10063,1.50246,-0.05514,0.00070)

ht=20; x.m=1-1.3/ht

Fibonacci <- function(x.m, a.par, b.par){
  value=-999
  Pb=(a.par[1]+b.par[1])*x.m    +
    (a.par[2]+b.par[2])*x.m^2  +
    (a.par[3]+b.par[3])*x.m^3  +
    b.par[4] *x.m^5  +
    b.par[5] *x.m^8  +
    b.par[6] *x.m^13 +
    b.par[7] *x.m^21 +
    b.par[8] *x.m^34
  
  value=Pb
  value
}

# test tree
ht=20; d13=25.4; x.m=1-1.3/ht
# taper curve function -----------------------------------------
d.m.taper <- function(d13, x.m, ht, a.par, b.par){
  value = -999
  d.0.2h = d13/Fibonacci(1-1.3/ht, a.par, b.par)
  value = d.0.2h*Fibonacci(x.m, a.par, b.par)
  value
}
# plots above test tree taper curve ----------------------------
hl=seq(.15, ht, by=0.01); hl=hl[2:length(hl)]-(0.01/2)
dl=d.m.taper(d13, 1-(hl/ht), ht, a.par, b.par)
plot(hl,dl, cex=.2); abline(h=d13, col="red", lty="dashed"); abline(v=1.3, col="red", lty="dashed")


# volume calculation with taper curve --------------------------
v.taper <- function(d13, ht, ht.x, a.par, b.par){
  value = -999
  if(ht.x == ht){
    hl    = seq(.15, ht, by=0.01); hl=hl[2:length(hl)]-(0.01/2)
    dl    = d.m.taper(d13, 1-(hl/ht), ht, a.par, b.par)
    value = sum((pi*dl^2/4)/1000)
  }
  if(ht.x  < ht){
    hl.x  = seq(.15, ht.x, by=0.01); hl.x=hl.x[2:length(hl.x)]-(0.01/2)
    dl.x  = d.m.taper(d13, 1-(hl.x/ht), ht, a.par, b.par)
    value = sum((pi*dl.x^2/4)/1000)
  }
  value
}

# function for calculating volume ratio -------------------------------------- 
v.ratio.broken.top.trees <- function(d13, ht, ht.x, crown_class, a.par, b.par){
  value = 1
  # if(crown_class == 6 & ht.x > 0) # not checking for crown_class needed, because only broken trees in dataset
  {
    v.t.height_p      = v.taper(d13, ht, ht, a.par, b.par)   # volume up to prognosed height (ht), 
    v.t.actual.height = v.taper(d13, ht, ht.x, a.par, b.par) # volume to broken top height (to height ht.x)
    value             = v.t.actual.height/v.t.height_p
    #                         if(value < 0){value = 1}      # height < height_p
  }
  value
}

#v.ratio.broken.top.trees(d13, ht, ht.x, crown_class, a.par, b.par)
#===============================================================================================
# volume_ratio needs to be calculated for trees: crown_class==6 and crown_class (7,8) and sample_tree ==3
# dataset H same as when modeling heights, can be loaded from database with 'read_tree_data_from_potgres_db.R'
# H dataset should be formulated as in above script
# H_broken is subset of trees that need volume_ratio different from default 1.
finaltree2 <- read.csv("outputs/1_alltree_height_imputed.csv")
H <- finaltree2
H_broken <- subset (H,H$crown_class==6 | 
                      (H$crown_class==7 & H$sample_tree==3 | H$crown_class==8 & H$sample_tree==3)
)
#H_broken$height_m
#H_broken[ is.na(H_broken$height_m),]$height_m <- 0

# calculating ratio for above trees
#
H_broken$volume_ratio = NA    # first all will get N/A
for(i in 1:nrow(H_broken)){
  # case when measured height below modeled ... 
  if(( H_broken$height_m[i]  < H_broken$height_p[i])& H_broken$height_m[i]>0 ){
    H_broken$volume_ratio[i] = v.ratio.broken.top.trees(H_broken$diameter_p[i], 
                                                        H_broken$height_p[i], H_broken$height_m[i], H_broken$crown_class[i], a.par, b.par)}
  # case where measured height is less than prognosed height ... 
  ##  add 10 % to measured height to get approx. of height without broken ... heikkip. 13.8.2013 
  if( H_broken$height_m[i]  >= H_broken$height_p[i]){
    H_broken$volume_ratio[i] = v.ratio.broken.top.trees(H_broken$diameter_p[i], 
                                                        H_broken$height_m[i]*1.1, H_broken$height_m[i], H_broken$crown_class[i], a.par, b.par)} 
  # case when broken tree has no measured height
  if( H_broken$height_m[i] == 0 ){
    H_broken$volume_ratio[i] = v.ratio.broken.top.trees(H_broken$diameter_p[i], 
                                                        H_broken$height_p[i], H_broken$height_p[i]*0.9, H_broken$crown_class[i], a.par, b.par)}    
}

## H_broken_0_height <- subset (H_broken,H_broken$height_m==0)  # just checking trees with no height measurements

H_not_broken   <- H[ ! (H$id %in%  H_broken$id),]
H_not_broken$volume_ratio <- 1

finaltree3 <- rbind(H_not_broken,H_broken )
write.csv(finaltree3, "outputs/2_alltree_height_imputed_diam.csv", row.names = FALSE)



