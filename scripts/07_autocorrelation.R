
#looking for spatial autocorrelation in activity center data

library(data.table)
library(spdep)

coords <- fread('COA_AC_Work/distances/COA_AC_2021_STN.csv')
head(coords)

#
dists=as.matrix(dist(cbind(coords$XNAD83AC, coords$YNAD83AC)))
dists.inv=1/dists
diag(dists.inv)=0
dists.inv[dists.inv==Inf]=0

w=dists.inv
lw <- mat2listw(w)
lwW <- nb2listw(lw$neighbours, glist=lw$weights, style="W")

#MoranDensity
m.d.data= data[!is.na(Density100m2),]
m.d.dists=as.matrix(dist(cbind(m.d.data$X,m.d.data$Y)))
m.d.dists.inv=1/m.d.dists
diag(m.d.dists.inv)=0
m.d.dists.inv[m.d.dists.inv==Inf]=0

m.d.w=m.d.dists.inv
m.d.lw <- mat2listw(m.d.w)
m.d.lwW <- nb2listw(m.d.lw$neighbours, glist=m.d.lw$weights, style="W")
