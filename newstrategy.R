library(dplyr)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

make_hex_stand <- function(hects=1, minsize=1){
  #scale larger
  x = (1:(100*hects))
  y = (1:(116*hects))
  set.seed(42)
  m <- merge(x,y) |> as.data.frame()
  m <- m |> mutate(x = ifelse(floor(y/2)==y/2, x-0.5,x), y = round(y*(3^0.5)/2,2), wt=1)
  pref=3
  f= pref^1
  mx1 <- merge(x*f,y*f) |> as.data.frame() |> mutate(x = ifelse(floor(y/2)==y/2, x-f*0.5,x), y = round(y*(3^0.5)/2,2), wt=f^2)  |> subset(x <=100*hects & y <= 100*hects)
  f = pref^2
  mx2 <- merge(x*f,y*f) |> as.data.frame() |> mutate(x = ifelse(floor(y/2)==y/2, x-f*0.5,x), y = round(y*(3^0.5)/2,2), wt=f^2)  |> subset(x <=100*hects & y <= 100*hects)
  f = pref^3
  mx3 <- merge(x*f,y*f) |> as.data.frame() |> mutate(x = ifelse(floor(y/2)==y/2, x-f*0.5,x), y = round(y*(3^0.5)/2,2), wt=f^2)  |> subset(x <=100*hects & y <= 100*hects)
  mm <- rbind(m,mx1,mx2,mx3) |> group_by(x,y) |> summarise(wt=max(wt)) |> as.data.frame()
  mm <- mm |> mutate(x = x*minsize, y = y*minsize)
  colnames(mm) <- c('xp','yp','wt')
  rownames(mm) <- 1:nrow(mm) |> as.numeric()
  mm$stumpid <- rownames(mm)
  return(mm)
}

# ggplot()+
#   geom_point(data=stand, aes(x=x,y=z,size=wt), color='white')+
#   geom_point(data=stand1, aes(x=x,y=z), color='pink', size=0.5) +
#   geom_point(data=stumps, aes(x=x,y=z), color='black', size=0.5)+
#   coord_fixed(ratio = 1)





conifer1 <- read.csv('conifer1.csv') |> data.frame(name='conifer1', fill=rgb(0.1,0.5,0.1), color='darkgreen')
conifer2 <- read.csv('conifer2.csv') |> data.frame(name='conifer2', fill='darkgreen', color='darkgreen')
cloud1 <- read.csv('cloud1.csv') |> data.frame(name='cloud1', fill='green', color='darkgreen')
flattop <- read.csv('flattop.csv') |> data.frame(name='flattop', fill='green', color='darkgreen')
flattop2 <- read.csv('flattop2.csv') |> data.frame(name='flattop2', fill='green', color='darkgreen')
blob <- read.csv('blob.csv') |> data.frame(name='blob', fill='green', color='darkgreen')
palm <- read.csv('palm.csv') |> data.frame(name='palm', fill='green', color='darkgreen')|> mutate(x=round(x,2),y=round(y,2))
trunk <- read.csv('trunk.csv') |> data.frame(name='trunk', fill='orange', color='brown')|> mutate(x=round(x,2),y=round(y,2))
triangle <- data.frame(x=c(-0.5, 0, 0.5),y=c(0, 1, 0), name='triangle', fill='darkgreen', color='darkgreen')
dome <- data.frame(x=c(-1,
                       -0.9,
                       -0.75,
                       -0.5,
                       -0.25,
                       0,
                       0.25,
                       0.5,
                       0.75,
                       0.9,
                       1)/2,
                   y=(c(0,
                        0.5,
                        0.7,
                        0.9,
                        0.95,
                        1.00,
                        0.95,
                        0.9,
                        0.7,
                        0.5,
                        0)), name='dome', fill='green', color='darkgreen')
sticks <- data.frame(x=c(-0.075,-0.13,-0.12,-0.025,-0.005,0.005,0.025,0.12,0.13,0.075),y=c(0,1,1,0,1,1,0,1,1,0), name='sticks', fill='orange', color='brown')


grass <- data.frame(x=c(-0.3, -0.5, -0.1,  0.0,  0.1,  0.5,  0.3), y=c(0,0.85,0,1,0,0.85,0), name='grass', fill='green', color='darkgreen')


forb <- data.frame(x=c(-0.04, -0.04, -0.32, -0.38, -0.45, -0.50, -0.45, -0.38, -0.32, -0.04, -0.04, -0.13, -0.12,  0.00,  0.10,  0.12,  0.03,  0.04,  0.29,  0.34,  0.44,  0.50,  0.43,  0.34,  0.30, 0.04, 0.05, -0.04), 
                          y=c(0, 0.39, 0.57, 0.52, 0.52, 0.61, 0.7, 0.7, 0.61, 0.45, 0.78, 0.82, 0.95, 1, 0.95, 0.82, 0.78, 0.45, 0.68, 0.76, 0.77, 0.68, 0.59, 0.59, 0.63, 0.39, 0, 0), name='forb', fill='green', color='darkgreen')
fern <- data.frame(x=c(-0.09, -0.12, -0.16, -0.22, -0.31, -0.50, -0.28, -0.12, -0.15, -0.10, -0.06, -0.06, -0.13, -0.05, -0.01,  0.06,  0.02,  0.00,  0.03, -0.05, -0.04,  0.02,  0.10,  0.09,  0.24, 0.50,  0.27,  0.17,  0.11,  0.03, -0.03, -0.09), 
                          y=c(0, 0.13, 0.2, 0.1, 0.29, 0.41, 0.43, 0.32, 0.24, 0.15, 0.02, 0.38, 0.44, 0.88, 0.96, 1, 0.94, 0.85, 0.42, 0.38, 0.03, 0.16, 0.28, 0.4, 0.45, 0.44, 0.27, 0.12, 0.25, 0.13, 0, 0), name='fern', fill='green', color='darkgreen')

shapes <- rbind(conifer1,conifer2,cloud1,flattop,flattop2,blob,trunk,triangle,dome,sticks,grass,forb,fern)
colnames(shapes) <- c("x","z","shape","fill","color")
# ggplot() +geom_polygon(data=shapes, aes(x=x,y=z,  group = shape, fill=fill, color=color), alpha=0.8)


ht.max = 5
ht.min = 2
crwd = 2
dbh = 30

colormixer <- function(colorname, mixcolor, p){
  ccc <- col2rgb(colorname)
  ccc <- data.frame(r = ccc[1,],   g = ccc[2,],   b = ccc[3,])
  mmm <- col2rgb(mixcolor)
  new <- ccc |> mutate(r = r*(1-p)+mmm[1,1]*p,
                       g = g*(1-p)+mmm[2,1]*p,
                       b = b*(1-p)+mmm[3,1]*p)
  new <- rgb(new$r/255,new$g/255,new$b/255)
  return(new)
}

make_tree <- function(ht.max, ht.min,crwd,dbh, crshape, stshape){
  crown <- subset(shapes, shape %in% crshape) |> mutate(x=x*crwd, z=z*(ht.max-ht.min)+ht.min, obj='crown')
  base <- subset(shapes, shape %in% stshape) |> mutate(x=x*dbh/100*1.1, z=z*(ht.min), obj='stem') 
  tree = rbind(crown, base)
  tree$ptord <- rownames(tree) |> as.numeric()
  return(tree)}
make_shrub <- function(ht.max, ht.min,crwd, crshape, stshape){
  crown <- subset(shapes, shape %in% crshape)  |> mutate(x=x*crwd, z=z*(ht.max-ht.min)+ht.min, obj='crown')
  base <- subset(shapes, shape %in% stshape) |> mutate(x=x*crwd*0.8, z=z*(ht.min), obj='stem') 
  shrub = rbind(crown, base)
  shrub$ptord <- rownames(shrub) |> as.numeric()
  return(shrub)}

tree <- make_tree(ht.max=20,ht.min=7.5,crwd = 5,dbh = 30, crshape='blob', stshape='trunk') 
tree2 <- make_tree(ht.max=30,ht.min=7.5,crwd = 5,dbh = 30, crshape='conifer1', stshape='trunk')  
shrub <- make_shrub(ht.max=3,ht.min=1,crwd=2, crshape='cloud1', stshape='sticks')  


stand <- make_hex_stand(0.5,1) |> subset(yp >= 15 & yp < 45) |> mutate(wtn = wt, stratid = NA)
strats <- data.frame(stratid = c(1:3), stems = c(20,20,20))
for (i in 1:nrow(strats)){#i=1
  thistrat = strats$stratid[i]
  nstems = strats$stems[i]
  newstumps <- sample(stand$stumpid, size = nstems, prob = stand$wtn)
  stand <- stand |> mutate(wtn = ifelse(stand$stumpid %in% newstumps, 0, wtn), 
                           stratid = ifelse(stand$stumpid %in% newstumps, thistrat,stratid))
}
stumps1 <- stand |> subset(stratid %in% 1)
stumps2 <- stand |> subset(stratid %in% 2)
stumps3 <- stand |> subset(stratid %in% 3)

trees0 <- merge(stumps1, tree) |> mutate(objid = paste0(obj,stumpid))
trees1 <- merge(stumps2, tree2)  |> mutate(objid = paste0(obj,stumpid))
shrubs <- merge(stumps3, shrub)  |> mutate(objid = paste0(obj,stumpid))
plants <- rbind(trees0, trees1, shrubs)

#randomize sizes and positions
plants <- plants |> group_by(stumpid) |> 
  mutate(ht.max = max(z), crwd = max(x)-min(x),
         xpp = xp + runif(1, min = -0.5, max = 0.5),#shift position on grid
         zr = rnorm(1,ht.max, ht.max/10)/ht.max,#deviation in height
         xr = (rnorm(1,ht.max, ht.max/10)/ht.max+rnorm(1,crwd, crwd/10)/crwd)/2,#deviation in width partially related to height
         xn = x*xr+xpp,#resized width and put on new position
         zn = z*zr*(1-1/15))#resized height adjusted downward show that variation is less than max height in the field

#rearrange stems depth drawing order
plants <- plants |> arrange(yp,stumpid, objid, ptord)
ypmax <- max(plants$yp)
ypmin <- min(plants$yp)
ypwid <- ypmax-ypmin
crowns1 <- plants |> subset(yp < ypmin+ypwid/3 & obj %in% 'crown') |> 
  mutate(fill=colormixer(fill, "#D9F2FF", 0.7), color=colormixer(color, "#D9F2FF", 0.7))
crowns2 <- plants |> subset(yp < ypmax-ypwid/3 & yp >= ypmin+ypwid/3 & obj %in% 'crown')|> 
  mutate(fill=colormixer(fill, "#D9F2FF", 0.3), color=colormixer(color, "#D9F2FF", 0.3))
crowns3 <- plants |> subset(yp >= ypmax-ypwid/3 & obj %in% 'crown')
stems1 <- plants |> subset(yp < ypmin+ypwid/3 & obj %in% 'stem')|> 
  mutate(fill=colormixer(fill, "#D9F2FF", 0.7), color=colormixer(color, "#D9F2FF", 0.7))
stems2 <- plants |> subset(yp < ypmax-ypwid/3 & yp >= ypmin+ypwid/3 & obj %in% 'stem')|> 
  mutate(fill=colormixer(fill, "#D9F2FF", 0.3), color=colormixer(color, "#D9F2FF", 0.3))
stems3 <- plants |> subset(yp >= ypmax-ypwid/3 & obj %in% 'stem')

plants2 <- rbind(crowns1,crowns2,crowns3,stems1,stems2,stems3)

# ggplot()+
#   geom_point(aes(x=1,y=1))+  
#   theme(panel.background = element_rect(fill = rgb(0.4,0.6,0.5)))

pcolor <- plants2$color |> unique() |> sort()
pfill <- plants2$fill |> unique()|> sort()



ggplot() +
  geom_polygon(data=stems1, aes(x=xn,y=zn,group=objid, fill=fill, color=color), alpha=1, linewidth=0.01)+
  geom_polygon(data=crowns1, aes(x=xn,y=zn,group=objid, fill=fill, color=color), alpha=1, linewidth=0.01)+
  geom_polygon(data=stems2, aes(x=xn,y=zn,group=objid, fill=fill, color=color), alpha=1, linewidth=0.01)+
  geom_polygon(data=crowns2, aes(x=xn,y=zn,group=objid, fill=fill, color=color), alpha=1, linewidth=0.01)+
  geom_polygon(data=stems3, aes(x=xn,y=zn,group=objid, fill=fill, color=color), alpha=1, linewidth=0.01)+
  geom_polygon(data=crowns3, aes(x=xn,y=zn,group=objid, fill=fill, color=color), alpha=1, linewidth=0.01)+
  scale_fill_manual(values=pfill)+
  scale_color_manual(values=pcolor)+
  theme(legend.position = "none", 
        panel.background = element_rect(fill = rgb(0.85,0.95,1,0.5),
                                        colour = "black",
                                        linewidth = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                        colour = rgb(0.1, 0.1, 0.1, 0.3)), 
        panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid',
                                        colour = rgb(0.1, 0.1, 0.1, 0.1))
  )+
  coord_fixed(ratio = 1)+
  scale_y_continuous(trans='identity', breaks = c(-10:(120/5))*5,minor_breaks = c(-10:(120)), limits = c(0,50))+
  scale_x_continuous(breaks = c(-10:(120/5))*5, minor_breaks = c(-10:(120)), limits = c(-5,55))

