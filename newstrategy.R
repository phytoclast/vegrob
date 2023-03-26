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
  colnames(mm) <- c('x','z','wt')
  rownames(mm) <- 1:nrow(mm) |> as.numeric()
  return(mm)
}

stand <- make_hex_stand(0.5,1)
stand1 <- subset(stand, z >= 15 & z < 45)
stumps <- stand1[sample(row.names(stand1),size = 50,prob = stand1$wt),]

ggplot()+
  geom_point(data=stand, aes(x=x,y=z,size=wt), color='white')+
  geom_point(data=stand1, aes(x=x,y=z), color='pink', size=0.5) +
  geom_point(data=stumps, aes(x=x,y=z), color='black', size=0.5)+
  coord_fixed(ratio = 1)





conifer1 <- read.csv('conifer1.csv') |> data.frame(name='conifer1', fill='darkgreen', color='darkgreen')
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


ggplot() +
  geom_polygon(data=trunk, aes(x,y,  fill=name, color=name), alpha=0.8)

ht.max = 5
ht.min = 2
crwd = 2
dbh = 30

make_B_tree <- function(ht.max, ht.min,crwd,dbh){
  crown <- blob |> mutate(x=x*crwd, y=y*(ht.max-ht.min)+ht.min)
  base <- trunk |> mutate(x=x*dbh/100*1.1, y=y*(ht.min)) 
  tree = rbind(crown, base)
  tree$ptord <- rownames(tree) |> as.numeric()
  return(tree)}
make_B_tree2 <- function(ht.max, ht.min,crwd,dbh){
  crown <- flattop2 |> mutate(x=x*crwd, y=y*(ht.max-ht.min)+ht.min)
  base <- trunk |> mutate(x=x*dbh/100*1.1, y=y*(ht.min)) 
  tree = rbind(crown, base)
  tree$ptord <- rownames(tree) |> as.numeric()
  return(tree)}
make_shrub <- function(ht.max, ht.min,crwd){
  crown <- cloud1 |> mutate(x=x*crwd, y=y*(ht.max-ht.min)+ht.min) #|> data.frame(id=1)
  base <- sticks |> mutate(x=x*crwd*0.8, y=y*(ht.min)) #|> data.frame(id=2) 
  tree = rbind(crown, base)
  tree$ptord <- rownames(tree) |> as.numeric()
  return(tree)}
make_palm <- function(ht.max, ht.min,crwd,dbh){
  crown <- palm |> mutate(x=x*crwd, y=y*(ht.max-ht.min)+ht.min) #|> data.frame(id=1)
  base <- trunk |> mutate(x=x*dbh/100*1.1, y=y*(ht.min)) #|> data.frame(id=2) 
  tree = rbind(crown, base)
  tree$ptord <- rownames(tree) |> as.numeric()
  return(tree)}
make_N_tree <- function(ht.max, ht.min,crwd,dbh){
  crown <- conifer1 |> mutate(x=x*crwd, y=y*(ht.max-ht.min)+ht.min) #|> data.frame(id=1)
  base <- trunk |> mutate(x=x*dbh/100*1.1, y=y*(ht.min)) #|> data.frame(id=2) 
  tree = rbind(crown, base)
  tree$ptord <- rownames(tree) |> as.numeric()
  return(tree)}
make_N_tree2 <- function(ht.max, ht.min,crwd,dbh){
  crown <- conifer2 |> mutate(x=x*crwd, y=y*(ht.max-ht.min)+ht.min) #|> data.frame(id=1)
  base <- trunk |> mutate(x=x*dbh/100*1.1, y=y*(ht.min)) #|> data.frame(id=2) 
  tree = rbind(crown, base)
  tree$ptord <- rownames(tree) |> as.numeric()
  return(tree)}

tree <- make_B_tree(ht.max=20,ht.min=7.5,crwd = 5,dbh = 30) 
tree2 <- make_N_tree2(ht.max=30,ht.min=7.5,crwd = 5,dbh = 30) 
shrub <- make_shrub(ht.max=3,ht.min=1,crwd=2) 

sample0 <- sample(row.names(stand1), size = 5, prob = stand1$wt)
stand2 <- stand1 |> mutate(wt = ifelse(rownames(stand1) %in% sample0, 0,wt))
sample1 <- sample(row.names(stand2), size = 20, prob = stand2$wt)
stand2 <- stand1 |> mutate(wt = ifelse(rownames(stand1) %in% sample1, 0,wt))
sample2 <- sample(row.names(stand2), size = 20, prob = stand2$wt)
stumps0 <- stand1[sample0,]
stumps1 <- stand1[sample1,]
stumps2 <- stand1[sample2,]
stumps0a <- data.frame(id=rownames(stumps0), xp=stumps0$x, z=stumps0$z)
stumps1a <- data.frame(id=rownames(stumps1), xp=stumps1$x, z=stumps1$z)
stumps2a <- data.frame(id=rownames(stumps2), xp=stumps2$x, z=stumps2$z)


trees0 <- merge(stumps0a, tree2) 
trees1 <- merge(stumps1a, tree) 
shrubs <- merge(stumps2a, shrub) 
plants <- rbind(trees0, trees1, shrubs)

#randomize sizes and positions
plants <- plants |> group_by(id) |> 
  mutate(ht.max = max(y), crwd = max(x)-min(x),
         xpp = xp + runif(1, min = -0.5, max = 0.5),#shift position on grid
         yr = rnorm(1,ht.max, ht.max/10)/ht.max,#deviation in height
         xr = (rnorm(1,ht.max, ht.max/10)/ht.max+rnorm(1,crwd, crwd/10)/crwd)/2,#deviation in width partially related to height
         xn = x*xr+xpp,#resized width and put on new position
         yn = y*yr*(1-1/15))#resized height adjusted downward show that variation is less than max height in the field

#rearrange stems depth drawing order
plants <- plants |> arrange(id, name, ptord, z)
zmax <- max(plants$z)
zmin <- min(plants$z)
zwid <- zmax-zmin
plants1 <- plants |> subset(z < zmin+zwid/3)
plants2 <- plants |> subset(z < zmax-zwid/3 & z >= zmin+zwid/3)
plants3 <- plants |> subset(z >= zmax-zwid/3)

ggplot() +
  geom_polygon(data=plants1, aes(x=xn,y=yn,group=paste0(name,id), fill=fill, color=color), alpha=0.9, size=0.01)+
  geom_polygon(data=plants2, aes(x=xn,y=yn,group=paste0(name,id), fill=fill, color=color), alpha=0.9, size=0.01)+
  geom_polygon(data=plants3, aes(x=xn,y=yn,group=paste0(name,id), fill=fill, color=color), alpha=0.9, size=0.01)+
  scale_fill_manual(values=c('darkgreen','green','orange'))+
  scale_color_manual(values=c('brown','darkgreen'))+
  theme(legend.position = "none", 
        panel.background = element_rect(fill = rgb(0.85,0.95,1,0.5),
                                        colour = "black",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = rgb(0.1, 0.1, 0.1, 0.3)), 
        panel.grid.minor = element_line(size = 0.1, linetype = 'solid',
                                        colour = rgb(0.1, 0.1, 0.1, 0.1))
  )+
  coord_fixed(ratio = 1)+
  scale_y_continuous(trans='identity', breaks = c(-10:(120/5))*5,minor_breaks = c(-10:(120)), limits = c(0,50))+
  scale_x_continuous(breaks = c(-10:(120/5))*5, minor_breaks = c(-10:(120)), limits = c(-5,55))

