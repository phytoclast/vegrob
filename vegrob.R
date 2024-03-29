setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# remotes::install_github('https://github.com/pmur002/gggrid')
# install.package('remotes')
remotes::install_github('coolbutuseless/cssparser') # Handles CSS styling
remotes::install_github('coolbutuseless/svgparser')
library(grid)
library(gggrid)
library(svgparser)
#https://cran.r-project.org/web/packages/ggpp/vignettes/grammar-extensions.html#geom_grob
library(grid)
library(ggpmisc)
library(dplyr)




ntrees <-  function(totalcover,crown.width, transect.length){
  numberoftrees = log(1-totalcover)/log(1-crown.width/transect.length)
return(numberoftrees)}

make.tree <- function(tree.height, branch.height, crown.width, diameter,form.choice,scale.exponent){
  #form.choice='E'
  forms = c('E','D','Ds','B','Bs','N','G','F1','F2','EX','Dx','Dsx','GX','FX')
  f.color = c('darkgreen','green','green','darkgreen','darkgreen','yellow','yellow','green','darkgreen','gray','gray','gray','gray','gray')
  use.color = f.color[which(forms == form.choice)]
  scxp <- scale.exponent#scxp=1
  tree.ht=tree.height/tree.height
  crn.ht=branch.height/tree.height
  crn.wd=crown.width/tree.height
  tree.wd=diameter/100/tree.height
  
  
  g.E.crown <- polygonGrob(x=c(0.5-crn.wd/2, 0.5, 0.5+crn.wd/2), 
                                 y=c(crn.ht, tree.ht, crn.ht)^scxp, gp=gpar(fill=use.color,alpha=0.5))
  
  g.D.crown <- polygonGrob(x=c(0.5-crn.wd/2,
                                   0.5-crn.wd/2,
                                   0.5-0.75*crn.wd/2,
                                   0.5-0.5*crn.wd/2,
                                   0.5-0.25*crn.wd/2,
                                   0.5,
                                   0.5+0.25*crn.wd/2,
                                   0.5+0.5*crn.wd/2,
                                   0.5+0.75*crn.wd/2,
                                   0.5+crn.wd/2,
                                   0.5+crn.wd/2), 
                               y=c(crn.ht,
                                   (crn.ht*0.33+tree.ht*0.67),
                                   (crn.ht*0.15+tree.ht*0.85),
                                   (crn.ht*0.05+tree.ht*0.95),
                                   (crn.ht*0.02+tree.ht*0.98),
                                   tree.ht, 
                                   (crn.ht*0.02+tree.ht*0.98),
                                   (crn.ht*0.05+tree.ht*0.95),
                                   (crn.ht*0.15+tree.ht*0.85),
                                   (crn.ht*0.33+tree.ht*0.67),
                                   crn.ht)^scxp, gp=gpar(fill=use.color,alpha=0.5))

  g.Ds.crown <- polygonGrob(x=c(-1,
                               -0.9,
                               -0.75,
                               -0.5,
                               -0.25,
                               0,
                               0.25,
                               0.5,
                               0.75,
                               0.9,
                               1)/2+0.5,
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
                                0)*.67+0.33)^scxp, gp=gpar(fill=use.color,alpha=0.5))
  
    

  g.trunk <- rectGrob(x=0.5, y=((crn.ht-0)^scxp)/2, width=tree.wd, height=(crn.ht-0)^scxp, gp=gpar(fill='brown'))
  
  g.stems <- polygonGrob(x=c(-0.075,-0.13,-0.12,-0.025,-0.005,0.005,0.025,0.12,0.13,0.075)+0.5, 
                                  y=(c(0,1,1,0,1,1,0,1,1,0)*0.33)^scxp, gp=gpar(fill='brown'))
    
  
  g.G.crown <- polygonGrob(x=c(0.425,0.375,0.475,0.5,0.525,0.625,0.575), 
                           y=c(0,0.85,0,1,0,0.85,0)^scxp, gp=gpar(fill=use.color,alpha=0.5))
  
  g.F1.crown <- polygonGrob(x=c(0.46, 0.46, 0.18, 0.12, 0.05, 0, 0.05, 0.12, 0.18, 0.46, 0.46, 0.37, 0.38, 0.5, 0.6, 0.62, 0.53, 0.54, 0.79, 0.84, 0.94, 1, 0.93, 0.84, 0.8, 0.54, 0.55, 0.46), 
                            y=c(0, 0.39, 0.57, 0.52, 0.52, 0.61, 0.7, 0.7, 0.61, 0.45, 0.78, 0.82, 0.95, 1, 0.95, 0.82, 0.78, 0.45, 0.68, 0.76, 0.77, 0.68, 0.59, 0.59, 0.63, 0.39, 0, 0)^scxp, gp=gpar(fill=use.color,alpha=0.5))
  g.F2.crown <- polygonGrob(x=c(0.41, 0.38, 0.34, 0.28, 0.19, 0, 0.22, 0.38, 0.35, 0.4, 0.44, 0.44, 0.37, 0.45, 0.49, 0.56, 0.52, 0.5, 0.53, 0.45, 0.46, 0.52, 0.6, 0.59, 0.74, 1, 0.77, 0.67, 0.61, 0.53, 0.47, 0.41), 
                            y=c(0, 0.13, 0.2, 0.1, 0.29, 0.41, 0.43, 0.32, 0.24, 0.15, 0.02, 0.38, 0.44, 0.88, 0.96, 1, 0.94, 0.85, 0.42, 0.38, 0.03, 0.16, 0.28, 0.4, 0.45, 0.44, 0.27, 0.12, 0.25, 0.13, 0, 0)^scxp, gp=gpar(fill=use.color,alpha=0.5))
  
  if(form.choice %in% c('E','N')){
    g.tree = grobTree(g.E.crown, g.trunk)
  }
  if(form.choice %in% c('D','B')){
    g.tree = grobTree(g.D.crown, g.trunk)
  }
  if(form.choice %in% c('Ds','Bs')){
    g.tree = grobTree(g.Ds.crown, g.stems)
  }
  if(form.choice %in% c('G','GX')){
    g.tree = grobTree(g.G.crown)
  }
  if(form.choice %in% c('F1','FX')){
    g.tree = grobTree(g.F1.crown)
  }
  if(form.choice %in% c('F2')){
    g.tree = grobTree(g.F2.crown)
  }
  
   return(g.tree) 

}

make.stratum <- function(veg.grob, tree.height, branch.height, crown.width, diameter, scale.exponent){
  n=round(ntrees(total.cover, crown.width, 50),0)
  x = sample(0:200, n, replace = T)/4
  rando =  rnorm(n)
  height = rando*0.05*tree.height+tree.height-0.05/2*tree.height
  width = rando*0.05*crown.width+crown.width
  y = height
  
  grobs.tb <- tibble(x = x, y = (y^scale.exponent)/2,
                     width = width/50,
                     height =  (height/50)^scale.exponent,
                     grob = list(veg.grob))
  return(grobs.tb)
}

get.crown.diam.ratio <- function(cover, dbh, ba, baf=10){
  #cover = aggregate overstory cover
  #diam = stand quadratic mean diameter
  #ba = stand basal area count
  #baf = basal area factor
  if(baf %in% c(5,10,20,40)){#legacy basal area units
    fba = baf*ba
    actoha = 43560*0.3048^2/10000
    ft2tom2 = 0.3048^2
    mba = fba*ft2tom2/actoha
  }
  if(is.na(baf)|baf==2){#metric basal area units
    mba = ba*baf
  }
  m2pertree = 3.141592*(dbh/200)^2
  treesperha = mba/m2pertree
  coverpertreeperha = 1-(1-cover)^(1/treesperha)
  crown.width = 2*(coverpertreeperha*10000/3.141592)^0.5
  crown.width
  crown.stem.ratio = crown.width/dbh*100
  return(crown.stem.ratio)#crown stem ratio meter per meter
}

get.crown.diam.ratio(0.8,15,10)
get.crown.diam.ratio(cover=0.8,dbh=15,ba=5,baf=10)

scale.exponent = 0.5 
#strat0 ----
tree.height=45
branch.height=25
crown.width=20
diameter=120
form.choice = 'E'

total.cover = 0.7

tree0 <- make.tree(tree.height, branch.height, crown.width, diameter,form.choice,scale.exponent)
grid.newpage()
grid.draw(tree0)
stratum0 <- make.stratum(tree0, tree.height, branch.height, crown.width, diameter, scale.exponent)
#strat1 ----
tree.height=30
branch.height=10
crown.width=20
diameter=35
form.choice = 'D'

total.cover = 0.95
breaks1 = ifelse(scale.exponent <=0.7,(c(0:4,(1:25)*5))^scale.exponent, c(1:25)*5)

tree1 <- make.tree(tree.height, branch.height, crown.width, diameter,form.choice,scale.exponent)
grid.newpage()
grid.draw(tree1)
stratum1 <- make.stratum(tree1, tree.height, branch.height, crown.width, diameter, scale.exponent)


#stat2 ----
tree.height=5
branch.height=1
crown.width=tree.height
diameter=1
form.choice = 'Ds'

total.cover = 0.20

tree2 <- make.tree(tree.height, branch.height, crown.width, diameter,form.choice,scale.exponent)
stratum2 <- make.stratum(tree2, tree.height, branch.height, crown.width, diameter, scale.exponent)
#stat3 ----
tree.height=1.2
branch.height=1
crown.width=tree.height
diameter=1
form.choice = 'F1'

total.cover = 0.20

tree3 <- make.tree(tree.height, branch.height, crown.width, diameter,form.choice,scale.exponent)
stratum3 <- make.stratum(tree3, tree.height, branch.height, crown.width, diameter, scale.exponent)
#stat4 ----
tree.height=0.7
branch.height=1
crown.width=tree.height
diameter=1
form.choice = 'G'

total.cover = 0.8

tree4 <- make.tree(tree.height, branch.height, crown.width, diameter,form.choice,scale.exponent)

stratum4 <- make.stratum(tree4, tree.height, branch.height, crown.width, diameter, scale.exponent)

all.strata <- rbind(stratum0,stratum1,stratum2,stratum3,stratum4)

all.strata <- all.strata[sample(1:nrow(all.strata),133),]


if(scale.exponent <=0.7){
  labels1=c(0:4,(1:25)*5)
  breaks1=(c(0:4,(1:25)*5))^scale.exponent
}else{
  labels1=c(1:25)*5
  breaks1=(c(1:25)*5)^scale.exponent
}

ggplot() +
  geom_grob(data = all.strata, 
            aes(x, y, label = grob, vp.width = width, vp.height = height)
  ) +
  scale_y_continuous(name='canopy height (m)',breaks=breaks1,labels=labels1) +
  scale_x_continuous(breaks=c(0:25)*5) +
  coord_fixed(ratio = 50/50^scale.exponent, ylim = c(0,50^scale.exponent), xlim = c(0,50), expand = F)+
  theme_bw(12)








convert.SVG <- function(filename){
  
  blob <- svgparser::read_svg(filename, obj_type = 'data.frame')
  
  blob$x <- (blob$x - min(blob$x)) / (max(blob$x) - min(blob$x)) 
  blob$y <- 1-1*(blob$y - min(blob$y)) / (max(blob$y) - min(blob$y)) 
   return(blob)
  
}



blob <- convert.SVG('blob.svg')
trunk <- convert.SVG('trunk.svg')
palm <- convert.SVG('palm.svg')

g.D.crown <- polygonGrob(x=(blob$x*crn.wd)+crn.wd/2, 
                         y=((blob$y*(1-crn.ht))+crn.ht)^scxp, gp=gpar(fill=use.color,alpha=0.5))
g.trunk <- polygonGrob(x=trunk$x*tree.wd/0.8+crn.wd, 
                       y=(trunk$y*crn.ht)^0.5, gp=gpar(fill='brown'))
g.tree <- grobTree(g.D.crown, g.trunk)
grid.newpage();grid.draw(g.tree)






make.tree <- function(tree.height, branch.height, crown.width, diameter,form.choice,scale.exponent){
  #form.choice='E'
  forms = c('E','D','Ds','B','Bs','N','G','F1','F2','EX','Dx','Dsx','GX','FX')
  f.color = c('darkgreen','green','green','darkgreen','darkgreen','yellow','yellow','green','darkgreen','gray','gray','gray','gray','gray')
  use.color = f.color[which(forms == form.choice)]
  scxp <- scale.exponent#scxp=1
  tree.ht=tree.height/tree.height
  crn.ht=branch.height/tree.height
  crn.wd=crown.width/tree.height
  tree.wd=diameter/100/tree.height
  
  
  g.E.crown <- polygonGrob(x=c(0.5-crn.wd/2, 0.5, 0.5+crn.wd/2), 
                           y=c(crn.ht, tree.ht, crn.ht)^scxp, gp=gpar(fill=use.color,alpha=0.5))
  
  g.D.crown <- polygonGrob(x=(blob$x*crn.wd)+crn.wd/2, 
                           y=((blob$y*(1-crn.ht))+crn.ht)^scxp, gp=gpar(fill=use.color,alpha=0.5))
  
  g.Ds.crown <- polygonGrob(x=c(-1,
                                -0.9,
                                -0.75,
                                -0.5,
                                -0.25,
                                0,
                                0.25,
                                0.5,
                                0.75,
                                0.9,
                                1)/2+0.5,
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
                                 0)*.67+0.33)^scxp, gp=gpar(fill=use.color,alpha=0.5))
  
  g.trunk <- polygonGrob(x=trunk$x*tree.wd/0.8+crn.wd, 
                         y=(trunk$y*crn.ht)^scxp, gp=gpar(fill='brown'))
  
  g.stems <- polygonGrob(x=c(-0.075,-0.13,-0.12,-0.025,-0.005,0.005,0.025,0.12,0.13,0.075)+0.5, 
                         y=(c(0,1,1,0,1,1,0,1,1,0)*0.33)^scxp, gp=gpar(fill='brown'))
  
  
  g.G.crown <- polygonGrob(x=c(0.425,0.375,0.475,0.5,0.525,0.625,0.575), 
                           y=c(0,0.85,0,1,0,0.85,0)^scxp, gp=gpar(fill=use.color,alpha=0.5))
  
  g.F1.crown <- polygonGrob(x=c(0.46, 0.46, 0.18, 0.12, 0.05, 0, 0.05, 0.12, 0.18, 0.46, 0.46, 0.37, 0.38, 0.5, 0.6, 0.62, 0.53, 0.54, 0.79, 0.84, 0.94, 1, 0.93, 0.84, 0.8, 0.54, 0.55, 0.46), 
                            y=c(0, 0.39, 0.57, 0.52, 0.52, 0.61, 0.7, 0.7, 0.61, 0.45, 0.78, 0.82, 0.95, 1, 0.95, 0.82, 0.78, 0.45, 0.68, 0.76, 0.77, 0.68, 0.59, 0.59, 0.63, 0.39, 0, 0)^scxp, gp=gpar(fill=use.color,alpha=0.5))
  g.F2.crown <- polygonGrob(x=c(0.41, 0.38, 0.34, 0.28, 0.19, 0, 0.22, 0.38, 0.35, 0.4, 0.44, 0.44, 0.37, 0.45, 0.49, 0.56, 0.52, 0.5, 0.53, 0.45, 0.46, 0.52, 0.6, 0.59, 0.74, 1, 0.77, 0.67, 0.61, 0.53, 0.47, 0.41), 
                            y=c(0, 0.13, 0.2, 0.1, 0.29, 0.41, 0.43, 0.32, 0.24, 0.15, 0.02, 0.38, 0.44, 0.88, 0.96, 1, 0.94, 0.85, 0.42, 0.38, 0.03, 0.16, 0.28, 0.4, 0.45, 0.44, 0.27, 0.12, 0.25, 0.13, 0, 0)^scxp, gp=gpar(fill=use.color,alpha=0.5))
  
  if(form.choice %in% c('E','N')){
    g.tree = grobTree(g.E.crown, g.trunk)
  }
  if(form.choice %in% c('D','B')){
    g.tree = grobTree(g.D.crown, g.trunk)
  }
  if(form.choice %in% c('Ds','Bs')){
    g.tree = grobTree(g.Ds.crown, g.stems)
  }
  if(form.choice %in% c('G','GX')){
    g.tree = grobTree(g.G.crown)
  }
  if(form.choice %in% c('F1','FX')){
    g.tree = grobTree(g.F1.crown)
  }
  if(form.choice %in% c('F2')){
    g.tree = grobTree(g.F2.crown)
  }
  
  return(g.tree) 
  
}





