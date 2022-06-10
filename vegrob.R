# remotes::install_github('https://github.com/pmur002/gggrid')
library(grid)
library(gggrid)
library(svgparser)

grid.newpage()
tree.ht = 0.75
crn.ht = .25
crn.wd = 0.25
tree.wd = 0.025
# maketree <- function(treedist)
g.conifer.crown <- polygonGrob(x=c(0.5-crn.wd/2, 0.5, 0.5+crn.wd/2), 
                               y=c(crn.ht, tree.ht, crn.ht), gp=gpar(fill='darkgreen'))
g.crown <- roundrectGrob(x=0.5, y=(tree.ht+crn.ht)/2, width=crn.wd, height=(tree.ht-crn.ht), gp=gpar(fill='green'))
g.trunk <- rectGrob(x=0.5, y=(crn.ht-0)/2, width=tree.wd, height=(crn.ht-0), gp=gpar(fill='brown'))
tree <- grobTree(g.crown, g.trunk)
tree2 <- grobTree(g.conifer.crown, g.trunk)

treedist <- data.frame(cbind(x=c(0,1,5,4),y=c(0,.5,3,0)))
grid.draw(tree2)






maketree <- function(treedist){
  g.crown <- roundrectGrob(x=treedist$x, y=(tree.ht+crn.ht)/2+treedist$y, width=crn.wd, height=(tree.ht-crn.ht), gp=gpar(fill='green'))
  g.trunk <- rectGrob(x=treedist$x, y=(crn.ht-0)/2+treedist$x, width=tree.wd, height=(crn.ht-0), gp=gpar(fill='brown'))
  tree <- grobTree(g.crown, g.trunk)
  return(tree)
}

g.crown <- roundrectGrob(x=unit(0.5,'npc'), y=unit((tree.ht+crn.ht)/2,'npc'), 
                         width=unit(crn.wd,'npc'), height=unit(tree.ht-crn.ht,'npc'), gp=gpar(fill='green'))
g.trunk <- rectGrob(x=unit(0.5,'npc'), y=unit((crn.ht-0)/2,'npc'), 
                    width=unit(tree.wd,'npc'), height=unit(crn.ht-0,'npc'), gp=gpar(fill='brown'))


vp <- viewport(x=unit(0/5,'npc'), y=unit(.5/5,'npc'), width=unit(5,'cm'), height=unit(5,'cm'))
tree <- grobTree(g.crown, g.trunk)

ggplot()+
  geom_point(aes(x=x,y=y), data = treedist)+
  coord_cartesian(xlim = c(0,5),ylim = c(0,5)) +
  grid_panel(tree)
#https://cran.r-project.org/web/packages/ggpp/vignettes/grammar-extensions.html#geom_grob
library(grid)
library(ggpmisc)
library(dplyr)

tree.ht = 1
crn.ht = .25
crn.wd = .5
tree.wd = 0.025

g.crown <- roundrectGrob(x=unit(0.5,'npc'), y=unit((tree.ht+crn.ht)/2,'npc'), 
                         width=unit(crn.wd,'npc'), height=unit(tree.ht-crn.ht,'npc'), gp=gpar(fill='green',alpha=0.5))

g.trunk <- rectGrob(x=unit(0.5,'npc'), y=unit((crn.ht-0)/2,'npc'), 
                    width=unit(tree.wd,'npc'), height=unit(crn.ht-0,'npc'), gp=gpar(fill='brown'))

tree <- grobTree(g.crown, g.trunk)

g.conifer.crown <- polygonGrob(x=c(0.5-crn.wd/2, 0.5, 0.5+crn.wd/2), 
                               y=c(crn.ht, tree.ht, crn.ht), gp=gpar(fill='darkgreen',alpha=0.5))

g.trunk <- rectGrob(x=0.5, y=(crn.ht-0)/2, width=tree.wd, height=(crn.ht-0), gp=gpar(fill='brown'))

tree2 <- grobTree(g.conifer.crown, g.trunk)

g.crown.round <- polygonGrob(x=c(0.5-crn.wd/2,
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
                                 crn.ht), gp=gpar(fill='green',alpha=0.5))


tree3 <- grobTree(g.crown.round, g.trunk)

sample(1:50, 10)
c=0.19
p=.1
n=2
c = 1-(1-p)^n
1-c = (1-p)^n
log(1-c) = n*log(1-p)

ntrees <-  function(totalcover,crown.width, transect.length){
  numberoftrees = log(1-totalcover)/log(1-crown.width/transect.length)
return(numberoftrees)}

maketree1 <- function(tree.height, branch.height, crown.width, diameter,form.choice,scale.exponent){
  

  #form.choice='E'
  forms = c('E','D','B','N','G','F1','F2','EX','Dx','GX','FX')
  f.color = c('darkgreen','green','darkgreen','yellow','yellow','green','darkgreen','gray','gray','gray','gray')
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

  g.trunk <- rectGrob(x=0.5, y=(crn.ht-0)/2, width=tree.wd, height=(crn.ht-0)^scxp, gp=gpar(fill='brown'))
  return(grobTree(g.crown.round, g.trunk))
  
  g.G.crown <- polygonGrob(x=c(0.425,0.375,0.475,0.5,0.525,0.625,0.575), 
                           y=c(0,0.85,0,1,0,0.85,0)^scxp, gp=gpar(fill='yellow',alpha=0.5))
  
  g.F1.crown <- polygonGrob(x=c(0.46, 0.46, 0.18, 0.12, 0.05, 0, 0.05, 0.12, 0.18, 0.46, 0.46, 0.37, 0.38, 0.5, 0.6, 0.62, 0.53, 0.54, 0.79, 0.84, 0.94, 1, 0.93, 0.84, 0.8, 0.54, 0.55, 0.46), 
                            y=c(0, 0.39, 0.57, 0.52, 0.52, 0.61, 0.7, 0.7, 0.61, 0.45, 0.78, 0.82, 0.95, 1, 0.95, 0.82, 0.78, 0.45, 0.68, 0.76, 0.77, 0.68, 0.59, 0.59, 0.63, 0.39, 0, 0)^scxp, gp=gpar(fill=use.color,alpha=0.5))
  g.F2.crown <- polygonGrob(x=c(0.41, 0.38, 0.34, 0.28, 0.19, 0, 0.22, 0.38, 0.35, 0.4, 0.44, 0.44, 0.37, 0.45, 0.49, 0.56, 0.52, 0.5, 0.53, 0.45, 0.46, 0.52, 0.6, 0.59, 0.74, 1, 0.77, 0.67, 0.61, 0.53, 0.47, 0.41), 
                            y=c(0, 0.13, 0.2, 0.1, 0.29, 0.41, 0.43, 0.32, 0.24, 0.15, 0.02, 0.38, 0.44, 0.88, 0.96, 1, 0.94, 0.85, 0.42, 0.38, 0.03, 0.16, 0.28, 0.4, 0.45, 0.44, 0.27, 0.12, 0.25, 0.13, 0, 0)^scxp, gp=gpar(fill=use.color,alpha=0.5))
  grid.newpage()
  grid.draw(g.F2.crown)
  
  c(0.425,0.375,0.475,0.5,0.525,0.625,0.575)-.5
}

tree.height=30
branch.height=20
crown.width=15
diameter=35
total.cover = 0.99

tree1 <- maketree1(tree.height, branch.height, crown.width, diameter)
grid.newpage()
grid.draw(tree1)

n=round(ntrees(total.cover, crown.width, 50),0)

x = sample(0:50, n)
rando =  rnorm(n)
height = rando*0.05*tree.height+tree.height-0.05/2*tree.height
width = rando*0.05*crown.width+crown.width
y = height/2


grobs.tb <- tibble(x = x, y =y,
                   width = width/50,
                   height =  height/50,
                   grob = list(tree1))

ggplot() +
  geom_grob(data = grobs.tb, 
            aes(x, y, label = grob, vp.width = width, vp.height = height)
            ) +
  scale_y_continuous(name='canopy height (m)',breaks=c(0:25)*5) +
  scale_x_continuous(breaks=c(0:25)*5) +
  coord_fixed(ylim = c(0,50), xlim = c(0,50), expand = F)+
  theme_bw(12)



maketree2 <- function(tree.height, branch.height, crown.width, diameter){
  
  
  
  
  
  tree.ht=tree.height/tree.height
  crn.ht=branch.height/tree.height
  crn.wd=crown.width/tree.height
  tree.wd=diameter/100/tree.height
  
  
  
  g.crown.round <- polygonGrob(x=c(0.5-crn.wd/2,
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
                                   crn.ht)^0.5, gp=gpar(fill='green',alpha=0.5))
  g.trunk <- rectGrob(x=0.5, y=((crn.ht-0)^0.5)/2, width=tree.wd, height=(crn.ht-0)^0.5, gp=gpar(fill='brown'))
  return(grobTree(g.crown.round, g.trunk))
}

tree2 <- maketree2(tree.height, branch.height, crown.width, diameter)
grid.newpage()
grid.draw(tree2)

n=round(ntrees(total.cover, crown.width, 50),0)

x = sample(0:50, n)
rando =  rnorm(n)
height = rando*0.05*tree.height+tree.height-0.05/2*tree.height
width = rando*0.05*crown.width+crown.width
y = height/2


grobs.tb <- tibble(x = x, y =(y*2)^0.5/2,
                   width = width/50,
                   height =  ((height)/50)^0.5,
                   grob = list(tree2))

ggplot() +
  geom_grob(data = grobs.tb, 
            aes(x, y, label = grob, vp.width = width, vp.height = height)
  ) +
  scale_y_continuous(name='canopy height (m)',breaks=(c(0:4,(1:25)*5))^0.5,labels=c(0:4,(1:25)*5)) +
  scale_x_continuous(breaks=c(0:25)*5) +
  coord_fixed(ratio = 50/50^0.5, ylim = c(0,50^0.5), xlim = c(0,50), expand = F)+
  theme_bw(12)
