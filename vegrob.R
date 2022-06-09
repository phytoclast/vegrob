# remotes::install_github('https://github.com/pmur002/gggrid')
library(grid)
library(gggrid)
library(svgparser)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Loat SVG as a grob
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rlogo_url <- 'https://www.r-project.org/logo/Rlogo.svg'
g <- svgparser::read_svg(rlogo_url)
g$vp <- grid::viewport(x = 0.8, y = 0.8, width = unit(.4, 'npc'), height = unit(.4, 'snpc'))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define a callfback function to use within each panel.
# See 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
func <- function(data, coords) {
  
  position_grob <- function(row) {
    gnew <- g
    gnew$vp$x      <- unit(row[['x']], 'npc')
    gnew$vp$y      <- unit(row[['y']], 'npc')
    gnew$vp$width  <- unit(row[['size']]/40, 'npc')
    gnew$vp$height <- unit(row[['size']]/40, 'npc')
    gnew$name <- strftime(Sys.time(), "%H%M%OS6") # Enforce unique name per grob. 
    gnew
  }
  
  grobs <- lapply(seq(nrow(coords)), function(idx) {position_grob(coords[idx, ])})
  do.call(grid::grobTree, grobs)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot wih a `gggrid::grid_panel()`
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot(mtcars, aes(mpg, wt, size = cyl)) + 
  grid_panel(func) + 
  theme_bw() +
  labs(title = "{svgparser} + {gggrid}: Custom ggplot points with SVG")


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

vp <- viewport(x=unit(0,'npc'), y=unit(.5,'npc'), width=unit(1,'npc'), height=unit(1,'npc'))
tree <- grobTree(g.crown, g.trunk, vp=vp)

ggplot()+
  geom_point(aes(x=x,y=y), data = treedist)+
  grid_panel(grob = tree)


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

maketree1 <- function(tree.height, branch.height, crown.width, diameter){
  

  
  
  
  tree.ht=tree.height/tree.height
  crn.ht=branch.ht/tree.height
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
                                   crn.ht), gp=gpar(fill='green',alpha=0.5))
  g.trunk <- rectGrob(x=0.5, y=(crn.ht-0)/2, width=tree.wd, height=(crn.ht-0), gp=gpar(fill='brown'))
  return(grobTree(g.crown.round, g.trunk))
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



# remotes::install_github('https://github.com/pmur002/gggrid')
library(grid)
library(gggrid)

hex <- FALSE
if (hex) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the template grob - a hex!
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  r     <- 0.5
  theta <- seq(30, 360, 60) * pi/180
  x     <- r * cos(theta)
  y     <- r * sin(theta)
  g     <- grid::polygonGrob(x = x, y = y, vp = viewport(), gp = gpar())
} else {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the template grob - a 5 pointed star
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  r     <- 0.5
  theta <- seq(36, 360, 72) * pi/180
  x     <- r * cos(theta)
  y     <- r * sin(theta)
  g     <- grid::polygonGrob(x = x[c(1,3,5,2,4)], y = y[c(1,3,5,2,4)], vp = viewport(), gp = gpar())
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define the callback function to use within each panel.
# See https://www.stat.auckland.ac.nz/~paul/Reports/gggrid/gggrid.html
#
# @param data data.frame of original data
# @param coords data.frame of values scaled to fit into the desired output units
#               as well as any aesthetic values. 
#               One row per point.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
panel_func <- function(data, coords) {
  
  # Function which will be called for each row in 'coords'
  position_grob <- function(point) {
    gnew           <- g
    gnew$gp$fill   <- point$fill
    gnew$gp$col    <- NA  #point$colour
    gnew$gp$alpha  <- point$alpha
    gnew$vp$x      <- unit(point[['x']], 'npc')
    gnew$vp$y      <- unit(point[['y']], 'npc')
    gnew$vp$width  <- unit(point[['size']]/40, 'npc')
    gnew$vp$height <- unit(point[['size']]/40, 'npc')
    gnew$name <- strftime(Sys.time(), "%H%M%OS6") # Enforce unique name per grob. 
    gnew
  }
  
  # For each point, generate a grob
  grobs <- lapply(
    seq(nrow(coords)), 
    function(idx) {
      position_grob(coords[idx, ])
    })
  
  # group all the point grobs into a single grobTree object
  do.call(grid::grobTree, grobs)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot wih a `gggrid::grid_panel()`
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot(
  data = mtcars, 
  mapping = aes(
    x      = mpg, 
    y      = wt, 
    size   = cyl, 
    fill   = as.factor(cyl)
  )
) + 
  gggrid::grid_panel(panel_func) + 
  theme_bw() +
  labs(title = "{gggrid}: Custom ggplot points")