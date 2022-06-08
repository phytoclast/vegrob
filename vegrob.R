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
maketree <- function(treedist)
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
tree <- grobTree(g.crown, g.trunk, vp=vp)

ggplot()+
  geom_point(aes(x=x,y=y), data = treedist)+
  coord_cartesian(xlim = c(0,5),ylim = c(0,5)) +
  grid_panel(tree)
#https://cran.r-project.org/web/packages/ggpp/vignettes/grammar-extensions.html#geom_grob
library(ggpmisc)
library(dplyr)
file.name <- 
  system.file("extdata", "Isoquercitin.png", 
              package = "ggpp", mustWork = TRUE)
Isoquercitin <- magick::image_read(file.name)
grobs.tb <- tibble(x = c(0, 10, 20, 40), y = c(4, 5, 6, 9),
                   width = c(0.05, 0.05, .5, 1),
                   height =  c(0.05, 0.05, 1, 0.3),
                   grob = list(grid::circleGrob(), 
                               grid::rectGrob(), 
                               tree2,
                               grid::rasterGrob(image = Isoquercitin)))

ggplot() +
  geom_grob(data = grobs.tb, 
            aes(x, y, label = grob, vp.width = width, vp.height = height),
            hjust = 0.7, vjust = 0.55) +
  scale_y_continuous(expand = expansion(mult = 0.3, add = 0)) +
  scale_x_continuous(expand = expansion(mult = 0.2, add = 0)) +
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