setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# remotes::install_github('https://github.com/pmur002/gggrid')
# install.package('remotes')
# remotes::install_github('coolbutuseless/cssparser') # Handles CSS styling
# remotes::install_github('coolbutuseless/svgparser')
library(svgparser)
#https://cran.r-project.org/web/packages/ggpp/vignettes/grammar-extensions.html#geom_grob



convert.SVG <- function(filename){

  blob <- svgparser::read_svg(filename, obj_type = 'data.frame')

  blob$x <- (blob$x - (max(blob$x) + min(blob$x))/2) / (max(blob$x) - min(blob$x))
  blob$y <- 1-1*(blob$y - min(blob$y)) / (max(blob$y) - min(blob$y))
  blob <- subset(blob)
  return(blob)

}
tree <- convert.SVG('complextree.svg')
library(sf)
library(dplyr)
library(ggplot2)
ggplot()+
  geom_polygon(data=tree, aes(x=x,y=y, group=elem_idx), fill=tree$fill, alpha=0.1)

library(vegnasis)

d = c(5 ,10,20,30,40,50,100,200,300)
w = est_crown_width(d)
((d/100)/w)^-1

h = c(5,12,30,50)
d = fill.diameters(h)
h/(d/100)

tree = tree |> mutate(x = ifelse(x > -0.05 & x < 0.05, 0,x), x = dplyr::case_when(y < 0.05 & x < 0 ~ -0.05,
                                                                                                y < 0.05 & x > 0 ~ 0.05,
                                                                                                TRUE ~ x))
tree = tree |> mutate(y = y*0.1*40)
tree = tree |> group_by(elem_idx) |> mutate(xmax = max(x), xmin = min(x), ymax = max(y), ymin = min(y))
tree.branch = subset(tree, !elem_idx %in% c(2,34,33))
ggplot()+
  geom_polygon(data=tree.branch, aes(x=x,y=y, group=elem_idx), fill=tree.branch$fill, alpha=0.1)+
  coord_fixed()
ggplot()+
  geom_polygon(data=tree, aes(x=x,y=y, group=elem_idx), fill=tree$fill, alpha=0.1)+
  coord_fixed()

x= c(0,1)
y= c(0,1)
atan((y[1]-y[2])/(x[1]-x[2]))#/2/3.141592*360


trbr <- st_as_sf(tree.branch, coords=c("x", "y"))
trbr = trbr %>% 
  group_by(elem_idx) %>% 
  summarise() %>%
  st_cast("POLYGON")
trbr.1<- st_union(trbr)  |> st_buffer(0.005)
# trbr.1<- trbr
separated_coord <- data.frame(x = st_coordinates(trbr.1)[,1],
         y = st_coordinates(trbr.1)[,2],
         L1= st_coordinates(trbr.1)[,3],
         L2= st_coordinates(trbr.1)[,4])

ggplot()+
  geom_polygon(data=separated_coord, aes(x=x,y=y, group=L2, subgroup = L1), alpha=0.1)

blob <- convert.SVG('blob.svg')
trunk <- convert.SVG('trunk.svg')
palm <- convert.SVG('palm.svg')

write.csv(blob, 'blob.csv', row.names = F)
write.csv(trunk, 'trunk.csv', row.names = F)
write.csv(palm, 'palm.csv', row.names = F)

conifer1 <- convert.SVG('conifer1.svg') 
write.csv(conifer1, 'conifer1.csv', row.names = F)

conifer2 <- convert.SVG('conifer2.svg') 
write.csv(conifer2, 'conifer2.csv', row.names = F)

cloud1 <- convert.SVG('cloud1.svg') 
write.csv(cloud1, 'cloud1.csv', row.names = F)

flattop <- convert.SVG('flattop.svg') 
write.csv(flattop, 'flattop.csv', row.names = F)

flattop2 <- convert.SVG('flattop2.svg') 
write.csv(flattop2, 'flattop2.csv', row.names = F)