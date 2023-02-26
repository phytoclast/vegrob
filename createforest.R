library(sf)
library(terra)
library(vegnasis)



x = (1:100)
y = (1:116)
set.seed(42)


m <- merge(x,y) |> as.data.frame()
m <- m |> mutate(x = ifelse(floor(y/2)==y/2, x-0.5,x), y = y*(3^0.5)/2)
#  scramble = data.frame(rx=runif(100*116)*0.5-0.5/2,ry = runif(100*116)*0.5-0.5/2)
# m <- m |> cbind(scramble)
# m <- m |> mutate(x=x+rx, y=y+ry, rx=NULL, ry=NULL)
n=200
s <- m[sample(1:11600, n, replace = F),] |> mutate(z = rnorm(n)*1+5)
s <- st_as_sf(x = s, coords = c('x', 'y'))
r <- rast(xmin = 0, xmax = 100, ymin = 0, ymax = 100, resolution = c(0.1,0.1))
buf <- sf::st_buffer(s, dist=s$z)
rbuf <- rasterize(buf, r, values=1)


rbuf <- ifel(is.na(rbuf), 0,1)
plot(rbuf)
plot(s, col='black', add = TRUE)
df <- as.data.frame(rbuf)
mean(df$layer)

n*(3.141592*5^2)/(100^2)
1-(1-1*(3.141592*5^2)/(100^2))^n


#Establish crown width scaled to diameter
get.crown.diam.ratio <- function(cover, dbh, ba){
  #cover = aggregate overstory cover
  #dbh = stand quadratic mean diameter
  #ba = stand basal area count

  m2pertree = 3.141592*(dbh/200)^2
  treesperha = ba/m2pertree
  coverpertreeperha = 1-(1-cover)^(1/treesperha)
  crown.width = 2*(coverpertreeperha*10000/3.141592)^0.5
  crown.stem.ratio = crown.width/dbh*100
  return(crown.stem.ratio)#crown stem ratio meter per meter
}
cover=.8
dbh=fill.diameters(30)
ba= 10
get.crown.diam.ratio(cover, dbh, ba)
