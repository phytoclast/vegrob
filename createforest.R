library(sf)
library(terra)
library(vegnasis)

veg <- clean.veg.log(obs, obsspp)

veg <- veg |> mutate(taxon=harmonize.taxa(veg$taxon, fix=T)) |> fill.type.df() |> fill.nativity.df() |> mutate(symbol = fill.usda.symbols(taxon)) |> fill.hts.df()
forest <-  veg |> mutate(h = ifelse(diam > 0 & BA > 0, ht.max, NA),
                         d = ifelse(diam > 0 & BA > 0, diam, NA),
                         b = ifelse(diam > 0 & BA > 0, BA, NA)) |>
  group_by(plot) |> filter(ht.max > 5) |>
  summarise(cover = cover.agg(cover), BA = sum(BA, na.rm = T),
            h=sum(h*b, na.rm = T), d=sum(d*b, na.rm = T), b=sum(b, na.rm = T)) |> mutate(BA = ifelse(BA > 0, BA, NA),
                                                                                         ht = ifelse(b == 0, NA, h/b),
                                                                                         diam = ifelse(b == 0, NA, d/b),
                                                                                         h = NULL, d = NULL, b=NULL)
df <- veg |> subset(!is.na(BA) & !is.na(cover) & !is.na(diam))

df <- df |> mutate(dens = BA/(3.141592*(diam/200)^2), crwd = 2*((1-(1-cover/100)^(1/dens))*10000/3.141592)^0.5)


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
