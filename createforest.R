library(sf)
library(terra)
library(vegnasis)
library(ggplot2)
veg <- clean.veg.log(obs, obsspp)
badrecords <- veg |> subset(!is.na(BA) & is.na(diam))

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
df <- forest |> subset(!is.na(BA) & !is.na(cover) & !is.na(diam) &  cover >= 75 & !plot %in% badrecords$plot)
df <- df |> mutate(dens = BA/(3.141592*(diam/200)^2), crwd = 2*((1-(1-cover/100)^(1/dens))*10000/3.141592)^0.5, 
                   confinedcrwd = ((10000/dens)/3.141592)^0.5*2) 
  x <- df$diam
  y <- df$crwd
  z <- df$confinedcrwd
  mod1 <- minpack.lm::nlsLM(z ~ b1*(1-exp(b2*x))^(b3) , start = list(b1=30, b2=-1, b3=1))
  summary(mod1)
  b1 = 40.654624    
  b2 = -0.009636      
  b3 = 1.442356      
  y1 <- b1*(1-exp(b2*x))^(b3)
  
 ggplot()+
   geom_smooth(aes(x=x, y=y), col='black')+
   geom_smooth(aes(x=x, y=z), col='blue')+
   geom_smooth(data=subset(df, cover >= 90), aes(x=diam, y=confinedcrwd), col='green')+
   geom_smooth(aes(x=x, y=y1), col='red')
   
  
 dfba <-  forest |> subset(!is.na(BA) & !is.na(cover))
 x=dfba$cover 
 y=dfba$BA 
 
 mod1 <- minpack.lm::nlsLM(y ~ b1*x^(b3) , start = list(b1=30,  b3=-1))
 summary(mod1)
 
 mod2  <- lm(y ~ x)
 
 y1 <- predict(mod1, x)
 y2 <- predict(mod2, list(x))
 
 ggplot()+
   geom_smooth(aes(x=x, y=y), col='black')+
 geom_smooth(aes(x=x, y=y1), col='red')+
 geom_smooth(aes(x=x, y=y2), col='blue')
 
  
  
  
#scatter plot of trees
x = (1:100)
y = (1:116)
set.seed(42)


m <- merge(x,y) |> as.data.frame()
m <- m |> mutate(x = ifelse(floor(y/2)==y/2, x-0.5,x), y = round(y*(3^0.5)/2,2), wt=1) 
#  scramble = data.frame(rx=runif(100*116)*0.5-0.5/2,ry = runif(100*116)*0.5-0.5/2)
# m <- m |> cbind(scramble)
# m <- m |> mutate(x=x+rx, y=y+ry, rx=NULL, ry=NULL)
f = 3
m3 <- merge(x*f,y*f) |> as.data.frame() |> mutate(x = ifelse(floor(y/2)==y/2, x-f*0.5,x), y = round(y*(3^0.5)/2,2), wt=3)  |> subset(x <=100 & y <= 100)
f = 5
m5 <- merge(x*f,y*f) |> as.data.frame() |> mutate(x = ifelse(floor(y/2)==y/2, x-f*0.5,x), y = round(y*(3^0.5)/2,2), wt=5)  |> subset(x <=100 & y <= 100)
f = 7
m7 <- merge(x*f,y*f) |> as.data.frame() |> mutate(x = ifelse(floor(y/2)==y/2, x-f*0.5,x), y = round(y*(3^0.5)/2,2), wt=7)  |> subset(x <=100 & y <= 100)
f = 9
m9 <- merge(x*f,y*f) |> as.data.frame() |> mutate(x = ifelse(floor(y/2)==y/2, x-f*0.5,x), y = round(y*(3^0.5)/2,2), wt=9)  |> subset(x <=100 & y <= 100)
f = 15
m15 <- merge(x*f,y*f) |> as.data.frame() |> mutate(x = ifelse(floor(y/2)==y/2, x-f*0.5,x), y = round(y*(3^0.5)/2,2), wt=15)  |> subset(x <=100 & y <= 100)
f = 17
m17 <- merge(x*f,y*f) |> as.data.frame() |> mutate(x = ifelse(floor(y/2)==y/2, x-f*0.5,x), y = round(y*(3^0.5)/2,2), wt=20)  |> subset(x <=100 & y <= 100)
f = 20
m20 <- merge(x*f,y*f) |> as.data.frame() |> mutate(x = ifelse(floor(y/2)==y/2, x-f*0.5,x), y = round(y*(3^0.5)/2,2), wt=20)  |> subset(x <=100 & y <= 100)
f = 21
m21 <- merge(x*f,y*f) |> as.data.frame() |> mutate(x = ifelse(floor(y/2)==y/2, x-f*0.5,x), y = round(y*(3^0.5)/2,2), wt=21)  |> subset(x <=100 & y <= 100)
f = 25
m25 <- merge(x*f,y*f) |> as.data.frame() |> mutate(x = ifelse(floor(y/2)==y/2, x-f*0.5,x), y = round(y*(3^0.5)/2,2), wt=25)  |> subset(x <=100 & y <= 100)
f = 27
m27 <- merge(x*f,y*f) |> as.data.frame() |> mutate(x = ifelse(floor(y/2)==y/2, x-f*0.5,x), y = round(y*(3^0.5)/2,2), wt=27)  |> subset(x <=100 & y <= 100)
f = 45
m45 <- merge(x*f,y*f) |> as.data.frame() |> mutate(x = ifelse(floor(y/2)==y/2, x-f*0.5,x), y = round(y*(3^0.5)/2,2), wt=45)  |> subset(x <=100 & y <= 100)

mm <- rbind(m,m3,m9,m27) |> group_by(x,y) |> summarise(wt=max(wt))

ggplot()+
  geom_point(data=mm, aes(x=x,y=y, size=wt))+
  geom_point(data=m, aes(x=x,y=y), col='white')+
  geom_point(data=m3, aes(x=x,y=y), col='blue')+
  geom_point(data=m9, aes(x=x,y=y), col='green')+
  geom_point(data=m27, aes(x=x,y=y), col='red')

  
mm <- rbind(m,m3,m5,m15,m25) |> group_by(x,y) |> summarise(wt=max(wt))
ggplot()+
  geom_point(data=mm, aes(x=x,y=y, size=wt))+
  geom_point(data=m, aes(x=x,y=y), col='white')+
  geom_point(data=m3, aes(x=x,y=y), col='cyan')+
  geom_point(data=m5, aes(x=x,y=y), col='blue')+
  geom_point(data=m15, aes(x=x,y=y), col='green')+
  geom_point(data=m25, aes(x=x,y=y), col='red')

mm <- rbind(m,m5,m25) |> group_by(x,y) |> summarise(wt=max(wt))
ggplot()+
  geom_point(data=mm, aes(x=x,y=y, size=wt))+
  geom_point(data=m, aes(x=x,y=y), col='white')+
  geom_point(data=m5, aes(x=x,y=y), col='blue')+
  geom_point(data=m25, aes(x=x,y=y), col='red')


mm <- as.data.frame(mm)
rownames(mm) <- 1:nrow(mm) |> as.numeric()
n=100
s <- mm[sample(1:11600, n, replace = F, prob = mm$wt^2),] |> mutate(z = rnorm(n)*0+5)
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
mean(s$wt)

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
