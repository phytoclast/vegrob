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

  x = c(10,10)

    gm <- (1-(10^(sum(log10(1-x/100))))^(1/length(x)))*100
  x3 <- (1:length(x))*0+gm
  100*(1-10^(sum(log10(1-(x/100)))))

  100*(1-10^(sum(log10(1-(x3/100)))))


 1-(x/100)
 10^(sum(log10(1-(x/100))))
 10^(sum(log10(1-(x/100))))*.5
 100*(1-   10^(sum(log10(1-(x/100)))) *.5        )

 n = 5
 p <- c(1,5,10,25,50,75,80,95,99)
 df <- data.frame(p=p, sp = 0, ap=0)
 df$sp = n*df$p
 df$ap = (1-(1-df$p/100)^n)*100

 df <- df |> mutate(final = (1-ap/100))

 ggplot()+
   geom_line(data=df, aes(x=sp, y=ap))+
   geom_line(data=df, aes(x=sp, y=final), col='red')

#scatter plot loops to semi-random establish even spaced trees to determing deviation from random cover.
scl=3 #scale larger
x = (1:(100*scl))
y = (1:(116*scl))
set.seed(42)


m <- merge(x,y) |> as.data.frame()
m <- m |> mutate(x = ifelse(floor(y/2)==y/2, x-0.5,x), y = round(y*(3^0.5)/2,2), wt=1)

r = 5
pref = 3
f=pref
mx1 <- merge(x*f,y*f) |> as.data.frame() |> mutate(x = ifelse(floor(y/2)==y/2, x-f*0.5,x), y = round(y*(3^0.5)/2,2), wt=f)  |> subset(x <=100*scl & y <= 100*scl)
f = pref^2
mx2 <- merge(x*f,y*f) |> as.data.frame() |> mutate(x = ifelse(floor(y/2)==y/2, x-f*0.5,x), y = round(y*(3^0.5)/2,2), wt=f)  |> subset(x <=100*scl & y <= 100*scl)
f = pref^3
mx3 <- merge(x*f,y*f) |> as.data.frame() |> mutate(x = ifelse(floor(y/2)==y/2, x-f*0.5,x), y = round(y*(3^0.5)/2,2), wt=f)  |> subset(x <=100*scl & y <= 100*scl)
mm <- rbind(m,mx1,mx2,mx3) |> group_by(x,y) |> summarise(wt=max(wt)) |> as.data.frame()

rownames(mm) <- 1:nrow(mm) |> as.numeric()
nums <- c(1:4, (1:100)*5)*scl^2

for (i in 1:length(nums)){
n=nums[i]
area = 3.141592*r^2
s <- mm[sample(1:(11600*scl^2), n, replace = F, prob = mm$wt^5*1+0),] |> mutate(z = (pmax(1,rnorm(n)*0.00*area+area)/3.141592)^0.5)
s <- st_as_sf(x = s, coords = c('x', 'y'))
rst <- rast(xmin = 0, xmax = 100*scl, ymin = 0, ymax = 100*scl, resolution = c(0.1,0.1))
buf <- sf::st_buffer(s, dist=s$z)
rbuf <- rasterize(buf, rst, values=1)
rbuf <- ifel(is.na(rbuf), 0,1)
rbuf.df <- as.data.frame(rbuf)
actual = mean(rbuf.df$layer)*100
potential <- n*(3.141592*r^2)/((100*scl)^2)*100
aggformula <- (1-(1-1*(3.141592*r^2)/((100*scl)^2))^n)*100
covtab0 <- data.frame(n=n,potential=potential,aggformula=aggformula,actual=actual)
if(i==1){covtab <- covtab0}else{covtab <- rbind(covtab,covtab0)}
}

cov1 <- covtab#f3 even diameter
# cov2 <- covtab#f3 uneven diameter variable wts
# cov3 <- covtab#f3 random

x=cov1$potential
y=cov1$actual

mod1 <- minpack.lm::nlsLM(y ~ b1*(1-exp(b2*x))^(b3) , start = list(b1=100, b2=-1, b3=1))
summary(mod1)

#proof that iterating geometric mean of gap cover equivalence can be used the same as variable cover.
# x = c(10,10)
#
# gm <- (1-(10^(sum(log10(1-x/100))))^(1/length(x)))*100
# x3 <- (1:length(x))*0+gm
# 100*(1-10^(sum(log10(1-(x/100)))))
#
# 100*(1-10^(sum(log10(1-(x3/100)))))
gmcover = c(1:100)
interations = c(1:50)
df <- merge(data.frame(gmcover=gmcover), data.frame(interations=interations))
df <- df |> mutate(potential=interations*(gmcover), aggformula= (1-(1-1*gmcover/100)^interations)*100)



ggplot()+
  geom_line(data=subset(df, interations == 1), aes(x=potential, y=aggformula),col='black')+
  geom_line(data=subset(df, interations == 2), aes(x=potential, y=aggformula),col='red')+
  geom_line(data=subset(df, interations == 3), aes(x=potential, y=aggformula),col='yellow')+
  geom_line(data=subset(df, interations == 5), aes(x=potential, y=aggformula),col='green')+
  geom_line(data=subset(df, interations == 10), aes(x=potential, y=aggformula), col='blue')+
  geom_line(data=subset(df, interations == 50), aes(x=potential, y=aggformula), col='purple')+
  geom_line(data=cov1, aes(x=potential, y=actual), col='orange')+
  coord_cartesian(xlim = c(0,200),ylim = c(0,120))+
  scale_x_continuous(breaks=c(0:30)*50, minor_breaks = c(0:30)*10)+
  scale_y_continuous(breaks=c(0:30)*50, minor_breaks = c(0:30)*10)

x=cov1$potential
y=cov1$aggformula
mod1 <- minpack.lm::nlsLM(y ~ b1*(1-exp(b2*x))^(b3) , start = list(b1=100, b2=-1, b3=1))
summary(mod1)
# Formula: y ~ b1 * (1 - exp(b2 * x))^(b3)
#
# Parameters:
#   Estimate Std. Error    t value Pr(>|t|)
#   b1  1.000e+02  2.476e-15  4.038e+16   <2e-16 ***
#   b2 -1.000e-02  1.479e-18 -6.765e+15   <2e-16 ***
#   b3  1.000e+00  1.281e-16  7.807e+15   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 8.095e-15 on 101 degrees of freedom
#
# Number of iterations to convergence: 12
# Achieved convergence tolerance: 1.49e-08
x=cov1$potential
y=cov1$actual
mod1 <- minpack.lm::nlsLM(y ~ b1*(1-exp(b2*x))^(b3) , start = list(b1=100, b2=-1, b3=1))
# summary(mod1)
# Formula: y ~ b1 * (1 - exp(b2 * x))^(b3)
#
# Parameters:
#   Estimate Std. Error t value Pr(>|t|)
#   b1  1.003e+02  3.369e-01  297.58   <2e-16 ***
#   b2 -2.462e-02  7.859e-04  -31.33   <2e-16 ***
#   b3  2.023e+00  9.846e-02   20.55   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 2.381 on 101 degrees of freedom
#
# Number of iterations to convergence: 13
# Achieved convergence tolerance: 1.49e-08
#
#many iterations of small area species is the same as small crowns randomly overlapping.
#The most evenly distributed crowns shows an intermediate cover between aggregate formula and simple summation of cover values until it converges with 100%.
#Introduction of any variability in crown width or relaxing of weights towards even distribution will quickly reduce the total cover to the same as the random formula.
x <- c(1:4,(1:60)*5) 
x <- data.frame(x=x)
p1 = 1.000e+02
p2 = -1.000e-02
p3 = 1.000e+00

b1 = 1.003e+02
b2 = -2.462e-02
b3 = 2.023e+00
x <- x |> mutate(y1 = p1 * (1 - exp(p2 * x))^(p3),
                 y2 = b1 * (1 - exp(b2 * x))^(b3),
                 y3 = 100*(y1/100)^(y1/x),
                 y4 = 100*(x/(100+x))^0.5,
                 x1 = pmin(x,100))


ggplot()+
  geom_line(data=x, aes(x=x, y=x1),col='black')+
  geom_line(data=x, aes(x=x, y=y1),col='red')+
  geom_line(data=x, aes(x=x, y=y2),col='green')+
  geom_line(data=x, aes(x=x, y=y4),col='blue')







cov1 <- cov1 |> mutate(ratio = actual/potential)
ggplot()+
  geom_line(data = cov1, aes(x=potential, y=ratio))
#scatter plot of trees
scl=4#scale
x = (1:(100*scl))
y = (1:(116*scl))
set.seed(42)


m <- merge(x,y) |> as.data.frame()
m <- m |> mutate(x = ifelse(floor(y/2)==y/2, x-0.5,x), y = round(y*(3^0.5)/2,2), wt=1)

r = 5
pref = 3
f=pref
mx1 <- merge(x*f,y*f) |> as.data.frame() |> mutate(x = ifelse(floor(y/2)==y/2, x-f*0.5,x), y = round(y*(3^0.5)/2,2), wt=f)  |> subset(x <=(100*scl) & y <= (100*scl))
f = pref^2
mx2 <- merge(x*f,y*f) |> as.data.frame() |> mutate(x = ifelse(floor(y/2)==y/2, x-f*0.5,x), y = round(y*(3^0.5)/2,2), wt=f)  |> subset(x <=(100*scl) & y <= (100*scl))
f = pref^3
mx3 <- merge(x*f,y*f) |> as.data.frame() |> mutate(x = ifelse(floor(y/2)==y/2, x-f*0.5,x), y = round(y*(3^0.5)/2,2), wt=f)  |> subset(x <=(100*scl) & y <= (100*scl))
mm <- rbind(m,mx1,mx2,mx3) |> group_by(x,y) |> summarise(wt=max(wt)) |> as.data.frame()

rownames(mm) <- 1:nrow(mm) |> as.numeric()

n=100
area = 3.141592*r^2
s <- mm[sample(1:(11600*4*4), n, replace = F, prob = mm$wt^3),] |> mutate(z = ((rnorm(n)*0.33*area+area)/3.141592)^0.5)
s1=s
s <- st_as_sf(x = s, coords = c('x', 'y'))
rst <- rast(xmin = 0, xmax = (100*scl), ymin = 0, ymax = (100*scl), resolution = c(0.1,0.1))
buf <- sf::st_buffer(s, dist=s$z)
rbuf <- rasterize(buf, rst, values=1)


rbuf <- ifel(is.na(rbuf), 0,1)
plot(rbuf)
plot(s, col='black', add = TRUE)
df <- as.data.frame(rbuf)
mean(df$layer)

n*(3.141592*r^2)/(100^2)
1-(1-1*(3.141592*r^2)/(100^2))^n
mean(s$wt)


ggplot()+
  geom_point(data = mm, aes(x=x,y=y,size=wt))











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
