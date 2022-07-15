
a=0.75
b=0.75

1-(1-a)*(1-b)

a2=a/2
b2=b/2

a1=a-a2
b1=b-b2

td = a2+b2
cd = 1-(1-a)*(1-b)


a+b
1-(1-a)*(1-b)
cd*(1-td)+td

linearcover = a+b
geometriccover = 1-(1-a)*(1-b)

((cd*(1-td)+td)-geometriccover)/linearcover



agg.cover <- function(cover,max.overlap){
  geometriccover <- 1-exp(sum(log(1-cover)))
  linearcover <- sum(cover*(1-max.overlap))
  total.cover = geometriccover*(1-linearcover)+linearcover
  return(total.cover)
}

cv <- c(0.02,0.1,0.75)

calc = agg.cover(cv, 0.95)

actual = .99
0.9*.05

calc*0.95
max.overlap=0.95
cover = c(0.02,0.1,0.75,0.9)
gcover <- 1-exp(sum(log(1-cover)))
lcover <- sum(cover*(1-max.overlap))
total.cover = gcover*(1-lcover)+lcover

r = actual/gcover
r2 = log(1-actual)/log(1-gcover)
#r3 = log((1-actual)/(1-gcover))
lcover * r 
gcover2 = 1-exp(log(1-cover)*r2)
#gcover3 = 1-exp(log(1-cover)+r3)
gcover
1-exp(sum(log(1-gcover2)))

gcover2 - cover
gcover2 / cover
(1-gcover2)-(1-cover)
(1-gcover2)/(1-cover)
log(1-gcover2)/log(1-cover)

(1-cover)^1.1/(1-cover)

cover
1-(1-cover)^1.1

e = c(1:100)*0.05
for(i in 1:100){
cx = 1-(1-cover)^e[i]
c0=agg.cover(cx,0.95)
if(i==1){cc=c0}else{cc=c(cc,c0)}
}


cover2 = c(0.2,0.1,0.5,0.9,0.8,0.9,0.9)
e = c(1:100)*0.05
for(i in 1:100){
  cx = 1-(1-cover2)^e[i]
  c0=agg.cover(cx,0.95)
  if(i==1){cc2=c0}else{cc2=c(cc2,c0)}
}
plot(cc~e)
plot(cc2~e,col='red', add=T)
library(ggplot2)

ggplot()+
  geom_line(aes(x=e, y=cc), col='blue')+  
  geom_line(aes(x=e, y=cc2), col='red')




#newSubcanopy: 100-100*Exp(IIf([subcanopy]>=100,-3,Log(1-[Subcanopy]/100))*Log(1-[Tree_Cover]/100)/[lnOverstory])
#
#
#
#
#
#
#
#
#
#
#

neff <- function(x) {
  1/sum((x/sum(x))^2)
}
qmean <- function(x) {
  mean(x^2)^0.5
}
peff <- function(x) {
  1-mean(1-x)
}

agcov <- function(x){
  1-(exp(sum(log(1-x))))
}

#----


x=c(0.8,0.2,0.1)

neff(x)
qmean(x)
1/neff(x)
agcov(x)
d=0.99
f0 = (1-((1-d)^(1/neff(x))))/(1-((1-agcov(x))^(1/neff(x))))
#f = d/agcov(x)
f = (d/agcov(x))^1.5
x*f0
xf = x*f
xf
#xf = xf/max(1,(max(xf)))*0.95
#xf = 1-(1-x)/f
xf = pmin(xf,0.99)
xf
agcov(xf)

ff = log(1-d)/log(1-agcov(xf))
xff = 1-exp(ff*log(1-xf))
xff
agcov(xff)


ff2 = log(1-d)/log(1-agcov(x))
xff2 = 1-exp(ff2*log(1-x))

agcov(xff2)
xff2
x/max(x)
xf/max(xf)
xff/(max(xff))
xff2/(max(xff2))







ff = ((1-((1-d)^(1/neff(x))))/(1-((1-agcov(xf))^(1/neff(x))))+1)/2
ff
xff =  xf*ff
xff = xff/max(1,(max(xff)))
xff
agcov(xff)
fff = ((1-((1-d)^(1/neff(x))))/(1-((1-agcov(xff))^(1/neff(x))))+1)/2
fff
xfff =  xff*fff
xfff = xfff/max(1,(max(xfff)))
xfff
agcov(xfff)
ffff = ((1-((1-d)^(1/neff(x))))/(1-((1-agcov(xfff))^(1/neff(x))))+1)/2
ffff
xffff =  xfff*ffff
xffff = xffff/max(1,(max(xffff)))
xffff
agcov(xffff)





f10 = 7
fba = 10*f10
actoha = 43560*0.3048^2/10000
ft2tom2 = 0.3048^2
mba = fba*ft2tom2/actoha

diam = 35
m2pertree = 3.141592*(diam/200)^2
treesperha = mba/m2pertree
cover = 0.8
coverpertree = 1-(1-cover)^0.5
coverpertree
areapertree = coverpertree*10000/treesperha
crown.width = (areapertree/3.141592)^0.5*2
crown.width
crown.stem.ratio = crown.width/diam*100
crown.stem.ratio

(areapertree/m2pertree)^0.5



crown.width=11.1

coverpertreeperha = (crown.width/2)^2*3.141892/10000
coverperha = 1-(1-coverpertreeperha)^treesperha
coverperha

coverperha = 1-(1-coverpertreeperha)^treesperha
1-coverperha = (1-coverpertreeperha)^treesperha
(1-coverperha)^(1/treesperha) = (1-coverpertreeperha)
coverpertreeperha = 1-(1-coverperha)^(1/treesperha)


coverpertreeperha = (crown.width/2)^2*3.141892/10000
coverpertreeperha*10000/3.141592 = (crown.width/2)^2
(coverpertreeperha*10000/3.141592)^0.5 = (crown.width/2)
crown.width = 2*(coverpertreeperha*10000/3.141592)^0.5

cover = 0.8
treesperha
coverpertreeperha = 1-(1-cover)^(1/treesperha)
crown.width = 2*(coverpertreeperha*10000/3.141592)^0.5
crown.width
crown.stem.ratio = crown.width/diam*100
crown.stem.ratio
