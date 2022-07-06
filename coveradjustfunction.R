
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
xneff <- function(x) {
  1/sum((x)^2)
}
peff <- function(x) {
  1-mean(1-x)
}

agcov <- function(x){
  1-(exp(sum(log(1-x))))
}

#----


x=c(0.89,0.2,0.1)

neff(x)
xneff(x)
1/neff(x)
agcov(x)
d=0.2
f = ((1-((1-d)^(1/neff(x))))/(1-((1-agcov(x))^(1/neff(x))))+0)/1
f
xf = x*f
xf = xf/max(1,(max(xf)))
#xf = 1-(1-x)/f
xf
agcov(xf)
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








ff = log(1-d)/log(1-agcov(x))
xff = 1-exp(ff*log(1-x))
xff
agcov(xff)
xff/x



c*d = a*b*f
c*d = a*b*f

