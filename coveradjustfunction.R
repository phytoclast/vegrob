
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

cv <- c(0.1,0.1,0.1,0.1,0.6,0.1,0.1,0.1,0.1,0.1,0.1)

calc = agg.cover(cv, 0.95)

actual = 0.9
0.9*.05

calc*0.95
max.overlap=0.95
cover = cv
gcover <- 1-exp(sum(log(1-cover)))
lcover <- sum(cover*(1-max.overlap))
total.cover = geometriccover*(1-linearcover)+linearcover

r = actual/gcover
r2 = log(1-actual)/log(1-gcover)
r3 = log((1-actual)/(1-gcover))
lcover * r 
gcover2 = 1-exp(log(1-cover)*r2)
gcover3 = 1-exp(log(1-cover)+r3)

1-exp(sum(log(1-gcover2)))


newSubcanopy: 100-100*Exp(IIf([subcanopy]>=100,-3,Log(1-[Subcanopy]/100))*Log(1-[Tree_Cover]/100)/[lnOverstory])