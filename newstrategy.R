library(ggplot2)


crn.wd = 1
crn.ht = 0
tree.ht = 1
g.D.crown <- data.frame(x=c(0.5-crn.wd/2,
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
                             crn.ht))
g.D.crown







b5 = data.frame(g.D.crown, id='5a', type='1crown')


b1 <- data.frame(x=c(0,1,2), y=c(1,5,1), id='1a', type='1crown')
b2 <- data.frame(x=c(0.8,0.8,1.2,1.2), y=c(0,1,1,0), id='1b', type='2trunk')
b3 <- data.frame(x=b1$x+1, y=b1$y, id='2a', type='1crown')
b4 <- data.frame(x=b2$x+1, y=b2$y, id='2b', type='2trunk')
blobs <- rbind(b1,b2,b3,b4,b5)
ggplot() +
  geom_polygon(data=blobs, aes(x,y, group=id, fill=type, color=type), alpha=0.8)+
  scale_fill_manual(values=c('green','orange'))+
  scale_color_manual(values=c('darkgreen','brown'))+
  theme(legend.position = NULL)+
  scale_y_continuous(trans='sqrt')
