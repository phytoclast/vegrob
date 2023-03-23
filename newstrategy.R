library(ggplot2)

b1 <- data.frame(x=c(0,1,2), y=c(1,5,1), id='1a', type='1crown')
b2 <- data.frame(x=c(0.8,0.8,1.2,1.2), y=c(0,1,1,0), id='1b', type='2trunk')
b3 <- data.frame(x=b1$x+1, y=b1$y, id='2a', type='1crown')
b4 <- data.frame(x=b2$x+1, y=b2$y, id='2b', type='2trunk')
blobs <- rbind(b1,b2,b3,b4)
ggplot() +
  geom_polygon(data=blobs, aes(x,y, group=id, fill=type, color=type), alpha=0.8)+
  scale_fill_manual(values=c('green','orange'))+
  scale_color_manual(values=c('darkgreen','brown'))+
  theme(legend.position = NULL)+
  scale_y_continuous(trans='sqrt')
