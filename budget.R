
library (ggplot2)
library(grid)


# define function to create multi-plot setup (nrow, ncol)

vp.setup <- function(x,y){
  # create a new layout with grid
  grid.newpage()
  # define viewports and assign it to grid layout
  pushViewport(viewport(layout = grid.layout(x,y)))
}
# define function to easily access layout (row, col)
vp.layout <- function(x,y){
  viewport(layout.pos.row=x, layout.pos.col=y)
}

##########################################################

bdj<- read.table ("databu.txt", header=T)

str(bdj)
head(bdj)

levels(bdj$spesa)

bdj<-bdj[order(bdj$stato),]


p1<-ggplot(data=bdj, aes(x=spesa, y=importo, fill = spesa)) + 
  geom_bar(stat="identity") + 
  ggtitle("spesa per vettore")

p2<-ggplot(data=bdj, aes(x=spesa, y=importo, fill = stato)) + 
  geom_bar(stat="identity") + 
  ggtitle("spesa per vettore") 

jpeg("budget1.jpg",width = 600, height = 900, units = "px")

vp.setup(2,1)
# plot graphics into layout
print(p1, vp=vp.layout(1,1))
print(p2, vp=vp.layout(2,1))


dev.off()



p1<-ggplot(data=bdj, aes(x=spesa, y=importo, fill = spesa)) + 
  geom_bar(stat="identity", show_guide = F) + 
  facet_wrap(~stato, ncol=1) +
  ggtitle("spesa per vettore") 

p2<-ggplot(data=bdj, aes(x=spesa, y=importo, fill = stato)) +  
  geom_bar(stat="identity") +
  facet_wrap( ~ dove, ncol=2)+
  theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=12))


jpeg("budget2.jpg",width = 600, height = 900, units = "px")

vp.setup(2,1)
# plot graphics into layout
print(p1, vp=vp.layout(1,1))
print(p2, vp=vp.layout(2,1))


dev.off()

lstato<-levels(bdj$stato)
staimp<-sapply(lstato, function(x) sum(bdj$importo[bdj$stato==x]))

barplot(staimp)

##################################################

bdj<-bdj[order(bdj$spesa),]

p1<-  ggplot(data=bdj, aes(x=dove, y=importo, fill = spesa)) + 
  geom_bar(stat="identity") + 
  ggtitle("spesa per ubicazione")

p2<-  ggplot(data=bdj, aes(x=dove, y=importo, fill = spesa)) + 
  geom_bar(stat="identity")  +
  facet_wrap( ~ stato, ncol=1 ) + 
  ggtitle("spesa per ubicazione")

bdj<-bdj[order(bdj$stato),]

p3<-  ggplot(data=bdj, aes(x=dove, y=importo, fill = stato)) + 
  geom_bar(stat="identity") + 
  ggtitle("spesa per ubicazione")


jpeg("budget3.jpg",width = 600, height = 900, units = "px")

vp.setup(3,1)
# plot graphics into layout
print(p1, vp=vp.layout(1,1))
print(p2, vp=vp.layout(2,1))
print(p3, vp=vp.layout(3,1))

dev.off()
