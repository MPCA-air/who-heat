theData<-getDisagData(indicator=c("carep"), 
                      stratifier=c("Sex", "Economic status"), 
                      countries="Armenia", 
                      years=c(2010, 2005), 
                      mostrecent=FALSE,
                      datasource="All")


library(ggplot2)
library(gtable)

p <- ggplot(mtcars, aes(mpg, cyl))+
  facet_grid(gear~., labeller=label_both) + geom_point() +
  theme(strip.text.y=element_text(angle=90)) + labs(y="")


g <- ggplotGrob(p)

g$layout[g$layout$name == "strip-right",c("l", "r")] <- 3
g$layout[g$layout$name == "axis-l",c("l", "r")] <- 5
grid.newpage()
grid.draw(g)


g$layout[g$layout$name == "strip-right",] 
