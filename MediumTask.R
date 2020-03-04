library(animint2)
FUN <- function(x) x-x^2
data.generator = function() {
  
  from <- 0 
  to <- 1
  max <- 100
  shape.option <- c(20, 4)
  
  x1 <- runif(max, from, to)
  ymin <- optimize(FUN, c(from, to), maximum = FALSE)$objective
  ymax <- optimize(FUN, c(from, to), maximum = TRUE)$objective
  x2 <- runif(max, ymin, ymax) 
  y <- FUN(x1)
  
  serial <- seq(1, max)
  under.The.Curve <- (x2 < y)
  shape <- shape.option[(x2 < y) + 1]
  
  generated.data <- data.frame("S.No" = serial, "y" = y, "x1" = x1, "x2" = x2, "Under.The.Curve" = under.The.Curve, "shape" = shape)
  
  invisible(list(data=generated.data))
}

data <- data.generator()$data

library(plyr)
data <- ldply(data$S.No, function(i) {
  df <- subset(data, S.No <= i)
  cbind(df, iter = i)
})

viz.plot <- ggplot(data.frame(x=c(0,1)), aes(x=x))+
  stat_function(fun=FUN)+
  ggtitle("Hit or Miss Monte Carlo Integration")+
  geom_point(aes(x1,x2, key=S.No, color=Under.The.Curve, shape=Under.The.Curve),
             showSelected="iter",
             size=4,
             data=data)+
  scale_shape_manual(values = c("TRUE" = 20, "FALSE" = 4))+ # Now working in Animint2 Viz
  scale_color_manual(values = c("TRUE" = "black", "FALSE"="red"))+
  theme_bw()+
  theme_animint(width=600, height=600)

(viz.publish <- animint(viz.plot,
                        title="Hit or Miss Monte Carlo Integration",
                        time=list(variable="iter", ms=300),
                        first=list(iter=1)))

animint2gist(viz.publish)