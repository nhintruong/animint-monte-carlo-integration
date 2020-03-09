library(animint2)
library(plyr)

HitOrMissMonteCarlo <- function() {
  FUN <- function(x) x-x^2
  data.generator = function() {
    
    from <- 0 
    to <- 1
    max <- 500
    shape.option <- c(20, 4)
    
    x1 <- runif(max, from, to)
    ymin <- optimize(FUN, c(from, to), maximum = FALSE)$objective
    ymax <- optimize(FUN, c(from, to), maximum = TRUE)$objective
    x2 <- runif(max, ymin, ymax) 
    y <- FUN(x1)
    
    serial <- seq(1, max)
    under.The.Curve <- (x2 < y)
    shape <- shape.option[(x2 < y) + 1]
    
    generated.iter <- data.frame(iter = serial)
    generated.data <- data.frame("S.No" = serial, "y" = y, "x1" = x1, "x2" = x2, "Under.The.Curve" = under.The.Curve, "shape" = shape)
    
    invisible(list(data=generated.data, iter=generated.iter))
  }
  
  init <- data.generator()
  data <- init$data
  iteration <- init$iter
  
  data <- ldply(data$S.No, function(i) {
    dfAll <- subset(data, S.No<=i)
    df <- subset(data, S.No==i)
    trueCount <- length(dfAll$Under.The.Curve[dfAll$Under.The.Curve]==TRUE)
    calcPI <- (2*i)/trueCount
    cbind(df, pi=calcPI)
  })
  iteration <- cbind(iteration, pi = data$pi)
  
  data <- ldply(data$S.No, function(i) {
    df <- subset(data, S.No <= i)
    cbind(df, iter = i)
  })
  
  viz.one <- ggplot(data.frame(x=c(0,1)), aes(x=x))+
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
  
  viz.two <- ggplot()+
    ggtitle("Monte Carlo Estimation of \u03C0")+
    geom_tallrect(aes(
      xmin=iter-0.5, xmax=iter+0.5),
      clickSelects="iter",
      alpha=0.5,
      data=iteration)+
    geom_hline(yintercept = pi, size=1)+
    geom_path(aes(S.No, pi, key=iter),
              showSelected="iter",
              size=1,
              data=data)+
    geom_text(aes(iter, pi, key=iter, label=paste("\u03C0", "=", pi, sep = " ")),
              showSelected=c("iter"),
              size=20,
              data=iteration)+
    labs(x="Iterations", y="Value of \u03C0")+
    scale_y_continuous(breaks = c(1, 2, 3, pi, 4, 5, 6, 7, 8))+
    theme_bw()+
    theme_animint(width=600, height=600)
  
 (viz.publish <- animint(viz.one,
                         viz.two,
                         title="Hit or Miss Monte Carlo Integration",
                         time=list(variable="iter", ms=200),
                         first=list(iter=1)))
  return(viz.publish)
}

viz <- HitOrMissMonteCarlo()
animint2gist(viz)
animint2dir(viz, "MediumOutput")
