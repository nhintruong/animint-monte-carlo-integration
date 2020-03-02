library(animint2)
UPIData <- read.csv('./data/UPIDatat.csv')


show.point.list <- list()
show.path.list <- list()

for(show.year in c(2017, 2018, 2019)){
  show.point.list[[paste(show.year)]] <- data.frame(
    show.year, subset(UPIData, show.year == Year))
  show.path.list[[paste(show.year)]] <- data.frame(
    show.year, subset(UPIData, show.year == Year))
}

show.point <- do.call(rbind, show.point.list)
show.path <- do.call(rbind, show.path.list)

viz.panels <- list(
  scatter=ggplot()+
    geom_point(aes(x=No.Of.Banks.Using.Upi, y=Volume.Of.Transaction.in.Mn, color=Month),
               size=6,
               data=show.point)+
    geom_path(aes(x=No.Of.Banks.Using.Upi, y=Volume.Of.Transaction.in.Mn),
              size=2,
               data=show.path)+
    facet_grid(. ~ show.year)+
    theme_bw()+
    theme(panel.margin=grid::unit(0, "lines"))+
    theme_animint(width=800, height=500)
)
structure(viz.panels, class="animint")
