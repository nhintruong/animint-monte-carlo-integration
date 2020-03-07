library(animint2)

data(WorldBank, package="animint2")
WorldBank <- subset(WorldBank, year>=1980)
years <- data.frame(year=unique(WorldBank$year))

viz.one <- ggplot()+
  ggtitle("Life Expectancy vs Fertility Rate")+
  geom_point(aes(
    x=life.expectancy, y=fertility.rate, fill=region,
    key=country, size=population),
    showSelected="year",
    color="#000000"	,
    clickSelects="country",
    data=WorldBank)+
  geom_path(aes(
    x=life.expectancy, y=fertility.rate, color=region, key=region),
    showSelected="country",
    aplha=0.5,
    data=WorldBank)+
  geom_text(aes(
    x=life.expectancy, y=fertility.rate, label=paste(country, "in", year, sep=" "),
    key=country),
    showSelected=c("year", "country", "region"),
    data=WorldBank)+
  theme_linedraw()+
  theme_animint(width=1000, height=700)
  
viz.two <- ggplot()+
  ggtitle("Year vs Fertility Rate")+
  geom_tallrect(aes(
    xmin=year-0.5, xmax=year+0.5),
    clickSelects="year",
    alpha=0.5,
    data=years)+
  geom_line(aes(
    x=year, y=fertility.rate, group=country, color=region),
    clickSelects="country",
    size=3,
    alpha=0.6,
    data=WorldBank)+
  geom_text(aes(
    x=year, y=fertility.rate, label=country, key=country),
    showSelected=c("year", "country", "region"),
    data=WorldBank)+
  theme_linedraw()+
  theme_animint(width=1000, height=700)

(viz.animint <- animint(viz.one,
                        viz.two,
                        title="World Bank Interactive Visualization",
                        duration = list(year=2000),
                        time=list(variable="year", ms=3000)))

animint2dir(viz.animint, "output")
animint2gist(viz.animint, description = "Interactive World Bank Data Viz. 'Life Expectancy vs Fertility Rate and Year vs Fertility Rate'")
