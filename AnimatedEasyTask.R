library(animint2)

#Reading Happiness Index Data of 2015
HappinessData <- read.csv('./data/2015.csv')

viz.plot.one <- ggplot()+
  ggtitle("Happiness Score vs. Life Expectancy")+
  geom_point(aes(x=Happiness.Score, y=Health..Life.Expectancy., color=Region),
             size=6,
             showSelected="Region",
             shape=4,
             data=HappinessData)+
  geom_line(aes(x=Happiness.Score, y=Health..Life.Expectancy.),
            alpha=0.3,
            showSelected="Region",
            size=1,
            data=HappinessData)+
  theme_light()+
  xlab("Happiness Score")+
  ylab("Life Expectancy")+
  theme_animint(width=600, height=600)

viz.plot.two <- ggplot()+
  ggtitle("Freedom vs. Happiness Rank")+
  geom_point(aes(x=Freedom, y=Happiness.Rank),
             showSelected="Region",
             shape=23,
             data=HappinessData)+
  geom_bar(aes(x=Freedom, y=Happiness.Rank, fill=Region, color=Region),
           showSelected="Region",
           stat="identity",
           position="identity",
           data=HappinessData)+
  theme_light()+
  xlab("Freedom")+
  ylab("Happiness Rank")+
  theme_animint(width=600, height=600)



(viz.publish <- animint(
  viz.plot.one,
  viz.plot.two,
  title="Happiness Dataset Viz",
  duration=list(Region=1500),
  time=list(variable="Region", ms=3000),
  selector.types=list(Region="multiple"),
  first=list(Region="Eastern Asia")
))

animint2gist(viz.publish)
