library(animint2)
UPIData <- read.csv('./data/UPIData.csv')

viz.plot= ggplot()+
    ggtitle("Comparision of Unified Payments Interface(UPI) Usage Data")+
    geom_point(aes(x=No.Of.Banks.Using.Upi, y=Volume.Of.Transaction.in.Mn, color=Month, key=Month),
               size=6,
               clickSelects="Month",
               showSelected="Year",
               data=UPIData)+
    geom_path(aes(x=No.Of.Banks.Using.Upi, y=Volume.Of.Transaction.in.Mn),
              alpha=0.3,
              size=2,
              data=UPIData)+
    theme_bw()+
    theme_animint(width=800, height=500)

viz.animint <- animint(
  title="Comparision of Unified Payments Interface(UPI) Usage Data",
  duration=list(Year=1000),
  time=list(variable="Year", ms=2000),
  selector.types=list(Month="multiple"),
  first=list(Year=2016),
  viz.plot
  )
viz.animint
