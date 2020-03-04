library(animint2)
UPIData <- read.csv('./data/UPIData.csv')

viz.plot.one <- ggplot()+
  ggtitle("No. of Banks vs. Transaction")+
  geom_point(aes(x=No.Of.Banks.Using.Upi, y=Volume.Of.Transaction.in.Mn, color=Month, key=Month),
             size=6,
             clickSelects="Month",
             showSelected="Year",
             data=UPIData)+
  geom_path(aes(x=No.Of.Banks.Using.Upi, y=Volume.Of.Transaction.in.Mn),
            size=2,
            data=UPIData)+
  theme_bw()

viz.plot.two <- ggplot()+
  ggtitle("Transaction: Volume vs. Amount")+
  geom_point(aes(x=Volume.Of.Transaction.in.Mn, y=Amount.Of.Transaction.In.Bn, color=Month, key=Month),
             size=5,
             clickSelects="Month",
             showSelected="Year",
             data=UPIData)+
  geom_step(aes(x=Volume.Of.Transaction.in.Mn, y=Amount.Of.Transaction.In.Bn),
            size=2,
            linetype=4,
            data=UPIData)+
  theme_bw()

(viz.publish <- animint(
  viz.plot.one,
  viz.plot.two,
  title="Unified Payments Interface(UPI) Data India",
  duration=list(Year=1000),
  time=list(variable="Year", ms=2000),
  selector.types=list(Month="multiple"),
  first=list(Year=2016)
))

animint2gist(viz.publish)
