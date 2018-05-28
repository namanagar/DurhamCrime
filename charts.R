library(knitr)
library(ggplot2)
library(tidyr)
crime = read.csv("./durham-police-crime-reports.csv", sep = ",")
kable(head(crime[,c("ï..INCI_ID", "STRDATE", "REPORTEDAS", "CHRGDESC")], 10), caption = "If you'd like to see the full data, it's included in my [Github repository](https://github.com/namanagar/DurhamCrime)")

# Get the Month vs Year table and turn it into a dataframe
heattable = xtabs(~ MONTHSTAMP + YEARSTAMP, crime)
heatmatrix = as.data.frame.matrix(heattable)
monthlist =  c("January","February","March","April","May","June","July","August","September","October","November","December")
heatmatrix$month = monthlist

# Use tidyr to turn it into a tidy (long) dataset, for ggplot
tidy1 = heatmatrix %>%
  gather(year, value, `2013`:`2017`, -month)

# Prevent alphabetical sort 
tidy1$month = factor(tidy1$month, levels=rev(monthlist))

# Color values
dark.orange <- '#F5A623'
light.gray <- '#f8f8f8'

# Heatmap
plot = ggplot(tidy1, aes(x=year, y=month, label=value)) +
  geom_tile(aes(fill=value), colour='white') +
  scale_fill_gradient(low=light.gray, high=dark.orange, limits=c(0, max(tidy1$value))) +
  geom_text()

plot = plot + 
  labs(title='Number of Gunshot Wounds in Durham, Jan. 2013 - Aug. 2017', x="Year", y="Month", subtitle="2017 data incomplete, only through August") +
  theme(
    panel.background=element_blank(),
    axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank()
  )



bar = ggplot(crime, aes(crime$YEARSTAMP)) + 
  geom_bar(fill="#F36170")

bar = bar + labs(title='Total Gunshot Wounds in Durham, Jan. 2013 - Aug. 2017', x="Year", y="Number of Gunshot Wounds", subtitle="2017 data incomplete, only through August")



comp = ggplot(crime) + 
  geom_bar(mapping = aes(crime$YEARSTAMP, fill= crime$CHRGDESC), position = "stack")

# title='Total Gunshot Wounds in Durham by Type, Jan. 2013 - Aug. 2017',
comp = comp + 
  labs(title='Total Gunshot Wounds in Durham by Type, Jan. 2013 - Aug. 2017', subtitle="2017 data incomplete, only through August", x="Year", y="Number of Gunshot Wounds") + 
  scale_x_discrete(name = "Year", limits = c(2013, 2014, 2015, 2016, 2017)) + 
  guides(fill=guide_legend(title = "Charge"))

plot
bar
comp
