# All NPV curves
ggplot(results, aes(x = pv_array_size, y=npv, col=as.factor(demand_index))) + geom_line() + geom_point()

ggplot(filter(results_old, index == 1), aes(x = pv_array_size, y=npv, col=as.factor(demand_index))) + geom_line() + geom_point()


# NPV curves for a small sample
7


# Sir for the band 1
ggplot(filter(results, band == 1), aes(x = pv_array_size, y=sir, group=as.factor(index), col=as.factor(band))) + 
  geom_line(size = 0.8, alpha = 0.5) + 
  theme_bw() + facet_grid(band ~ .)


# solar energy demand ratio plot
ggplot(head(results, 2000), aes(x = pv_array_size, y=solar_energy_demand_ratio, group=as.factor(index), col=as.factor(band))) + 
  geom_line(size = 0.8, alpha = 0.5) + 
  theme_bw() + facet_grid(band ~ .)


# Distribution of the optimal NPV
results %>%
  group_by(index) %>%
  summarize(
    npv_max = max(npv),
    npv_min = min(npv)) %>%
  inner_join(results, by="index") %>%
  mutate(
    weight = (npv - npv_min) / (npv_max - npv_min)) %>%
  ggplot(aes(x = pv_array_size, fill=as.factor(band), weight=weight, alpha=0.5)) + 
  geom_density() +
  theme_bw() + 
  facet_grid(band ~ .)


# Roof distribution
library("gridExtra")

hist_top <- ggplot(roofs, aes(x = Area)) + geom_histogram() + coord_cartesian(xlim=c(0, 125)) + theme(axis.title.x=element_blank())
hist_right <- ggplot(roofs, aes(x = Angle)) + geom_histogram() + theme(axis.title.y=element_blank()) + coord_flip(xlim = c(20, 50)) + scale_x_continuous(limits = c(20, 50))
empty <- ggplot() + geom_point(aes(1,1), colour="white") + theme_void()
scatter <- 
  ggplot(roofs, aes(x = Area, y = Angle)) + geom_point(size=2) + coord_cartesian(xlim=c(0, 125), ylim=c(20, 50)) + 
  labs(x = "Area (m^2)", y = "Pitch (degree)")
grid.arrange(hist_top, empty, scatter, hist_right, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))

# This is how you can do superscript in labels
#x = expression(paste("Area (", m^2, ")", sep="")),

# Alternative version is geom_rug, but it does not look as nice.
ggplot(roofs, aes(x = Area, y = Angle)) + geom_point() +
  geom_rug()



#-------------------------------------------
#' Demand profiles histogram
df <- data.frame(x = energy_demand_profiles_sums)
ggplot(df, aes(x = x)) + 
  geom_histogram(binwidth = 500) + 
  coord_cartesian(xlim=c(0, 30000)) +
  labs(x = "Total yearly demand (kWh)") +
  theme_bw()

#-------------------------------------------
Sys.setlocale("LC_TIME", "English")
#save(balance_yearly, file = "example_balance_yearly.RData")
load("example_balance_yearly.RData")
df <- balance_yearly
df$date <- seq(as.POSIXct("2009-01-01 00:00"), as.POSIXct("2009-12-31 23:30"), by = 30 * 60)
df <- df %>% filter(date > "2009-05-02" & date <"2009-05-09")

melt(df, id.vars="date", measure.vars=c("pv_array_energy_output", "energy_demand_profile")) %>%
  ggplot(aes(x=date, y=value, fill=variable)) + 
  geom_area(position = "identity") +
  geom_line(col="black") +
  theme_bw() +
  scale_fill_manual(values=c( "lightgrey", "darkgrey")) +
  theme(legend.position = "bottom") +
  scale_x_datetime(date_breaks = "1 day", date_labels="%B %d")

ggplot(df, aes(x=date, y=battery_percentage)) + 
  geom_area(alpha = 0.2, position = "identity") +
  geom_line() +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_datetime(date_breaks = "1 day", date_labels="%B %d")

ggplot(df, aes(x=date, y=energy_diff)) + 
  geom_area(alpha = 0.2, position = "identity") +
  geom_line() +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_datetime(date_breaks = "1 day", date_labels="%B %d")



ggplot(df, aes(x = date, y = pv_array_energy_output)) + geom_line()
ggplot(df, aes(x = date, y = energy_demand_profile)) + geom_line()


calculate_yearly_energy_balance(
  energy_demand_profile = energy_demand_profiles[, 1],
  pv_array_energy_output = )


# -----------------------
# Building example
#save(results, file="example_building_results.RData")
ggplot(results, aes(x = pv_array_size, y=npv, group=as.factor(index), col=as.factor(band))) + 
  geom_point(size = 3, col="black") + 
  geom_line(col="black") +
  theme_bw() +
  scale_y_continuous(limits = c(-5000, -3500), labels = scales::comma) +
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, 2)) +
  labs(x = "Number of PV Modules (each 245Wp)", y = "Net Present Value (€)")


ggplot(results, aes(x = pv_array_size, y=solar_energy_demand_ratio, group=as.factor(index), col=as.factor(band))) + 
  geom_point(size = 3, col="black") + 
  geom_line(col="black") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), labels = scales::percent) +
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, 2)) +
  labs(x = "Number of PV Modules (each 245Wp)", y = "PV Output: Electricity Demand Ratio")

