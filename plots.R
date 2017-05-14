# All NPV curves
ggplot(results, aes(x = pv_array_size, y=npv, col=as.factor(demand_index))) + geom_line() + geo

results$band <- factor(results$band, levels = 1:5, labels = c("DA", "DB", "DC", "DD", "DE"))


results_mean <- results %>%
  group_by(pv_array_size, band) %>%
  summarize(
    index = 0,
    npv_05 = quantile(npv, 0.05),
    npv_50 = quantile(npv, 0.50),
    npv_95 = quantile(npv, 0.95),
    npv_mean = mean(npv),
    solar_energy_demand_ratio_05 = quantile(solar_energy_demand_ratio, 0.05),
    solar_energy_demand_ratio_50 = quantile(solar_energy_demand_ratio, 0.50),
    solar_energy_demand_ratio_95 = quantile(solar_energy_demand_ratio, 0.95),
    solar_energy_demand_ratio_mean = mean(solar_energy_demand_ratio),
    count = n()) %>%
  inner_join(by="band",
    data.frame(
      band = factor(c("DA", "DB", "DC", "DD", "DE")),
      pv_array_size_display_limit = c(6, 17, 30, 50, 65))) %>%
  filter(pv_array_size <= pv_array_size_display_limit)


# NPV curves for a small sample
ggplot(results, aes(x = pv_array_size, y=npv, group=as.factor(index), col = band)) + 
  geom_line(size = 0.5, alpha = 0.2) + 
  theme_bw() + 
  coord_cartesian(ylim = c(-6000, 6000)) +
  facet_grid(. ~ band, scales = "free_x") +
  theme(legend.position='none')
#' Plot NPV curves in greyscale
ggplot(results, aes(x = pv_array_size, y=npv, group=as.factor(index))) + 
  geom_line(size = 0.2, alpha = 0.2) + 
  theme_bw() + 
  coord_cartesian(ylim = c(-6000, 6000)) +
  facet_grid(. ~ band, scales = "free_x") +
  theme(legend.position='none') +
  geom_line(data = results_mean, aes(x = pv_array_size, y=npv_50), size=1, col="red") +
  geom_line(data = results_mean, aes(x = pv_array_size, y=npv_05), size=1, col="blue") +
  geom_line(data = results_mean, aes(x = pv_array_size, y=npv_95), size=1, col="blue") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Number of PV Modules (each 245Wp)", y = "Net Present Value (€)")


# Sir for the band 1
ggplot(filter(results, band == 1), aes(x = pv_array_size, y=sir, group=as.factor(index), col=as.factor(band))) + 
  geom_line(size = 0.8, alpha = 0.5) + 
  theme_bw() + facet_grid(band ~ .) +
  theme(legend.position='none')


# solar energy demand ratio plot
ggplot(results, aes(x = pv_array_size, y=solar_energy_demand_ratio, group=as.factor(index), col=band)) + 
  geom_line(size = 0.5, alpha = 0.5) + 
  facet_grid(. ~ band, scales = "free_x") +
  theme_bw() + 
  theme(legend.position='none')



ggplot(results, aes(x = pv_array_size, y=solar_energy_demand_ratio, group=as.factor(index))) + 
  geom_line(size = 0.5, alpha = 0.1) + 
  facet_grid(. ~ band, scales = "free_x") +
  theme_bw() + 
  theme(legend.position='none') +
  geom_line(data = results_mean, aes(x = pv_array_size, y=solar_energy_demand_ratio_50), size=1, col="red") +
  geom_line(data = results_mean, aes(x = pv_array_size, y=solar_energy_demand_ratio_05), size=1, col="blue") +
  geom_line(data = results_mean, aes(x = pv_array_size, y=solar_energy_demand_ratio_95), size=1, col="blue") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), labels = scales::percent) +
  labs(x = "Number of PV Modules (each 245Wp)", y = "PV Output:Electricity Demand Ratio")


# Distribution of the optimal NPV
# weighted
df <- results %>%
  group_by(index) %>%
  summarize(
    npv_max = max(npv),
    npv_min = min(npv)) %>%
  inner_join(results, by="index") %>%
  mutate(
    weight = (npv - npv_min) / (npv_max - npv_min) / n())

# not-weighted
df <- results %>%
  group_by(index) %>%
  summarize(
    band = first(band),
    pv_array_size = which.max(npv)) %>%
  mutate(
    weight = 1)
  
df <- results %>%
  group_by(band, pv_array_size) %>%
  summarize(
    npv_mean = mean(npv),
    npv_sd = sd(npv),
    a = n())
ggplot(df, aes(x = pv_array_size, npv_mean)) +
  geom_point(size = 3, col="black") + 
  geom_line(col="black") +
  geom_errorbar(aes(ymin = npv_mean - npv_sd, ymax = npv_mean + npv_sd), width=0.25)+
  facet_grid(band ~ ., scales = "free_y") +
  scale_x_continuous(breaks = seq(0,100,5))


ggplot(df, aes(x = pv_array_size, weight=weight, fill=factor(1))) + 
  geom_histogram(fill = "grey50", color = "black") +
  scale_fill_grey() +
  theme_bw() + 
  facet_grid(band ~ ., scales = "free_y") +
  scale_x_continuous(breaks = seq(0,100,5)) +
  theme(legend.position='none') +
  labs(x = "Number of PV Modules (each 245Wp)", y = "Frequency") +
  scale_y_continuous(labels = scales::comma)
  
# Optimal pv_array_size: 3, 6, 12, 20, 61
df_summary <- df %>%
  group_by(band, pv_array_size) %>%
  summarize(weight_sum = sum(weight)) 



# solar energy demand ratio change plot
# This plot shows initial linear drop which becomes almost exponential
# after reaching certain threshold
results %>%
  group_by(index) %>%
  mutate(solar_energy_demand_ratio_change = solar_energy_demand_ratio - lag(solar_energy_demand_ratio)) %>%
  ggplot(aes(x = pv_array_size, y=solar_energy_demand_ratio_change, group=as.factor(index), col=band)) + 
    geom_line(size = 0.5, alpha = 0.5) + 
    facet_grid(. ~ band, scales = "free_x") +
  theme_bw() + 
  theme(legend.position='none')
# Roof distribution
hist_top <- 
  ggplot(roofs, aes(x = Area)) + geom_histogram() + coord_cartesian(xlim=c(0, 125)) +
  theme(axis.title.x=element_blank()) +
  labs(y = "Frequency") +
  theme_no_x

hist_right <- 
  ggplot(roofs, aes(x = Angle)) + geom_histogram() + 
  theme(
    axis.title.y=element_blank(),
    axis.title.x=element_text(margin=margin(15,0,0,0))) + 
  coord_flip(xlim = c(20, 50)) + scale_x_continuous(limits = c(20, 50)) +
  labs(y = "Frequency") +
  theme_no_y

empty <- ggplot() + geom_point(aes(1,1), colour="white") + theme_void()
scatter <- 
  ggplot(roofs, aes(x = Area, y = Angle)) + geom_point(size=2) + coord_cartesian(xlim=c(0, 125), ylim=c(20, 50)) + 
  labs(x = expression(Area~(m^2)), y = "Pitch (degree)")
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
  geom_histogram(binwidth = 500, color="black", fill="grey50") + 
  coord_cartesian(xlim=c(0, 30000)) +
  scale_x_continuous(labels = scales::comma) +
  labs(x = "Total Annual Electricity Consumption (kWh)", y = "Frequency") +
  theme_bw()

#-------------------------------------------
Sys.setlocale("LC_TIME", "English")
Sys.setenv(TZ='GMT')
#save(balance_yearly, file = "example_balance_yearly.RData")
load("example_balance_yearly.RData")
df <- balance_yearly
df$date <- seq(as.POSIXct("2009-01-01 00:00"), as.POSIXct("2009-12-31 23:30"), by = 30 * 60)
df <- df %>% filter(date > "2009-05-02" & date <"2009-05-09")

scale_x_custom <- scale_x_datetime(
  breaks = seq(as.POSIXct("2009-05-02 12:00"), as.POSIXct("2009-05-08 12:00"), by = 24 * 60 * 60),
  minor_breaks = seq(as.POSIXct("2009-05-02 00:00"), as.POSIXct("2009-05-09 00:00"), by = 24 * 60 * 60),
  labels = date_format("%B %d", tz = "GMT"))

theme_custom <- theme_bw() + theme(
  legend.position = "bottom", 
  axis.title.x = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_line(colour = "grey90", size = 0.2),
  axis.ticks.x = element_blank(),
  axis.title.y=element_text(margin=margin(0,20,0,0)))

theme_no_x <- theme(
  axis.ticks.x = element_blank(),
  axis.title.x = element_blank(), 
  axis.text.x = element_blank())

theme_no_y <- theme(
  axis.ticks.y = element_blank(),
  axis.title.y = element_blank(), 
  axis.text.y = element_blank())


df_long <- melt(df, id.vars="date", measure.vars=c("pv_array_energy_output", "energy_demand_profile"))
attr(df_long$variable, "levels") <- c("PV Output", "Electricity Demand")


p1 <- 
  ggplot(df_long, aes(x=date, y=value, fill=variable)) + 
  geom_area(position = "identity") +
  geom_line(col="black") +
  scale_fill_manual(values=c( "lightgrey", "darkgrey")) +
  theme(legend.position = "bottom") +
  scale_x_custom +
  theme_custom +
  theme(legend.position = c(.9, .85), legend.title = element_blank()) +
  labs(y = "Electricity Demand\nand PV Output (kW)")


p2 <- 
  ggplot(df, aes(x=date, y=battery_percentage / 100)) + 
  geom_area(alpha = 0.2, position = "identity") +
  geom_line() +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_custom +
  theme_custom +
  labs(y = "Battery Charge\n(%)")


p3 <- 
  ggplot(df, aes(x=date, y=energy_diff * 2)) + 
  geom_area(alpha = 0.2, position = "identity") +
  geom_line() +
  scale_x_custom +
  theme_custom +
  labs(y = " Electricity Imported [+]\nand Exported [-] (kW)")

grid.newpage()
grid.draw(rbind(ggplotGrob(p1 + theme_no_x), ggplotGrob(p2 + theme_no_x), ggplotGrob(p3), size = "last"))

# Exported to PDF as 12x10

ggplot(df, aes(x = date, y = pv_array_energy_output)) + geom_line()
ggplot(df, aes(x = date, y = energy_demand_profile)) + geom_line()


calculate_yearly_energy_balance(
  energy_demand_profile = energy_demand_profiles[, 1],
  pv_array_energy_output = )


# -----------------------
# Building example
#save(results, file="example_building_results.RData")
load("example_building_results.RData")
p1 <-
  ggplot(results, aes(x = pv_array_size, y=npv, group=as.factor(index), col=as.factor(band))) + 
  geom_point(size = 3, col="black") + 
  geom_line(col="black") +
  theme_bw() +
  scale_y_continuous(limits = c(-5000, -3500), labels = scales::comma) +
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, 2)) +
  labs(x = "Number of PV Modules (each 245Wp)", y = "Net Present Value (€)") +
  theme(axis.title.y=element_text(margin=margin(0,20,0,0)))

p2 <- 
  ggplot(results, aes(x = pv_array_size, y=solar_energy_demand_ratio, group=as.factor(index), col=as.factor(band))) + 
  geom_point(size = 3, col="black") + 
  geom_line(col="black") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), labels = scales::percent) +
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, 2)) +
  labs(x = "Number of PV Modules (each 245Wp)", y = "Utilization Ratio") +
  theme(axis.title.y=element_text(margin=margin(0,20,0,0)))

grid.newpage()
grid.draw(rbind(ggplotGrob(p1 + theme_no_x), ggplotGrob(p2), size = "last"))
#PDF 12x6.6