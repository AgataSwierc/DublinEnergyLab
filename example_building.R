a <- sample(which(energy_demand_profiles_bands_map == 4), 10)

df <- data.frame(value = energy_demand_profiles[[a[2]]]) 
df$date <-seq(as.POSIXct("2009-01-01 00:00"), as.POSIXct("2009-12-31 23:30"), by = 30 * 60)
df <- df %>% filter(date > "2009-12-14" & date <"2009-12-20")

p <- 
  ggplot(df, aes(x=date, y=value * 2)) + 
  geom_area(position = "identity", fill = "grey50") +
  geom_line(col="black") +
  scale_x_custom +
  theme_custom +
  labs(y = "(kW)")

print(p)
print(a[2])

df <- data.frame(
  D = energy_demand_profiles[1463],
  A = energy_demand_profiles[929],
  #B = energy_demand_profiles[1837],
  #C = energy_demand_profiles[2931],
  A = energy_demand_profiles[454])
df$date <-seq(as.POSIXct("2009-01-01 00:00"), as.POSIXct("2009-12-31 23:30"), by = 30 * 60)

df <- df %>% filter(date > "2009-12-14" & date <"2009-12-21")
df_long <- melt(df, id.vars="date")

ggplot(df_long, aes(x=date, y=value * 2)) + 
  geom_area(position = "identity", fill = "grey50") +
  geom_line(col="black") +
  theme(legend.position = "bottom") +
  scale_x_datetime(
    breaks = seq(as.POSIXct("2009-12-14 12:00"), as.POSIXct("2009-12-20 12:00"), by = 24 * 60 * 60),
    minor_breaks = seq(as.POSIXct("2009-12-14 00:00"), as.POSIXct("2009-12-21 00:00"), by = 24 * 60 * 60),
    labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
  theme_custom +
  theme(legend.position = c(.9, .85), legend.title = element_blank()) +
  labs(y = "Electricity Demand (kW)") +
  facet_grid(variable ~ .) +
  coord_cartesian(ylim = c(0, 6)) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())

# 929 - wszysktie dni ska takie same
# 1837 takie same
# 2931
# 1463 weekend
# 454
# 255
