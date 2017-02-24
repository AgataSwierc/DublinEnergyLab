# All NPV curves
ggplot(results, aes(x = pv_array_size, y=npv, col=as.factor(demand_index))) + geom_line() + geo

results$band <- factor(results$band, levels = 1:5, labels = c("DA", "DB", "DC", "DD", "DE"))


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
  geom_line(data = results_mean, aes(x = pv_array_size, y=npv_95), size=1, col="blue")


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
  geom_line(data = results_mean, aes(x = pv_array_size, y=solar_energy_demand_ratio_95), size=1, col="blue")



# Distribution of the optimal NPV
df <- results %>%
  group_by(index) %>%
  summarize(
    npv_max = max(npv),
    npv_min = min(npv)) %>%
  inner_join(results, by="index") %>%
  mutate(
    weight = (npv - npv_min) / (npv_max - npv_min))
ggplot(df, aes(x = pv_array_size, weight=weight, fill=factor(1))) + 
  geom_density(alpha=0.5) +
  scale_fill_grey() +
  theme_bw() + 
  facet_grid(band ~ ., scales = "free_y") +
  scale_x_continuous(breaks = seq(0,100,5)) +
  theme(legend.position='none')
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
