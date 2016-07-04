# All NPV curves
ggplot(results, aes(x = pv_array_size, y=npv, col=as.factor(demand_index))) + geom_line()


# NPV curves for a small sample
ggplot(head(results, 2000), aes(x = pv_array_size, y=npv, group=as.factor(index), col=as.factor(band))) + 
  geom_line(size = 0.8, alpha = 0.5) + 
  theme_bw() + facet_grid(band ~ .)


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