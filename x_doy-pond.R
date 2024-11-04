## Temp file exploring doy and pond relationship

# plot data with smooths for each pond
ggplot(fyke99, aes(x = haul.date_jul, y = wfl_freq, color = pond)) +
  #geom_point() +
  geom_smooth(method = "loess", se = TRUE) +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 15)) +
  labs(x = "Day of Year", y = "WFL Frequency", color = "Pond") +
  ggtitle("WFL Frequency by Day of Year and Pond") +
  theme(legend.position = "bottom")
