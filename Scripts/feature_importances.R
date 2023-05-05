library(readr)
library(ggtext)

rf_importances <- read.csv("rf_importances.csv") %>% select(Feature, Importance)
nn_importances <- read.csv("2nn_importances.csv") %>% select(Feature, Importance)

nn_importances <- nn_importances %>%
  mutate(Importance = abs(Importance)) %>%
  arrange(-Importance) %>%
  select(Feature, Importance)

names(rf_importances) <- c("Feature", "Random Forest")
names(nn_importances) <- c("Feature", "Neural Network")

rf_importances$`Random Forest` <- scale(rf_importances$`Random Forest`)
nn_importances$`Neural Network`<- scale(nn_importances$`Neural Network`)

rf_plot <- rf_importances %>% filter(`Random Forest` > 0)
rf_plot <- rf_plot[c(1:3,5:8, 10:11),]
rf_plot$category <- c("Current", "Current", "Temperature", "Water", "Water", "Water", "Wind", "Wind", "Wind")

colors <- c("Current" = "#BF6B63", "Temperature" = "#D9A384",
            "Location" = "#A9D4D9", "Water" = "#5B9EA6", "Wind" = "#5A6868")
library(ggfx)
ggplot(rf_plot, aes(x = reorder(Feature, `Random Forest`), y = `Random Forest`, fill = category)) +
  geom_col(color = "black") +
  theme_minimal() +
  scale_fill_manual(values = colors) +
  labs(x = "",
       y = "Scaled Importance",
       title = "Importance of Features in the Random Forest Model") +
  scale_x_discrete(labels = c("North-South Wind",
                              "Wind Stress (Curl, 10m)",
                              "North-South Wind Stress",
                              "Potential Density",
                              "Surface Velocity",
                              "Humidity",
                              "Temperature",
                              "North-South Current",
                              "East-West Current")) +
  theme(text = element_text(family = "serif", size = 14),
        plot.background = element_rect(fill = "#e7f7fa"),
        legend.title = element_blank(),
        legend.position = c(.75,.3),
        legend.background = with_shadow(element_rect(), x_offset = 2, y_offset = 2)) +coord_flip()

nn_plot <- nn_importances %>% filter(`Neural Network` > 0)
nn_plot <- nn_plot[c(1:15,17:20),]
nn_plot$category <- c("Current", "Current", "Location", "Water", "Wind",
                      "Current", "Water", "Temperature", "Temperature", "Wind",
                      "Wind", "Wind", "Water", "Water", "Water", "Water", "Temperature",
                      "Water", "Water")
nn_plot$category <- factor(nn_plot$category)
nn_plot <- nn_plot %>% arrange(-`Neural Network`)


ggplot(nn_plot, aes(x = reorder(Feature, `Neural Network`), y = `Neural Network`, fill = category)) +
  geom_col(color = "black") +
  theme_minimal() +
  scale_fill_manual(values = colors) +
  labs(x = "",
       y = "Scaled Importance",
       title = "Importance of Features in the Neural Network Model") +
  scale_x_discrete(labels = rev(c("Surface Velocity",
                              "North-South Current",
                              "Longitude / Distance from Shore",
                              "Potential Density",
                              "Wind Magnitude (20m)",
                              "East-West Current",
                              "Ice Concentration",
                              "Humidity - Temperature",
                              "Temperature",
                              "East-West Wind Stress",
                              "Wind Stress (Curl, 20m)",
                              "Wind Speed",
                              "Latent Heat (East-West)",
                              "Latent Heat",
                              "Humidity",
                              "Water Pressure",
                              "Temperature Difference, Sea/Air",
                              "Heat Parameter",
                              "North-South Wind Stress"))) +
  theme(text = element_text(family = "serif", size = 14),
        plot.background = element_rect(fill = "#e7f7fa"),
        legend.title = element_blank(),
        legend.position = c(.75,.3),
        legend.background = with_shadow(element_rect(), x_offset = 2, y_offset = 2)) +coord_flip()
  

