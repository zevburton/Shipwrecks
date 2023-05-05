# comparison

comparison <- data.frame(Model = c("Logistic", "Decision Tree", "Random Forest", "SVM", "One-Layer NN", "Two-Layer NN", "Three-Layer NN"),
                         Accuracy = c(.9959, .9878, .9985, .9959, .9973, .9981, .9972),
                         AUC = c(NA, .6135, .945, NA, .9879, .9888, .9888))

library(kableExtra)

# Format the comparison dataframe using kable
kable(comparison, "html",
      align = "c",
      col.names = c("Model", "Accuracy", "AUC")) %>%
  kable_classic(latex_options = "striped", full_width = F) %>% 
  column_spec(1, border_right = T) %>%
  row_spec(c(3, 6), color = "black", background = "lightgreen") %>%
  row_spec(0, bold = T)

