plot_population_data <-
function(df,
                                     totals = TRUE,
                                     title  = 'Jonglei State has the largest population'
                                       ) {

      if (totals) {

        df %>%

          # Initiliaze the canvas
          ggplot(aes(State, Population)) +

          # Add the bars
          geom_col(fill = "#4caf50") +

          # Add the title, x-axis title, and y-axis title.
          labs(
            title    = {title},
            x        = '',
            y        = 'Population') +

          # Adjust the y-axis scale.
          scale_y_continuous(labels = scales::number_format(scale = 1e-6, suffix = 'M')) +

          # Modify the y-axis limits.
          expand_limits(y = c(0, 1500000)) +

          # Adjust graph attributes.
          theme(
            plot.title          = element_text(hjust  = 0.5,
                                               margin = margin(0, 0, 15, 0, unit = "pt")),
            plot.subtitle       = element_text(hjust  = 0.5,
                                               margin = margin(2, 0, 10, 0, unit = "pt")),
            axis.text.x         = element_text(angle  = 30,
                                               hjust  = 1),
            axis.ticks.x        =  element_blank(),
            panel.grid.major.x  = element_blank()
          )

      } else {
        
        df %>%

          # Initiliaze the canvas
          ggplot(aes(State, Population)) +

          # Add the bars
          geom_col(aes(fill = Gender)) +

          # Add the title, x-axis title, and y-axis title.
          labs(
            title    =  {title},
            x        = '',
            y        = 'Population') +

          # Adjust the y-axis scale.
          scale_y_continuous(labels = scales::number_format(scale = 1e-6, suffix = 'M')) +

          # Modify the y-axis limits.
          expand_limits(y = c(0, 1500000)) +

          # Adjust graph attributes.
          theme(
            plot.title          = element_text(hjust  = 0.5,
                                               margin = margin(0, 0, 15, 0, unit = "pt")),
            plot.subtitle       = element_text(hjust  = 0.5,
                                               margin = margin(2, 0, 10, 0, unit = "pt")),
            axis.text.x         = element_text(angle  = 30,
                                               hjust  = 1),
            axis.ticks.x        =  element_blank(),
            panel.grid.major.x  = element_blank()
          )
      }
    }
compute_population_sums <-
function(df, totals = TRUE, ...) {
  
  # Data grouping columns
  grouping_vars_expr <- quos(...)
  
  if(totals) {
    totals <- df %>% 
      group_by(!!!grouping_vars_expr) %>% 
      summarize(Population = sum(Population),
                .groups    = "drop") %>% 
      
      arrange(desc(Population))
    
    totals
  } else {
    data <- df %>% 
      group_by(!!!grouping_vars_expr) %>% 
      summarize(Population = sum(Population),
                .groups    = "drop")
    
    data
  }
 
}
