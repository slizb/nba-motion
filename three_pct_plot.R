
library(viridis)
library(dplyr)
library(ggplot2)

plot_3pct_bins <- function(model_df, dependent, n_bins, upper_bin) {
     
     makes_df <- model_df %>% 
          filter(target == 'Make') %>%  
          .[ .[[dependent]] < 24 , ]
     
     misses_df <- model_df %>% 
          filter(target == 'Miss') %>%  
          .[ .[[dependent]] < 24 , ]
     
     miss_hist <- hist(misses_df[[dependent]],
                       breaks = c(seq(0, upper_bin, l = n_bins), 24) ) 
     
     make_hist <- hist(makes_df[[dependent]], 
                       breaks = miss_hist$breaks)
     
     
     x_breaks <- make_hist$breaks %>%
          round(1) %>% 
          paste0(lag(.), '-', .) %>% 
          tail(., length(.) - 1)
     
     hist_df <- data.frame(misses = miss_hist$counts,
                           makes = make_hist$counts,
                           x = factor(x_breaks, 
                                      levels = unique(x_breaks))) %>% 
          mutate(threePct = makes / (makes + misses) )
     
     ggplot(data = hist_df,
            aes(x = x,
                y = threePct,
                fill = (makes + misses) )) +
          geom_bar(stat = 'identity') +
          theme_bw() +
          theme(axis.text.x=element_text(angle = 45, hjust = 1) ) + 
          scale_fill_viridis(option = 'inferno')
     
}

plot_3pct_bins(model_df, 'timeInPaint', n_bins = 30, upper_bin = 10)
