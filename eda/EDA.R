


ggplot(data = df, aes(x = as.factor(EVENTMSGTYPE),
                      y = timeInPaint) ) +
     
     geom_violin()


quants <- quantile(df$timeInPaint, probs = seq(0, 1, .01)) 

three_rates <- NULL

for (q in quants) {
     
     sub_df <- df %>% 
          filter(timeInPaint <= q)
     
     len <- nrow(sub_df)
     n_makes <- filter(sub_df, EVENTMSGTYPE == 1) %>% nrow()
     
     rate <- n_makes / len
     
     three_rates <- bind_rows(three_rates, 
                              data.frame(timeInPaint = q, rate = rate))
     
}

ggplot(data = three_rates,
       aes(x = timeInPaint,
           y = rate)) +
     geom_point() +
     labs(x = "X or Fewer Seconds in Paint",
          y = "3 Pt Percentage") +
     theme_bw(base_size = 20)

###
get_3pt_rate <- function(df, dependent, dx, degs) {
     
     check <- df %>% 
          select_(dependent, 'EVENTMSGTYPE') %>% 
          arrange_(dependent) %>% 
          mutate(make = ifelse(EVENTMSGTYPE == 1, 1, 0),
                 cumMakes = cumsum(make),
                 cumTakes = row_number() )
     
     interp_points <- seq(1, 24, dx)
     
     makes_fill <- smooth.spline(x = check[,paste(dependent)], 
                                 y = check$cumMakes, 
                                 df = degs) %>% 
          predict(interp_points) %>% 
          data.frame()
     
     takes_fill <- smooth.spline(x = check[,paste(dependent)],
                                 y = check$cumTakes,
                                 df = degs) %>% 
          predict(interp_points) %>% 
          data.frame()
     
     final <- makes_fill %>% 
          left_join(takes_fill, by = 'x') %>% 
          rename(cumMakes = y.x,
                 cumTakes = y.y) %>% 
          mutate(cumMakes_ = lag(cumMakes),
                 cumTakes_ = lag(cumTakes),
                 threePct = (cumMakes - cumMakes_) / (cumTakes - cumTakes_) )
     
     plot(final$x, final$threePct)
     
}






