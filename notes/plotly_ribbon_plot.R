library(ggplot2)
library(plotly)
df <- 
  expand_grid(
    x = as.double(1:9),
    z = c("A", "B", "C")
) |> 
  mutate(y = rnorm(n()))

colors <-
  c("A" = "#378DBD", "B" = "#001C48", "C" = "#AB0520")

p <- 
  ggplot(df, aes(x = x, y = y, color = z)) +
  geom_line() +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors)
ggplotly(p)


library(ggplot2); library(plotly); library(dplyr)
df = data.frame(x = 1:5, y = sin(1:5), min = sin(1:5) + 1, max = sin(1:5)-1) %>%
  rbind(c(6,sin(6),NA, NA))
pl = ggplot(df, aes(x=x, y=y)) + 
  geom_line()
pl
ggplotly(pl) |> 
  add_ribbons(x = ~x, y = ~y, ymin = ~min, ymax = ~max, connectgaps = TRUE)
  
