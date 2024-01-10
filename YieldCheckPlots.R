library(esquisse)
esquisser()




library(dplyr)
library(ggplot2)

dat.lp %>%
 filter(set_id %in% c("Q4", "Q1", "Q2", "Q3")) %>%
 filter(above_ground_dry_yield >= 0.5 & above_ground_dry_yield <= 
 5 | is.na(above_ground_dry_yield)) %>%
 filter(Score >= 0L & Score <= 1L | is.na(Score)) %>%
 filter (z <= 605 | is.na(z)) %>%
 filter(Edge %in% "0") %>%
 #filter(Score2 >= 6L & Score2 <= 30L | 
 #is.na(Score2)) %>%
 ggplot() +
 aes(x = set_id, y = above_ground_dry_yield, fill = set_id) +
 geom_boxplot(shape = "circle") +
 scale_fill_viridis_d(option = "viridis", direction = 1) +
 theme_minimal()

dat.lp %>%
  filter(set_id %in% c("Q4", "Q1", "Q2", "Q3")) %>%
  filter(above_ground_dry_yield >= 0.5 & above_ground_dry_yield <= 
           5 | is.na(above_ground_dry_yield)) %>%
  ggplot() +
  aes(x = set_id, y = above_ground_dry_yield, fill = set_id) +
  geom_boxplot(shape = "circle") +
  scale_fill_viridis_d(option = "viridis", direction = 1) +
  theme_minimal()


library(ggplot2)

dat.lp %>%
 filter(set_id %in% c("Q3", "Q4", "Q1", "Q2")) %>%
 filter(above_ground_dry_yield >= 0.5 & above_ground_dry_yield <= 
 4.5 | is.na(above_ground_dry_yield)) %>%
 filter(Score >= 0L & Score <= 1L | is.na(Score)) %>%
 filter( z <= 605 | is.na(z)) %>%
 filter(Edge %in% "0") %>%
 ggplot() +
 aes(x = height, y = above_ground_dry_yield, colour = set_id) +
 geom_point(shape = "circle", 
 size = 3L) +
 scale_color_brewer(palette = "Spectral", direction = 1) +
 theme_minimal()



 
 ggplot(dat.lp) +
   aes(x = row, y = range, fill = z) +
   geom_tile(size = 1.5) +
   scale_fill_distiller(palette = "Spectral", 
                        direction = -1) +
   theme_minimal()
 

dat.lp %>%
 filter(above_ground_dry_yield >= 0.6 & above_ground_dry_yield <= 4 | is.na(above_ground_dry_yield)) %>%
 ggplot() +
 aes(x = row, y = range, fill = above_ground_dry_yield) +
 geom_tile(size = 1.5) +
 scale_fill_viridis_c(option = "viridis", 
 direction = 1) +
 theme_minimal()


