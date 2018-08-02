### Libraries ----

library(ggplot2)

### Create functions for MSY plot ----

msy = function(x) {
  (0.1 * x) * (1 - (x/10000))
  } 


bio <- function(x) {
  -0.1 * x + 1000
}

#### Plot ----


msy.plot <- ggplot(data.frame(x=c(1, 10000)), aes(x=x)) + 
  stat_function(fun=bio, geom= "line")

a <- grid::convertWidth(unit(1,'npc'), 'inch', TRUE)
b <- grid::convertHeight(unit(1,'npc'), 'inch', TRUE)


msy.plot +
  stat_function(fun=msy, geom= "line", linetype = 'longdash') +
  scale_y_continuous(limits = c(0, 1000)) +
  xlab("Fishing Mortality Rate") + 
  ylab("Biomass") +
  annotate(geom = "text", x = 5000, y = 500, label = "Fish Abundance", angle = (atan(a/b) * 180/pi) + 270, vjust = -1) +
  annotate(geom = "text", x = 5000, y = 250, label = "Sustainable Yield", vjust = -1) +
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid = element_blank())


### Create function for vB growth plot ----

growth <- function(x) {
  Linf <- 700    # controlls the asymptotic max_length; maximum MEAN length
  K <- 0.15      # controls the steepness of the curve
  t0 <- -1       # x-intercept
  Linf*(1-exp(-K*(x - t0)))
}

#### Plot ----

ggplot(data.frame(x = c(1:25)), aes(x=x)) +
  stat_function(fun = growth) 
                
