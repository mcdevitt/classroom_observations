require(numbers)
require(gridGraphics)
require(ggplot2)
require(ggthemes)
require(cowplot)
require(gplots)
#####
#Publications citing Protocol
#####
set.seed(112)
data <- matrix(c(
  c(499, 238, 50, 20, 30, 6, 6, 1),
  c(100, 70, 23, 15, 3, 0, 0, 0),
  c(180, 58, 4, 2, 0, 0, 0, 0),
  c(20, 5, 2, 1, 1, 0, 0, 0)
) , byrow = T, nrow = 4)
colnames(data) <-
  c("RTOP",
    "COPUS",
    "BERI",
    "UTOP",
    "TDOP",
    "RIOT",
    "STROBE",
    "3D-LOP")
rownames(data) <-
  c("Journal",
    "Book/Chapter",
    "Dissertation/Thesis",
    "Conference Proceeding")


# Get the stacked barplot
barplot(
  data,
  col = c('darkorange1', 'lightskyblue1', 'springgreen4', 'yellow'),
  border = "white",
  space = 0.04,
  font.axis = 2,
  ylim = c(0, 800),
  xlab = "Observation Protocol",
  ylab = "Number of sources",
  main = 'Placeholder (totals approximate, proportions simulated)'
)
legend(
  "topright",
  inset = .02,
  title = "Publication Type",
  c(
    "Journal",
    "Book/Chapter",
    "Dissertation/Thesis",
    "Conference Proceeding"
  ),
  fill = c('darkorange1', 'lightskyblue1', 'springgreen4', 'yellow'),
  horiz = F,
  cex = 1
)

#####
#Temporal resolution
#####
class.length <- 60 * 60 #60 minute course represented by second
divisible <- divisors(class.length) #find all divisible numbers


#generate sequences 60 min
generate.seq <- function(target.length = 1,
                         prob = 0.35) {
  reps <- rbinom(3700, 1, prob = prob)
  poisson <- rpois(3700, target.length)
  seq <- unlist(sapply(1:length(poisson), function(l) {
    rep(reps[l], poisson[l])
  }))
  seq[1:3600]
}


#Ploting function for frequent code switching
time.plot <- function(fill = seq.0, title = 'High rate of change') {
  p1 <- qplot(
    x = rep(1:60, 60),
    y = rep(1:60, each = 60),
    fill = as.factor(fill),
    color = 'black',
    geom = 'tile'
  ) +
    labs(x = "Second", y = "Minute") +
    scale_y_reverse(
      lim = c(62, 0),
      breaks = c(1, 11, 21, 31, 41, 51, 61),
      expand = c(0, 0),
      labels = c('0 ', '10', '20 ', '30', '40', '50', '60')
    ) +
    scale_x_continuous(
      lim = c(0, 61),
      breaks = c(1, 11, 21, 31, 41, 51, 61),
      expand = c(0, 0),
      labels = c('0 ', '10', '20 ', '30', '40', '50', '60')
    ) +
    scale_fill_manual(
      name = "Code",
      labels = c('not active', 'active'),
      values = c('white', 'black')
    ) +
    scale_colour_manual(
      guide = FALSE,
      name = 'Border',
      labels = c('default'),
      values = c('black', 'black')
    ) +
    scale_alpha(guide = FALSE) +
    ggtitle(paste(title)) +
    theme_par(base_size = 12) +
    theme(
      legend.position = 'right',
      plot.margin = unit(c(1, 3.5, 1, 1), "cm"),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      legend.key = element_rect(
        colour = 'black',
        fill = 'white',
        linetype = 'solid'
      )
    )
  
  per.list <- lapply(1:length(divisible), function(d) {
    temp.seq <- matrix(fill, ncol = divisible[d], byrow = T)
    mean(rowSums(temp.seq) >= 1)
  })
  plot(
    divisible,
    per.list,
    ylim = c(0, 1),
    xlim = c(0, 300),
    xlab = 'Observation window (seconds)',
    ylab = "Percent of class period"
  )
  abline(v = c(1, 10, 60, 120),
         col = 'red',
         lty = 2)
  abline(h = mean(fill), lty = 2)
  p2 <- recordPlot()
  plot_grid(p1, p2, rel_widths = c(2, 1), labels = "AUTO")
}
seq.0 <- generate.seq(1)
seq.1 <- generate.seq(10)
seq.2 <- generate.seq(60)
seq.3 <- generate.seq(120)
seq.4 <- generate.seq(160)

oi.1 <- time.plot(seq.0, "Behavior duration: 1 second")
oi.2 <- time.plot(seq.1, "Behavior duration: 10 second")
oi.3 <- time.plot(seq.2, "Behavior duration: 60 second")
oi.4 <- time.plot(seq.3, "Behavior duration: 120 second")
oi.5 <- time.plot(seq.4, NULL)
pdf('obs.interval.pdf', width = 30, height = 50)
par(mfrow = c(5, 1))
oi.1
oi.2
oi.3
oi.4
oi.5
dev.off()



#####
#Inter-rater reliability
#####
set.seed(12)
stripchart(
  x = as.vector(replicate(4, sample(
    seq(0.6:1, by = 0.01), 20, replace = T
  ))) ~ rep(c('RTOP', 'COPUS', 'BERI', 'UTOP'), each = 20),
  xlim = c(0, 1),
  xlab = "Inter-rater reliability (IRR)",
  ylab = 'Observation Protocol',
  main = 'Placeholder (simulated data)'
)


sample(seq(0.6:1, by = 0.01), 20, replace = T)

prop<- seq(1,0, by = 0.01)
irr <- seq(0,1, by = 0.01)

abs(rep(prop, each=101)-rep(2*prop*(1-irr),101))


comb <- matrix(abs(rep(prop, 101)-rep(2*prop*(1-irr),each=101)), nrow = 101, byrow = F)
rownames(comb) <-irr
colnames(comb) <-prop

heatmap(comb, Rowv = NA, Colv = NA, xlab = 'IRR', ylab = 'Code frequency', main = "Placeholder", symm = T) # _NO_ reorder()



1-pnorm(1, .8, .15)

d.mean <- 0.8
d.sd <- 0.15
distribution <- rnorm(100000, d.mean, d.sd)
plot(density(distribution), xlab = "Code frequency", 
     main = "Normal distribution where mean = 0.8 and sd = 0.1",
     xlim=c(0,max(distribution)))
abline(v = 1, col = 'red')


