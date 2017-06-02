#manuscript figures 
#both walleye and bass projections
#11/17/15
library(ggplot2)
library(reshape2)
library(dplyr)
library(scales)
library(grid)


##############################
#plot bass and walleye relationship to GDD together on the same figure
gdd=read.csv("data/partial_plot_wae_GDD_median_quartiles.csv",header=T)
gdd.bass=read.csv("data/partial_plot_lmb_GDD_median_quartiles2.csv", header=T)
gdd.bass$Species="LMB"
gdd$Species="Walleye"
colnames(gdd)[1]="GDD"
colnames(gdd.bass)[1]="GDD"
gdd.bass=select(gdd.bass, -fifth, -ninetyfifth)
gdd.both=rbind(gdd, gdd.hab)

text.data=data.frame(x=c(2250,2900),
                     y=c(.65, .95),
                     Species=c("Walleye", "HAB"),
                     Species.name=c("Walleye", "Harmful Algal Bloom"))


windows()
z4<-ggplot(gdd.both)
z4=z4+geom_ribbon(aes(GDD, ymin=twentyfifth, ymax=seventyfifth, fill=Species),   alpha=.25)
z4=z4+geom_path(aes(GDD, median, colour=Species), lwd=1)
#z4=z4+scale_y_continuous(lim=c(0,.8), breaks=c(0,.2,.4,.8))
z4=z4+scale_x_continuous(breaks=c(2200,2500,2800, 3100), lim=c(2140, 3200))
z4=z4+xlab(expression(bold(atop("Degree days",(~degree~C%.%days)))))+ylab("Probability")+theme_bw()
z4=z4+theme( axis.title=element_text(size=11, face="bold"),
             axis.text=element_text(size=11, margin=unit(c(1,1,1,1), "cm")),
             axis.line=element_line(colour="black"),
             axis.ticks.length=unit(-0.08, "cm"),
             panel.grid.major = element_line(colour=NA),
             panel.grid.minor = element_line(colour = NA),legend.position="none",
             panel.background = element_rect(colour = NA)
)
z4=z4+geom_text(data=text.data, aes(x, y, label=Species.name, colour=Species), size=3.5)

print(z4)
ggsave('GDDeffect_bass_walleye.tiff', height=80, width=80, units="mm", dpi=300)
###########################################################

# generate random data for hypothetical algal biomass data
gdd.hab <- data.frame(GDD = gdd$GDD,
                      median = NA, 
                      twentyfifth = NA, 
                      seventyfifth = NA,
                      Species = 'HAB')
stand.x <- scale(gdd.hab$GDD)
seq.ran <- 

for (i in seq(from = 1, to = nrow(gdd.hab), by = 1)){
  n <- sample(1:5, 1)
  sample.b <- rnorm(100, mean = 2, sd = 1)
  y <- 1/(1+exp(-1*sample.b*stand.x[i]))
  gdd.hab$median[i] <- median(y)
  gdd.hab$twentyfifth[i] <- gdd.hab$median[i] - rnorm(1, .2, .05)
  gdd.hab$seventyfifth[i] <- gdd.hab$median[i] + rnorm(1, .2, .05)
}
