# As I always do, I have the variables which you should replace with your own data in ALL CAPS

# This is a custom function that will create a new dataframe capturing the mean, standard deviation, and confidence intervals of your data
# Don't alter any of it. Just run it as is.
CustomSummary <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                          conf.interval=.95, .drop=TRUE) {
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm))
                 },
                 measurevar
  )
  datac <- plyr::rename(datac, c("mean" = measurevar))
  datac$se <- datac$sd / sqrt(datac$N)
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  return(datac)
}

# I then run that function with my outcome variable in quotes, and then all of my independent variables in the concatenated area
Summary <- CustomSummary(DATAFRAME, "YVAR", c("XVAR1", "XVAR2"))


# Then this is the ggplot function I usually run, which has more bells and whistles than you might need. 
Plot <- ggplot(DATAFRAME, aes(x = XVAR1, y = YVAR, color = XVAR2, group = XVAR2)) +
  # This plots the mean value
  stat_summary(geom="point", fun = mean, size = 3.5, position=position_dodge(width = 0.45)) +
  # This plots the confidence intervals using that summary dataframe we created
  geom_errorbar(data = Summary, aes(ymin = YVAR - ci, ymax = YVAR + ci), size= 0.75, width=0.25, position=position_dodge(width = 0.45)) +
  # This will produce the violin plots I'd mentioned, which might not be necessary. 
  geom_violin(trim=FALSE, position=position_dodge(width = 0.45)) +
  # This will produce those jittered dots * I think*. Let me know if it doesn't work
  geom_jitter(shape = 16, position =position_jitter(0.2)) +
  # Everything below this point is just to spruce up the plot and is not necessary.
  # This just let's me specify the levels of my moderator variable and note the color
  scale_color_brewer(palette="Dark2", name="XVAR2", breaks=c("X2LVL1", "X2LVL2"), labels=c("X2LVL1", "X2LVL2")) +
  # Also specifies level of our main predictor
  scale_x_discrete(name = "XVAR1", labels = c("X1LVL1", "X1LVL2")) +
  # Determines the labels we want to use. Feel free to play around with these.
  labs(title = "TITLE",
       subtitle = "FINDING",
       x =NULL, 
       y ="YVAR LABEL",
       caption = "p > 0.05: N.S. \np < 0.05: * \np < 0.01: ** \np < 0.001: ***") +
  # This constrains the scope of the plot without eliminating data. I'm commenting it out because I don't know the range of your data
  # coord_cartesian(ylim=c(0.05, 0.45)) +
  #This stuff just affects all of the text and style
  theme_classic() +
  theme(plot.title = element_text(face="bold", size=13, hjust = 0.5)) +
  theme(plot.subtitle = element_text(size = 10, hjust = 0.5, face = "italic")) +
  theme(plot.caption = element_text(size = 8, hjust = 0.0, face = "italic")) +
  theme(axis.title = element_text(size = 12)) +
  theme(axis.text.x = element_text(size = 14, color = "Black")) +
  theme(axis.text.y = element_text(size = 14, color = "Black"))
Plot 
```