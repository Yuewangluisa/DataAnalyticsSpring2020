multivariate <-read.csv(file.choose(),header=TRUE)
attach(multivariate)
names(multivariate)
multivariate
plot(Income,Immigrant,main='Scatterplot')
plot(Immigrant,Homeowners)
help(lm)
mm<-lm(Homeowners~Immigrant)
mm
plot(Immigrant,Homeowners)
abline(mm)
abline(mm,col=2,lwd=3)
summary(mm)
attributes(mm)
mm$coefficients
HP<- Homeowners/Population
PD<-Population/area
mm<-lm(Immigrant~Income+Population+HP+PD)
summary(mm)
cm<-coef(mm)
cm
library(dplyr)
#method 1
df_mtcars <- mtcars
head(df_mtcars)
filter(df_mtcars, mpg > 20) 
sample_n(filter(df_mtcars, mpg > 20), 10)
arrange( sample_n(filter(df_mtcars, mpg >20), 10) ,desc(mpg))
results_mpg <- arrange( sample_n(filter(df_mtcars, mpg >20), 10) ,desc(mpg))
results_mpg
#method 2
a1<-filter(df_mtcars, mpg > 20)
a2<-sample_n(df_mtcars,5)
results_mpg_des<-arrange(a2,desc(mpg))
results_mpg_des
#method 3
df_mtcars%>%filter(mpg>20)%>%sample_n(10)%>%arrange(desc(mpg))
results<-df_mtcars%>%filter(mpg>20)%>%sample_n(10)%>%arrange(desc(mpg))
results

install.packages('gcookbook')
library(gcookbook) 
library(ggplot2)
ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat = "identity")
BOD
# Time is numeric (continuous)
str(BOD)
ggplot(BOD, aes(x=Time, y=demand)) + geom_bar(stat = "identity")
# Convert Time to a discrete (categorical) variable with factor() function.
ggplot(BOD, aes(x=factor(Time), y=demand)) + geom_bar(stat = "identity")
# change the color of the bars and add an outline to the bars
# NOTE: In ggplot2, the default is to use the British spelling, colour, instead of
# the American spelling, color.
ggplot(pg_mean, aes(x=group, y=weight)) +geom_bar(stat = "identity", fill="lightblue", colour = "red")
ggplot(BOD, aes(x=factor(Time), y=demand)) +geom_bar(stat = "identity", fill="orange", colour = "red")
# Grouping Bars Together
# You want to group bars together by a second variable.
# In this example we’ll use the cabbage_exp data set, which has two categorical variables,
# Cultivar and Date, and one continuous variable, Weight:
library(gcookbook) # For the data set
library(ggplot2)
cabbage_exp
# We’ll map Date to the x position and map Cultivar to the fill color
# ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(position = "dodge")
ggplot(cabbage_exp, aes(x=Date, fill=Cultivar)) + geom_bar(position = "dodge")

library(gcookbook) # For the data set
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity")

# Making Bar Graph of Counts
ggplot(diamonds, aes(x=cut)) +geom_bar() # this is equvalent to using geom_bar(stat="bin)
# The diamonds data set has 53,940 rows, each of which represents information about one
data("diamonds")
diamonds
# In this example, the variable on the x-axis is discrete. If we use a continuous variable on
# the x-axis, we’ll get a histogram
ggplot(diamonds,aes(x=carat)) + geom_bar()
# It turns out that in this case, the result is the same as if we had used geom_histogram() instead of geom_bar()
ggplot(diamonds, aes(x=carat)) + geom_histogram()
# Using Colors in Bar Graphs. Now we want to use different colors for the bars in our bar graph
# We can do this by using the "fill" asethetic. 
# We’ll use the uspopchange data set for this example. It contains the percentage change
# in population for the US states from 2000 to 2010. We’ll take the top 10 fastest-growing
# states and graph their percentage change. 
# We’ll also color the bars by region (Northeast,South, North Central, or West)
# Taking Top 10 States 
library(gcookbook) # for the dataset
ups <- subset(uspopchange, rank(Change)>40)
ups
# Now we can make the graph, mapping Region to fill
ggplot(ups, aes(x=Abb, y= Change, fill=Region)) + geom_bar(stat = "identity")
# Do an Experiment with followings ... :=) 
ggplot(ups, aes(x=Abb, y=Change, fill=Region)) +geom_bin2d()
ggplot(ups, aes(x=Abb, y=Change, fill=Region)) + geom_col()
# The default colors aren’t very appealing, so you may want to set them, using
# scale_fill_brewer() or scale_fill_manual().
ggplot(ups, aes(x=reorder(Abb,Change), y=Change, fill=Region)) + geom_bar(stat = "identity", colour= "red") +
  scale_fill_manual(values=c("#669933", "#FFCC66")) + xlab("US-States")
ggplot(ups, aes(x=reorder(Abb,Change), y=Change, fill=Region)) + geom_bar(stat = "identity", color = "purple") +
  scale_fill_manual(values=c("#224455","#DDCC33"))
# Coloring Negative and Positive Bars Differently
# You want to use different colors for negative and positive-valued bars.
library(gcookbook)
csub <- subset(climate, source="Berkeley" & Year >= 1900)
csub
csub$pos <- csub$Anomaly10y >=0
csub
ggplot(csub, aes(x=Year, y=Anomaly10y, fill= pos)) + geom_bar(stat = "identity", position = "identity")
# changing the color with scale_fill_manual
ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) + geom_bar(stat="identity", colour="black", size=0.25) +
  scale_fill_manual(values=c("#CCEEFF", "#FFDDDD"), guide=FALSE)
# Adjusting Bar Width and Spacing
# You want to adjust the width of bars and the spacing between them.
# To make the bars narrower or wider, set width in geom_bar(). The default value is 0.9; 
# larger values make the bars wider, and smaller values make the bars narrower
library(gcookbook) # for the datset
ggplot(pg_mean, aes(x=group, y=weight)) +geom_bar(stat="identity")
# Narrow Bars
ggplot(pg_mean, aes(x=group, y=weight)) +geom_bar(stat="identity", width = 0.5)
# Wider bars, maximum width = 1
ggplot(pg_mean, aes(x=group, y=weight)) +geom_bar(stat = "identity", width = 0.95)
# Different bar widths
ggplot(cabbage_exp, aes(x=Date, y= Weight, fill=Cultivar)) + geom_bar(stat = "identity", width = 0.5, position = "dodge")
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(stat = "identity", width = 0.5, position = position_dodge(0.7))
# Making a Sketched Bar Graph
library(gcookbook) # for the dataset
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(stat = "identity")
cabbage_exp
ggplot(cabbage_exp, aes(x= Date, y= Weight, fill=Cultivar)) + geom_bar(stat = "identity") + guides(fill=guide_legend(reverse = TRUE))
# Adding Lables to your Graphs
library(gcookbook) # For the data set
ggplot(cabbage_exp, aes(x=interaction(Date,Cultivar), y=Weight)) +geom_bar(stat = "identity") + geom_text(aes(label=Weight),vjust=1.5,colour="white")
library(ggplot2)
# Adjust y limits to be a little higher
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Weight), vjust=-0.2) +
  ylim(0, max(cabbage_exp$Weight) * 1.05)

# Map y positions slightly above bar top - y range of plot will auto-adjust
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
  geom_bar(stat="identity") +
  geom_text(aes(y=Weight+0.1, label=Weight))

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=Weight), vjust=1.5, colour="white",
            position=position_dodge(.9), size=3)
# make a Cleveland dot plot
#The simplest way to create a dot plot is to use geom_point() function
library(gcookbook) # For the data set

tophit <- tophitters2001[1:25,] # take top 25 top hitters
tophit
ggplot(tophit, aes(x=avg, y=name)) + geom_point()
tophit[,c("name","lg","avg")]
ggplot(tophit, aes(x=avg, y= reorder(name,avg))) + geom_point(size=3, colour="red") + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour ="grey60",linetype="dashed"))

ggplot(tophit, aes(x=avg, y=reorder(name,avg))) + geom_point(size=2.5, colour="blue") + 
  theme_classic() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey60", linetype = twodash))
# Get the names sorted by lg, then by avg
nameorder <- tophit$name[order(tophit$lg, tophit$avg)]
# Turn the name into factor, with levels in the order of nameorder
tophit$name <- factor(tophit$name, levels = nameorder)
ggplot(tophit, aes(x=avg, y=name)) +
  geom_segment(aes(yend=name), xend=0, colour="grey70")+
  geom_point(size=3, aes(colour=lg)) +
  scale_color_brewer(palette="Set1", limits=c("NL","AL")) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = c(1,0.55),
        legend.justification = c(1,0.5))

ggplot(tophit, aes(x=avg, y=name)) +
  geom_segment(aes(yend=name), xend=0, colour="grey40") +
  geom_point(size=3, aes(colour=lg)) +
  scale_color_brewer(palette="Set1", limits=c("NL","AL"), guide=FALSE) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  facet_grid(lg ~ ., scales = "free_y", space="free_y")
