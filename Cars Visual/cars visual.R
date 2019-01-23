#load packages for visualization
library(rggobi)
library(ggplot2)
library(RColorBrewer)
#import cars dataset
Cars.data<-read.csv("Cars.csv", header=TRUE)
#store all variables
car.name<-Cars.data$car.name
mpg<-Cars.data$mpg
cylinders<-Cars.data$cylinders
displacement<-Cars.data$displacement
horsepower<-Cars.data$horsepower
weight<-Cars.data$weight
acceleration<-Cars.data$acceleration
model.year<-Cars.data$model.year
origin<-Cars.data$origin

#change formats to numeric
Cars.data$mpg<-as.numeric(as.character(Cars.data$mpg))
Cars.data$horsepower<-as.numeric(as.character(Cars.data$horsepower))
#make origin a factor
Cars.data$origin<-as.factor(Cars.data$origin)

#Create variables for each origin, removing columns car name and origin
cars.USA=Cars.data[origin==1,-c(1,9)]
cars.EUR=Cars.data[origin==2,-c(1,9)]
cars.JPN=Cars.data[origin==3,-c(1,9)]
cars.col<-Cars.data$origin
#pairs(Cars.data, col<-Cars.data)
#summarise each origin
summary(cars.USA)
summary(cars.EUR)
summary(cars.JPN)

#create boxplots and histograms with origin as the factor
par(mfrow=c(2,4))
x.names=c("USA","JPN", "EUR")
for (i in 2:7){ 
  plot.title=colnames(Cars.data)[i]
  boxplot(cars.USA[,i-1], cars.JPN[,i-1], cars.EUR[,i-1], main=plot.title, xaxt="n", col=topo.colors(3))
  axis(1, at=1:3, labels=x.names)
}

{boxplot(Cars.data$model.year~Cars.data$origin, xaxt="n", main="model year", col=topo.colors(3))
axis(1, at=1:3, labels=x.names)}
{hist(origin, xaxt="n", col=topo.colors(3))
axis(1, at=1:3, labels=x.names)}

#obtain correlation matrices for certain variables to justify assumptions made with the boxplots
cor(Cars.data[ , c('weight', 'cylinders', 'displacement', 'horsepower', 'acceleration')], 
    use='complete')
cor(Cars.data[ , c('weight', 'mpg')], 
    use='complete')

#cor(Cars.data[ , c('weight', 'mpg', 'model.year', 'acceleration')], 
    #use='complete')

#g<-ggobi(Cars.data)
#glyph_color(g[1])<-cars.col

#create univariate histogram of cylinders variable for all the dataset
qplot(Cars.data$cylinders, xlab = 'Cylinders', ylab = 'Count', 
      main='Frequency Histogram: Number of Cylinders')



panel.smooth.asp <- function (x, y, col = par("col"), bg = NA, pch = par("pch"), 
                              cex = 1, col.smooth = "red", span = 2/3, iter = 3, asp,...) 
{
  #browser()
  points(x, y, pch = pch, col = col, bg = bg, cex = cex, asp=1)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    lines(lowess(x[ok], y[ok], f = span, iter = iter), col = col.smooth,...) 
}

## put (absolute) correlations on the upper panels,
## with size proportional to the correlations.

panel.cor <- function(x, y, digits=2, prefix="", cex.cor) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)*r
  text(0.5, 0.5, txt, cex = cex.cor)
}

## put histograms on the diagonal
panel.hist <- function(x, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}
pairs(Cars.data, upper.panel=panel.cor, diag.panel=panel.hist)
#create scatterplots with origin as a factor
pairs(Cars.data, col = origin + 1)
#create scatterplots with model year as factor
pairs(Cars.data, col = model.year + 1)

#summarise the whole dataset
summary(Cars.data)

mpg_sorted <- Cars.data[order(-Cars.data$mpg),]
head((mpg_sorted),3)
horsepower_sorted<-Cars.data[order(-Cars.data$horsepower),]
head((horsepower_sorted),3)
acceleration_sorted<-Cars.data[order(-Cars.data$acceleration),]
head((acceleration_sorted),3)