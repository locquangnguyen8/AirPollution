setwd("~/Downloads")
data1<-read.csv("Data_475.csv",header = TRUE,stringsAsFactors = TRUE)
install.packages(bayesvl)
library(bayesvl)
data1<-na.omit(data1)
install.packages(cowplot)
library(cowplot)
View(data1)
attach(data1)
#Visualization data
## Histogram with density plot
ggplot(data1, aes(x=AirCurrent)) + 
  +     geom_histogram(aes(y=..density..), colour="black", fill="white")+
  +     geom_density(alpha=.2, fill="#FF6666")
###### Model 1
model1<-bayesvl()
model1 <- bvl_addNode(model1,"MoveCity","binom") 
model1 <- bvl_addNode(model1,"AirCurrent","norm")
model1<-bvl_addArc(model1,"AirCurrent","MoveCity","slope")
bvl_bnPlot(model1)
#Generate Stan code
model_string1 <- bvl_model2Stan(model1)
cat(model_string1)
# Model Fit
model1<-bvl_modelFit(model1, data1, warmup = 2000, iter = 5000, chains = 4,cores = 4)
bvl_plotTrace(model1)
bvl_plotGelman(model1)
bvl_plotAcfs(model1,param=NULL,2,2)
bvl_plotDensity(model1,"b_AirCurrent_MoveCity")
bvl_plotIntervals(model1,"b_AirCurrent_MoveCity")
bvl_plotParams(model1,2,2,credMass = 0.89,params = NULL)