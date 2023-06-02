############################################
####FIGURE 4: 10.1007/s10340-022-01489-1#####
#############################################
####Library to do the model
library(MASS)#
####Library to do the plots
library(ggplot2)#
library(ggpubr)#

####This is to keep the graphs with the same appearance
my.theme = theme(
  title= element_text(size = 20),
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 14),
  axis.text.y = element_text(size = 14),
  axis.title.y = element_text(size = 16),
  axis.line = element_line(size = 0.5, colour = "black"),
  panel.background = element_rect(fill = "white"),
  plot.margin = margin(4,4,4,4))

###Loading data
data <- read.csv("data.csv")

#new code to capture sample size (needed to calculate the coefficient of correlation)
samplesize <- length(data$shdi)

######Model surrounding olive groves
glm.bo.ol<- glm.nb(bo~olive,data=data)

######Model landscape Shannon diversity
glm.bo.shdi<- glm.nb(bo~shdi,data=data)

#new code to capture ANOVA 
anova(glm.bo.shdi)


###Plotting predictions
####Predictions model surrounding olive groves
Xolive <-data.frame(olive=seq(min(data$olive),max(data$olive),length=nrow(data)))


new.data.olive.com <- data.frame(altitude= mean(data$altitude),
                                 ponto=data$ponto,eucalyptus=mean(data$eucalyptus), oak = mean(data$oak), vineyards =mean(data$vineyards),
                                 olive=Xolive, pine =mean(data$pine), pastagem=mean(data$pastagem), scrubs =mean(data$scrubs))


new.data.olive.com$fit <- data.frame(predict(glm.bo.ol, new.data.olive.com, se=T, exclude=c("s(ponto)")))

new.data.olive.com$fit.fit <- new.data.olive.com$fit$fit
new.data.olive.com$up.fit <- new.data.olive.com$fit.fit+new.data.olive.com$fit$se.fit
new.data.olive.com$down.fit <- new.data.olive.com$fit.fit-new.data.olive.com$fit$se.fit


g.olive.com <- ggplot(data,aes(x=olive, y =data$bo))+ my.theme+
  geom_line( data = new.data.olive.com, aes(y = exp(fit.fit)), color="forestgreen") +
  geom_ribbon(data = new.data.olive.com, aes(ymin=exp(down.fit), ymax=exp(up.fit)),alpha=0.5,fill="green4") +
  geom_point()+ xlab("Surrounding olive groves (%)") + ylab("")

g.olive.com


####Predictions model Shannon diversity
Xshdi <-data.frame(shdi=seq(min(data$shdi),max(data$shdi),length=nrow(data)))

new.data.shdi.com <- data.frame(altitude= mean(data$altitude), shdi= Xshdi,
                                ponto=data$ponto,eucalyptus=mean(data$eucalyptus), oak = mean(data$oak), vineyards =mean(data$vineyards),
                                olive=mean(data$olive), pine =mean(data$pine), pastagem=mean(data$pastagem), scrubs =mean(data$scrubs))


new.data.shdi.com$fit <- data.frame(predict(glm.bo.shdi, new.data.shdi.com, se=T, exclude=c("s(ponto)")))

new.data.shdi.com$fit.fit <- new.data.shdi.com$fit$fit
new.data.shdi.com$up.fit <- new.data.shdi.com$fit.fit+new.data.shdi.com$fit$se.fit
new.data.shdi.com$down.fit <- new.data.shdi.com$fit.fit-new.data.shdi.com$fit$se.fit


g.shdi.com <- ggplot(data,aes(x=shdi, y =data$bo))+ my.theme+
  geom_line( data = new.data.shdi.com, aes(y = exp(fit.fit)), color="sienna4") +
  geom_ribbon(data = new.data.shdi.com, aes(ymin=exp(down.fit), ymax=exp(up.fit)),alpha=0.5,fill="sienna1") +
  geom_point() + xlab("Landscape Shannon diversity index") + ylab("B. oleae abundance")
g.shdi.com


###Combining plots
final.plot <- ggarrange(g.shdi.com,g.olive.com)
final.plot