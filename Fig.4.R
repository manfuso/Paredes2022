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
glm.bo.shdi$coefficients
LMMOutput <- data.frame(glm.bo.shdi$coefficients)

#new code to capture ANOVA 
anovaOutput <- data.frame(anova(glm.bo.shdi))

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

## Save plot
ggsave("Fig.4.png", plot = g.shdi.com, scale=0.5)

###Combining plots
final.plot <- ggarrange(g.shdi.com,g.olive.com)
final.plot


####################################### 
############### ORKG ##################
####################################### 
library(orkg)
orkg <- ORKG(host="https://incubating.orkg.org")
# Template 'LMM Planned Process('
orkg$templates$materialize_template(template_id = "R492225")
tp = orkg$templates$list_templates()
keys(tp)

##################################
######### Definitions ############
##################################

Meter <- tp$qudt_unit(label="m", qudtucumcode="m", same_as="http://qudt.org/vocab/unit/M")
BactroceraOleae <- tp$entity(label="Bactrocera Oleae", same_as="https://www.wikidata.org/wiki/Q2207329")
Abundance <- tp$property(label="Abundance", same_as="http://purl.obolibrary.org/obo/NCIT_C70589")
LandscapeDiversity <- tp$entity(label="Landscape Diversity", same_as="http://")
OliveGrove <-  tp$entity(label="Olive Grove", same_as="http://purl.obolibrary.org/obo/ENVO_00000193")
Percentage <- tp$property(label="Percentage", same_as="http://purl.obolibrary.org/obo/NCIT_C70589")
SamplingPoint <- tp$entity(label="Sampling Point", same_as="http://")
Radius <- tp$quantity_value(label="250 m radius", qudtnumericvalue= 500, qudtunit=Meter)
BufferArea <- tp$entity(label="Buffer Area", same_as="http://", is_constrained_by=Radius)

################################
######## LMM Variables #########
################################

var_bactrocera_oleae_abundance <- tp$variable(
  label="Bactrocera oleae abundance in olive groves",
  has_object_of_interest_= BactroceraOleae,
  has_matrix = OliveGrove,
  has_property = Abundance
)


var_surrounding_olive_groves <- tp$variable(
  label="Bactrocera oleae abundance in olive groves",
  has_object_of_interest_= OliveGrove,
  has_matrix = SamplingPoint,
  has_property = Percentage
)

var_landscape_Shannon_diversity <- tp$variable(
  label="Landscape Shannon Diversity Index",
  has_object_of_interest_= LandscapeDiversity,
  has_matrix = BufferArea,
  has_property = Percentage
)


################################
############ Anova  ############
################################
ANOVA <- tp$anova(
  label="ANOVA with bo (bactrocera oleae abundance) as the response variable and shdi (shannon diversity index) as term.",
  has_input_dataset= tuple(LMMOutput, "Fitted LMM with bo (bactrocera oleae abundance) as the response variable and shdi (shannon diversity index) as a fixed effect."),
  has_output_dataset= tuple(anovaOutput, 'Df (degrees of freedom of the numerator), Df (degrees of freedom of the denominator) and the associated p-value for fixed effects.'),
)

################################
############# LMM  #############
################################
lmm <- tp$linear_mixed_model(
  label="A linear mixed model (LMM) with bactrocera oleae abundance (bo) as the response variable and shdi (shannon diversity index) as a fixed effect",
  has_response_variable = var_bactrocera_oleae_abundance,
  has_fixed_effect_term_i = var_landscape_Shannon_diversity,
)

################################
######### LMM Fitting  #########
################################
lmmFitting <- tp$linear_mixed_model_fitting(
  label="A linear mixed model (LMM) fitting with bactrocera oleae abundance (bo) as the response variable and shdi (shannon diversity index) as a fixed effect",
  has_input_dataset= tuple(data, "Raw field data on bactrocera oleae abundance"),
  has_input_model=lmm,
  has_output_dataset= tuple(LMMOutput, 'Results of LMM fitting with bo as the response variable and shdi a as a fixed effect'),
)

################################
######## LMM Prediction  #######
################################
LMMPrediction <- tp$lmm_prediction(
  label="Prediction using the fitted LMM with bo as the response variable shdi as a fixed effect.",
  has_input_dataset= tuple(LMMOutput, "Fitted LMM with bo as the response variable shdi as a fixed effect."),
  has_output_figure = "https://raw.githubusercontent.com/manfuso/Paredes2022/main/Fig.4.png",
)

################################
#### LMM Planned Process #######
################################
instance <- tp$lmm_planned_process(
  has_implementation= "https://raw.githubusercontent.com/manfuso/Paredes2022/main/Fig4.snippet.R",
  label="Estimated effects of landscape diversity on Bactrocera oleae abundance.", 
  #as_lmm_fitting= lmmFitting,
  has_anova = ANOVA,
)
instance$serialize_to_file("article.contribution.1.json", format="json-ld")
