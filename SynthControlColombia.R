# Replication code master thesis
# Author: Jorge M. orozco

# Libraries
library(gtools)
library(kernlab)
library(foreign)
library(Synth)
library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(xtable)

paises_deseadosA<-c("Colombia", "Costa Rica", "Cyprus", "Dominican Republic",
                    "Ecuador", "Gabon", "Malaysia", "Mauritius", "Panama",
                    "Paraguay", "Singapore", "Uruguay")

# Fig 2: Donor Pools
GTD <- read_csv("databases/terrorist-attacks.csv")
GTD2 <- GTD %>%
  mutate(Entity = case_when(
    Entity == "Republic of Korea" ~ "South Korea",
    Entity == "Republic of the Congo" ~ "Congo",
    TRUE ~ Entity
  ))

GTD2 <- GTD2 %>%
  filter(`Entity` %in% paises_deseadosA)
GTD2filtro<-subset(GTD2, Year >=1976 & Year<=1994)

promedio_ataques <- GTD2filtro %>%
  group_by(`Entity`) %>%  
  summarise(Promedio_Ataques = mean(`Terrorist attacks`, na.rm = TRUE)) 
print(promedio_ataques)

GTD2<-subset(GTD2, Year >=1970 & Year<=1994)
colnames(GTD2)<-c("Country Name", "Code", "Year", "Attacks")

# PLOT
ggplot(GTD2, aes(x = Year, y = Attacks, color = `Country Name`)) +
  geom_line(size = 1) +  # Línea para los ataques terroristas
  labs(title = "",
       x = "",
       y = "Number of terrorist attacks",
       color = "Country") +
  theme_minimal() +  # Tema minimalista para un diseño limpio
  theme(plot.title = element_text(hjust = 0.5))  # Centrar el título

## Synthetic control
library(haven)
sintetico<-read_dta("databases/BaseSC.dta")
sintetico<-as.data.frame(sintetico)

# Model
dataprep.out <-
  dataprep(
    foo = sintetico,
    predictors    = c("Infrate"), # Predictores
    dependent     = "GDPppp", # Dependiente
    unit.variable = 18, # index
    time.variable = 2, # Años
    special.predictors = list(
      list("GDPppp", 1960:1965, c("mean")),
      list("Invest60", 1970, c("mean")),
      list("Secondary", c(1960,1965), c("mean")),
      list("Primary", c(1960,1965), c("mean")),
      list("Terciary", c(1960,1965), c("mean")),
      list("HI", 1960:1965, c("mean")),
      list("AvgSchool", 1960:1965, c("mean"))
    ),
    treatment.identifier = 1,
    controls.identifier = unique(sintetico$index)[-1],
    time.predictors.prior = 1957:1966, 
    time.optimize.ssr = 1966:1975,
    unit.names.variable = 1, #nombres
    time.plot = 1950:1994
  )

# fit training model
synth.out <- 
  synth(
    data.prep.obj=dataprep.out,
    Margin.ipop=.005,Sigf.ipop=7,Bound.ipop=6
  )

# data prep for main model
dataprep.out <-
  dataprep(
    foo = sintetico,
    predictors    = c("Infrate"),
    dependent     = "GDPppp",
    unit.variable = 18,
    time.variable = 2,
    special.predictors = list(
      list("GDPppp", 1970:1975, c("mean")),
      list("Invest70", 1970, c("mean")),
      list("Secondary", c(1970,1975), c("mean")),
      list("Primary", c(1970,1975), c("mean")),
      list("Terciary", c(1970,1975), c("mean")),
      list("HI", 1970:1975, c("mean")),
      list("AvgSchool", 1970:1975, c("mean"))
    ),
    treatment.identifier = 1,
    controls.identifier = unique(sintetico$index)[-1],
    time.predictors.prior = 1966:1975,
    time.optimize.ssr = 1955:1974,
    unit.names.variable = 1,
    time.plot = 1950:1994
  )
# fit main model with v from training model
synth.out <- synth(
  data.prep.obj=dataprep.out,
  custom.v=as.numeric(synth.out$solution.v)
)

synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out
); synth.tables

# Tabla Performance
rownames(synth.tables$tab.pred) <- c("Inflation Rate","GDP per capita",
                                     "Investment rate","% Secondary",
                                     "% Primary","% Terciary", "Human Capital index", "Avg Years of schooling")
xtable(round(synth.tables$tab.pred,1),digits=1)

# Tabla peso variables
xtable(round(synth.tables$tab.v,3),digits=3)

# Tabla pesos paises sintetico vs regresion
# synth weights
tab1 <- data.frame(synth.tables$tab.w)
tab1[,1] <- round(tab1[,1],2) 
# regression weights
X0 <- cbind(1,t(dataprep.out$X0))
X1 <- as.matrix(c(1,dataprep.out$X1))
W     <- X0%*%solve(t(X0)%*%X0)%*%X1
Wdat  <- data.frame(unit.numbers=as.numeric(rownames(X0)),
                    regression.w=round(W,2))
tab1  <- merge(tab1,Wdat,by="unit.numbers")
tab1  <- tab1[order(tab1[,3]),]

xtable(cbind(tab1[1:9,c(3,2,4)],
             tab1[10:18,c(3,2,4)]
)
)

# Fig 3: Evolution of GDP per capita: Colombia vs. synthetic Colombia
Text.height <- 10000
Cex.set <- .8
# Plotear resultados
synthY0 <- (dataprep.out$Y0%*%synth.out$solution.w)
plot(1950:1994,dataprep.out$Y1plot,
     type="l",ylim=c(0,14000),col="black",lty="solid",
     ylab ="GDP per capita (PPP, 2011 USD)",
     xlab ="Year",
     xaxs = "i", yaxs = "i",
     lwd=2
)
lines(1950:1994,synthY0,col="black",lty="dashed",lwd=2)
abline(v=1975,lty="dotted")
legend(x="bottomright",
       legend=c("Colombia","Synthetic Colombia")
       ,lty=c("solid","dashed"),col=c("black","black")
       ,cex=.6,bg="white",lwd=c(2,2))
arrows(1973,Text.height,1975,Text.height,col="black",length=.1)
text(1980,Text.height,"Conflict intensification",cex=Cex.set)

# Fig 4: Difference in GDP per capita between Colombia and synthetic Colombia
gap <- dataprep.out$Y1-(dataprep.out$Y0%*%synth.out$solution.w)
plot(1950:1994,gap,
     type="l",ylim=c(-4500,4500),col="black",lty="solid",
     ylab =c("Gap GDP per capita (PPP, 2011 USD)"),
     xlab ="Year",
     xaxs = "i", yaxs = "i",
     lwd=2
)
abline(v=1975,lty="dotted")
abline(h=0,lty="dotted")
arrows(1973,1000,1975,1000,col="black",length=.1)
text(1980,1000,"Conflict intensification",cex=Cex.set)

# Loss of GDP
gaps <- dataprep.out$Y1 - (dataprep.out$Y0plot %*% synth.out$solution.w)
gaps[26:45, 1]
mean(gaps)

# Calculation:
# perdida en promedio anual: 627.2921 USD a terminos de porcentaje en terminos
# de los años 75 = 627.2921/5772 = 10.86%.
# En 1994, el PIB per capita de la Colombia sintetica se estima en ser un 31.78%
# mayor que en la Colombia real. (2685.21/8447.85=0.3178)

# Fig 5: Placebo over time 1970-Trends in GDP per capita: Colombia v.s synthetic Colombia
dataprep.out <-
  dataprep(
    foo = sintetico,
    predictors    = c("Infrate"), # Predictores
    dependent     = "GDPppp", # Dependiente
    unit.variable = 18, # index
    time.variable = 2, # Años
    special.predictors = list(
      list("GDPppp", c(1955,1960), c("mean")),
      list("Secondary", c(1955,1960),c("mean")),
      list("Primary", c(1955,1960), c("mean")),
      list("Terciary", c(1955,1960), c("mean")),
      list("AvgSchool",1955, c("mean")),
      list("Invest60", 1970, c("mean")),
      list("HI", 1960, c("mean"))
    ),
    treatment.identifier = 1,
    controls.identifier = unique(sintetico$index)[-1],
    time.predictors.prior = 1951:1964, 
    time.optimize.ssr = 1961:1970,
    unit.names.variable = 1, #nombres
    time.plot = 1950:1994
  )

# fit training model
synth.out <- 
  synth(
    data.prep.obj=dataprep.out,
    Margin.ipop=.005,Sigf.ipop=7,Bound.ipop=6
  )

# data prep for main model
dataprep.out <-
  dataprep(
    foo = sintetico,
    predictors    = c("Infrate"),
    dependent     = "GDPppp",
    unit.variable = 18,
    time.variable = 2,
    special.predictors = list(
      list("GDPppp", c(1965,1970), c("mean")),
      list("Secondary", c(1965,1970),c("mean")),
      list("Primary", c(1965,1970), c("mean")),
      list("Terciary", c(1965,1970), c("mean")),
      list("AvgSchool",1970, c("mean")),
      list("Invest60", 1970, c("mean")),
      list("HI", 1970, c("mean"))
    ),
    treatment.identifier = 1,
    controls.identifier = unique(sintetico$index)[-1],
    time.predictors.prior = 1961:1970,
    time.optimize.ssr = 1950:1969,
    unit.names.variable = 1,
    time.plot = 1950:1994
  )

# fit main model
synth.out <- synth(
  data.prep.obj=dataprep.out,
  custom.v=as.numeric(synth.out$solution.v)
)

synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out
); synth.tables

# Image
Cex.set <- 1
#pdf(file = "2intimeplacebo1975.pdf", width = 5.5, height = 5.5, family = "Times",pointsize = 12)
plot(1950:1994,dataprep.out$Y1plot,
     type="l",ylim=c(0,14000),col="black",lty="solid",
     ylab ="GDP per capita (PPP)",
     xlab ="Year",
     xaxs = "i", yaxs = "i",
     lwd=2
)
aver<-dataprep.out$Y0%*%synth.out$solution.w
lines(1950:1994,(dataprep.out$Y0%*%synth.out$solution.w),col="black",lty="dashed",lwd=2)
abline(v=1970,lty="dotted")
legend(x="bottomright",
       legend=c("Colombia", "Synthetic Colombia")
       ,lty=c("solid","dashed"),col=c("black","black")
       ,cex=.8,bg="white",lwd=c(2,2))
arrows(1968,10000,1970,10000,col="black",length=.1)
text(1975,10000,"Placebo treatment",cex=Cex.set)

# Fig 6: Placebo in space -Differences in GDP per capita: Colombia vs. donors
# In space test
# Crear una matriz para almacenar las brechas de cada unidad
store <- matrix(NA, length(1950:1994), 12)
colnames(store) <- unique(sintetico$index)

for (iter in 1:12) {
  
  # Preparar datos para el placebo
  dataprep.out <- dataprep(
    foo = sintetico,
    predictors = c("Infrate"),
    dependent = "GDPppp",
    unit.variable = 18,
    time.variable = 2,
    special.predictors = list(
      list("GDPppp", 1960:1965, c("mean")),
      list("Invest60", 1970, c("mean")),
      list("Secondary", c(1960, 1965), c("mean")),
      list("Primary", c(1960, 1965), c("mean")),
      list("Terciary", c(1960, 1965), c("mean")),
      list("HI", 1960:1965, c("mean")),
      list("AvgSchool", 1960:1965, c("mean"))
    ),
    treatment.identifier = iter,
    controls.identifier = setdiff(1:12, iter),
    time.predictors.prior = 1957:1966,
    time.optimize.ssr = 1966:1975,
    unit.names.variable = 1,
    time.plot = 1950:1994
  )
  
  # Ajustar modelo sintético
  synth.out <- synth(
    data.prep.obj = dataprep.out,
    Margin.ipop = .005, Sigf.ipop = 7, Bound.ipop = 6
  )
  
  # data prep for main model
  dataprep.out <-
    dataprep(
      foo = sintetico,
      predictors    = c("Infrate"),
      dependent     = "GDPppp",
      unit.variable = 18,
      time.variable = 2,
      special.predictors = list(
        list("GDPppp", 1970:1975, c("mean")),
        list("Invest70", 1970, c("mean")),
        list("Secondary", c(1970,1975), c("mean")),
        list("Primary", c(1970,1975), c("mean")),
        list("Terciary", c(1970,1975), c("mean")),
        list("HI", 1970:1975, c("mean")),
        list("AvgSchool", 1970:1975, c("mean"))
      ),
      treatment.identifier = iter,
      controls.identifier = setdiff(1:12, iter),
      time.predictors.prior = 1966:1975,
      time.optimize.ssr = 1950:1974,
      unit.names.variable = 1,
      time.plot = 1950:1994
    )
  
  # fit main model 
  synth.out <- synth(
    data.prep.obj=dataprep.out,
    custom.v=as.numeric(synth.out$solution.v)
  )
  
  # Calcular la brecha entre los valores observados y los predichos
  store[, iter] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}

# figura
# Configurar datos para la figura
data <- store
rownames(data) <- 1950:1994  # Cambia a los años relevantes para tu análisis

# Definir límites en los datos de brechas
gap.start <- 1
gap.end <- nrow(data)
years <- 1950:1994
gap.end.pre <- which(rownames(data) == "1975")

# Calcular MSPE antes del tratamiento
mse <- apply(data[gap.start:gap.end.pre, ]^2, 2, mean)
colombia.mse <- as.numeric(mse[1])  # Suponiendo que Colombia sea el primer país en el índice

# Excluir países con un MSPE cinco veces mayor al de Colombia
data <- data[, mse < 10 * colombia.mse]
Cex.set <- 0.75

# Graficar la brecha en PIB per cápita
plot(
  years,
  data[gap.start:gap.end, which(colnames(data) == "1")],
  ylim = c(-4500, 4500), xlab = "Year",
  xlim = c(1950, 1994), ylab = "Gap GDP per cápita",
  type = "l", lwd = 2, col = "black",
  xaxs = "i", yaxs = "i"
)

# Agregar líneas para los países de control
for (i in 1:ncol(data)) {
  lines(years, data[gap.start:gap.end, i], col = "darkgray")
}

# Agregar línea para Colombia
lines(
  years,
  data[gap.start:gap.end, which(colnames(data) == "1")],
  lwd = 2, col = "black"
)

# Agregar líneas y leyendas adicionales
abline(v = 1975, lty = "dotted", lwd = 2)  # Inicio del tratamiento
abline(h = 0, lty = "dashed", lwd = 2)
legend(
  "topleft", legend = c("Colombia", "Control countries"),
  lty = c(2, 2), col = c("black", "darkgray"), lwd = c(2, 2), cex = 0.8
)

# Fig 7: Relationship between post-treatment RMSPE and pretreatment RMSPE: Colombia and control countries
storegaps <- 
  matrix(NA,
         length(1950:1994),
         length(unique(sintetico$index))-1
  )
rownames(storegaps) <- 1950:1994
i <- 1
co <- unique(sintetico$index)

for(k in unique(sintetico$index)[-1]){
  
  # data prep for training model
  dataprep.out <-
    dataprep(
      foo = sintetico,
      predictors    = c("Infrate"),
      dependent     = "GDPppp",
      unit.variable = 18,
      time.variable = 2,
      special.predictors = list(
        list("GDPppp", 1960:1965, c("mean")),
        list("Invest60", 1970, c("mean")),
        list("Secondary", c(1960,1965), c("mean")),
        list("Primary", c(1960,1965), c("mean")),
        list("Terciary", c(1960,1965), c("mean")),
        list("HI", 1960:1965, c("mean")),
        list("AvgSchool", 1960:1965, c("mean"))
      ),
      treatment.identifier = k,
      controls.identifier = co[-which(co==k)],
      time.predictors.prior = 1957:1966,
      time.optimize.ssr = 1966:1975,
      unit.names.variable = 1,
      time.plot = 1950:1994
    )
  
  # fit training model
  synth.out <-
    synth(
      data.prep.obj=dataprep.out,
      Margin.ipop=.005,Sigf.ipop=7,Bound.ipop=6
    )
  
  # data prep for main model
  dataprep.out <-
    dataprep(
      foo = sintetico,
      predictors    = c("Infrate"),
      dependent     = "GDPppp",
      unit.variable = 18,
      time.variable = 2,
      special.predictors = list(
        list("GDPppp", 1970:1975, c("mean")),
        list("Invest70", 1970, c("mean")),
        list("Secondary", c(1970,1975), c("mean")),
        list("Primary", c(1970,1975), c("mean")),
        list("Terciary", c(1970,1975), c("mean")),
        list("HI", 1970:1975, c("mean")),
        list("AvgSchool", 1970:1975, c("mean"))
      ),
      treatment.identifier = k,
      controls.identifier = co[-which(co==k)],
      time.predictors.prior = 1966:1975,
      time.optimize.ssr = 1950:1974,
      unit.names.variable = 1,
      time.plot = 1950:1994
    )
  
  # fit main model
  synth.out <- synth(
    data.prep.obj=dataprep.out,
    custom.v=as.numeric(synth.out$solution.v)
  )
  
  storegaps[,i] <-  
    dataprep.out$Y1-
    (dataprep.out$Y0%*%synth.out$solution.w)
  i <- i + 1
} # close loop over control units

sintetico <- sintetico[order(sintetico$index,sintetico$Year),]
colnames(storegaps) <- unique(sintetico$Country)[-1]
storegaps <- cbind(gap,storegaps)
colnames(storegaps)[1] <- c("Colombia")

# compute ratio of post-reunification RMSPE 
# to pre-reunification RMSPE                                                  
rmse <- function(x){sqrt(mean(x^2))}
preloss <- apply(storegaps[1:26,],2,rmse)
postloss <- apply(storegaps[27:45,],2,rmse)

#pdf("2ratio_post_to_preperiod_rmse2a.pdf")
dotchart(sort(postloss/preloss),
         xlab="Post-Period RMSE / Pre-Period RMSE",
         pch=19)

# Fig 8: Leave-one-out Synthetic Control Distribution for Colombia
storegaps <- 
  matrix(NA,
         length(1950:1994),
         4)
colnames(storegaps) <- c(3,8,10,12)
co <- unique(sintetico$index)[-1]

for(k in 1:4){
  
  # data prep for training model
  omit <- c(3,8,10,12)[k]  
  dataprep.out <-
    dataprep(
      foo = sintetico,
      predictors    = c("Infrate"),
      dependent     = "GDPppp",
      unit.variable = 18,
      time.variable = 2,
      special.predictors = list(
        list("GDPppp", 1960:1965, c("mean")),
        list("Invest60", 1970, c("mean")),
        list("Secondary", c(1960,1965), c("mean")),
        list("Primary", c(1960,1965), c("mean")),
        list("Terciary", c(1960,1965), c("mean")),
        list("HI", 1960:1965, c("mean")),
        list("AvgSchool", 1960:1965, c("mean"))
      ),
      treatment.identifier = 1,
      controls.identifier = co[-which(co==omit)],
      time.predictors.prior = 1957:1966,
      time.optimize.ssr = 1966:1975,
      unit.names.variable = 1,
      time.plot = 1950:1994
    )
  
  # fit training model
  synth.out <- synth(
    data.prep.obj=dataprep.out,
    Margin.ipop=.005,Sigf.ipop=7,Bound.ipop=6
  )
  
  # data prep for main model
  dataprep.out <-
    dataprep(
      foo = sintetico,
      predictors    = c("Infrate"),
      dependent     = "GDPppp",
      unit.variable = 18,
      time.variable = 2,
      special.predictors = list(
        list("GDPppp", 1970:1975, c("mean")),
        list("Invest70", 1970, c("mean")),
        list("Secondary", c(1970,1975), c("mean")),
        list("Primary", c(1970,1975), c("mean")),
        list("Terciary", c(1970,1975), c("mean")),
        list("HI", 1970:1975, c("mean")),
        list("AvgSchool", 1970:1975, c("mean"))
      ),
      treatment.identifier = 1,
      controls.identifier = co[-which(co==omit)],
      time.predictors.prior = 1966:1975,
      time.optimize.ssr = 1950:1974,
      unit.names.variable = 1,
      time.plot = 1950:1994
    )
  
  # fit main model 
  synth.out <- synth(
    data.prep.obj=dataprep.out,
    custom.v=as.numeric(synth.out$solution.v)
  )
  storegaps[,k] <- (dataprep.out$Y0%*%synth.out$solution.w)
} # close loop over leave one outs

Text.height <- 23000
Cex.set <- .8
#pdf(file = "1jackknife2.pdf", width = 5.5, height = 5.5, family = "Times",pointsize = 12)
plot(1950:1994,dataprep.out$Y1plot,
     type="l",ylim=c(0,14000),col="black",lty="solid",
     ylab ="GDP per capita (PPP, 2011 USD)",
     xlab ="Year",
     xaxs = "i", yaxs = "i",lwd=2
)

abline(v=1975,lty="dotted")
arrows(1972,8500,1974,8500,col="black",length=.1)
for(i in 1:4){
  lines(1950:1994,storegaps[,i],col="darkgray",lty="solid")
}
lines(1950:1994,synthY0,col="black",lty="dashed",lwd=2)
lines(1950:1994,dataprep.out$Y1plot,col="black",lty="solid",lwd=2)
text(1969,8500,"Treatment",cex=.8)
legend(x="topleft",
       legend=c("Colombia",
                "Synthetic Colombia",
                "Synthetic Colombia (leave-one-out)")
       ,lty=c("solid","dashed","solid"),
       col=c("black","black","darkgray")
       ,cex=.6,bg="white",lwdc(2,2,1))

# Table 3 and fig 9
rm(list=ls())
sintetico<-read_dta("databases/BaseSC.dta")
sintetico<-as.data.frame(sintetico)

dataprep.out <-
  dataprep(
    foo = sintetico,
    predictors    = c("Infrate"),
    dependent     = "GDPppp",
    unit.variable = 18,
    time.variable = 2,
    special.predictors = list(
      list("GDPppp", 1960:1965, c("mean")),
      list("Invest60", 1970, c("mean")),
      list("Secondary", c(1960,1965), c("mean")),
      list("Primary", c(1960,1965), c("mean")),
      list("Terciary", c(1960,1965), c("mean")),
      list("HI", 1960:1965, c("mean")),
      list("AvgSchool", 1960:1965, c("mean"))
    ),
    treatment.identifier = 1,
    controls.identifier = unique(sintetico$index)[-1],
    time.predictors.prior = 1957:1966,
    time.optimize.ssr = 1966:1975,
    unit.names.variable = 1,
    time.plot = 1950:1994
  )

# fit training model
synth.out <- 
  synth(
    data.prep.obj=dataprep.out,
    Margin.ipop=.005,Sigf.ipop=7,Bound.ipop=6
  )

# data prep for main model
dataprep.out <-
  dataprep(
    foo = sintetico,
    predictors    = c("Infrate"),
    dependent     = "GDPppp",
    unit.variable = 18,
    time.variable = 2,
    special.predictors = list(
      list("GDPppp", 1970:1975, c("mean")),
      list("Invest70", 1970, c("mean")),
      list("Secondary", c(1970,1975), c("mean")),
      list("Primary", c(1970,1975), c("mean")),
      list("Terciary", c(1970,1975), c("mean")),
      list("HI", 1970:1975, c("mean")),
      list("AvgSchool", 1970:1975, c("mean"))
    ),
    treatment.identifier = 1,
    controls.identifier = unique(sintetico$index)[-1],
    time.predictors.prior = 1966:1975,
    time.optimize.ssr = 1950:1974,
    unit.names.variable = 1,
    time.plot = 1950:1994
  )

# fit main model with v from training model
synth.out <- synth(
  data.prep.obj=dataprep.out,
  custom.v=as.numeric(synth.out$solution.v)
)

synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out
); synth.tables

table3 <- list()
synth.tables$tab.w[,1] <- round(synth.tables$tab.w[,1],2)
table3[[6]] <-synth.tables$tab.w[order(-1*synth.tables$tab.w[,1]),2:1][1:6,]

# compute loss for all combinations 
# of 4, 3, 2, 1 sized donor pools

# get W and v
solution.w <- round(synth.out$solution.w,3)
V <- diag(as.numeric(synth.out$solution.v))

# compute scaled Xs
nvarsV <- dim(dataprep.out$X0)[1]
big.dataframe <- cbind(dataprep.out$X0, dataprep.out$X1)
divisor <- sqrt(apply(big.dataframe, 1, var))
scaled.matrix <-
  t(t(big.dataframe) %*% ( 1/(divisor) *
                             diag(rep(dim(big.dataframe)[1], 1)) ))
X0.scaled <- scaled.matrix[,c(1:(dim(dataprep.out$X0)[2]))]
X1.scaled <- as.matrix(scaled.matrix[,dim(scaled.matrix)[2]])

dn <- sintetico[sintetico$Year==1970,c("Country","index")]
dn <- dn[order(dn$index),]
dn <- dn[-1,]

table2store <- matrix(NA,nrow(dataprep.out$X1),6)
fig7store   <- matrix(NA,length(1950:1994),6)  

# loop through number of controls
for(pp in 6:1){
  store       <- combinations(length(unique(sintetico$index)[-1]),
                              r=pp, v=unique(sintetico$index)[-1])
  store.loss  <- matrix(NA,nrow=nrow(store),1)
  store.w     <- matrix(NA,nrow=nrow(store),pp)
  store.c     <- store.w
  
  # loop through combinations 
  for(k in 1:nrow(store)){
    # index positions of control units
    posvector <- c()
    for(i in 1:pp){
      posvector <- c(posvector,which(dn$index==store[k,i]))
    }
    
    # run quad optimization  
    X0temp <- X0.scaled[ , posvector ]
    H <- t(X0temp) %*% V %*% (X0temp)
    c <- -1*c(t(X1.scaled) %*% V %*% (X0temp) )
    
    if(pp == 1){
      solution.w <- matrix(1)
    } else {      
      res <- ipop(c = c, H = H, A = t(rep(1, length(c))),
                  b = 1, l = rep(0, length(c)),
                  u = rep(1, length(c)), r = 0,
                  margin = 0.005,sigf = 7, bound = 6)
      solution.w <- as.matrix(primal(res))
    }
    loss.w <- t(X1.scaled - X0temp %*% solution.w) %*% V %*% (X1.scaled - X0temp %*% solution.w)
    
    store.loss[k] <- loss.w
    store.w[k,]   <- t(solution.w)
    store.c[k,]   <- dn$Country[posvector]
  } # close loop over combinations
  
  # get best fitting combination
  dat <- data.frame(store.loss,
                    store,
                    store.c,
                    store.w
  )
  colnames(dat) <- c("loss",
                     paste("CNo",1:pp,sep=""),
                     paste("CNa",1:pp,sep=""),
                     paste("W",1:pp,sep="")
  )
  dat <- dat[order(dat$loss),]
  Countries <- dat[1,paste("CNo",1:pp,sep="")]
  Cweights  <- as.numeric(dat[1,paste("W",1:pp,sep="")])
  
  outdat <-  data.frame(unit.names=as.vector(
    (t(as.vector(dat[1,paste("CNa",1:pp,sep="")])))),
    w.weights=round(Cweights,2))
  
  table3[[pp]]<- outdat[order(-1*outdat$w.weights),]
  
  # get posvector for fitting
  posvector <- c()
  if(pp == 1 ){
    posvector <- c(posvector,which(dn$index==Countries))
  } else {
    for(i in 1:pp){
      posvector <- c(posvector,which(dn$index==Countries[1,i]))
    }
  }
  
  X0t <- as.matrix(dataprep.out$X0[,posvector])%*% as.matrix(Cweights)
  table2store[,(6:1)[pp]] <- X0t
  
  fig7store[,(6:1)[pp]] <- 
    dataprep.out$Y0[,posvector]%*%as.matrix(Cweights)
  
} # close loop over number of countries

# Table 3
table3
# Control Units: 
# 1: Mauritius
# 2: Mauritius, Panama
# 3: Paraguay, Singapore, Uruguay
# 4: Gabon, Paraguay, Singapore, Uruguay
# 5: Cyprus, Gabon, Mauritius, Paraguay, Uruguay
# 6: Costa Rica, Cyprus, Mauritius, Paraguay, Singapore, Uruguay

# Tabla 4: Combination
table4 <- round(
  cbind(synth.tables$tab.pred[,1:2],
        table2store,
        synth.tables$tab.pred[,3]),1)
rownames(table4) <- c("Inflation rate","GDP per capita",
                      "Investment rate","% Secondary",
                      "% Primary","% Terciary", "Human Capital index", "Avg Years of schooling")
table4

# Fig 9: GDP per capita gap between Colombia and synthetic dispersed controls
Text.height <- 23000
Cex.set <- .8

par(mfrow=c(2,3)) 
for(pp in 6:1){
  #pdf(file = paste("2ger_vs_synth","CValt",pp,".pdf",sep=""), width = 5.5, height = 5.5, family = "Times",pointsize = 12)
  plot(1950:1994,dataprep.out$Y1,
       type="l",ylim=c(0,14000),col="black",lty="solid",
       ylab ="per-capita GDP (PPP, 2011 USD)",
       xlab ="year",
       xaxs = "i", yaxs = "i",
       lwd=2,
       main=paste("No. of control countries: ",pp,sep="")
  )
  lines(1950:1994,fig7store[,c(6:1)[pp]],col="black",lty="dashed",lwd=2)
  abline(v=1975,lty="dotted")
  legend(x="bottomright",
         legend=c("Colombia","Colombia Sintetica")
         ,lty=c("solid","dashed"),col=c("black","black")
         ,cex=.8,bg="white",lwd=c(2,2))
  #dev.off()
}