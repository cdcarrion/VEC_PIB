#####
library(readxl)
library(WDI)
library(wbstats)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tsDyn)
library(urca)
library(ggsci)
library(gganimate)
library(forecast) 
library(tseries)
library(huxtable)
library(fUnitRoots)
library(broom)
library(purrr)
library(vars)

#setwd("~/eco2/trabajo_eco2")

#WDIsearch(string = "",
#          field = "name",
#          short = F)

db <- data.frame(
  WDI(country = "ECU",
  indicator = c("EN.ATM.CO2E.PC",  #CO2
                "EG.USE.PCAP.KG.OE", #Uso de energía
                "NY.GDP.PCAP.KD", #gdp per capita cte 2010
                "NE.TRD.GNFS.ZS"),  #comercio
  start = 1980,
  end = 2014,
  extra = F)) 


db =   mutate(db, 
              lco2 = log(EN.ATM.CO2E.PC),
              lener = log(EG.USE.PCAP.KG.OE),
              lgdp = log(NY.GDP.PCAP.KD),
              lcom = log(NE.TRD.GNFS.ZS),
              lcom2 = lcom^2)
#library(ImportExport)
#write.csv(db, "eco2/trabajo_eco2/dd.csv")

df1 <- db %>%
  dplyr::select(year, 
         lco2,
         lener,
         lgdp) %>%
  gather(key = "variable", 
         value = "value", 
         -year)

###--- DOC 1era imagen
ggplot(df1, aes(x = year, 
                y = value)) + 
  geom_line(aes(color = variable), 
            size = 1) +
  theme_bw() +
  xlab("Año") + 
  scale_color_uchicago(name="Variables",
                     labels=c("LCO2","LENERG","LGDP")) +
  labs(caption = "Fuente: Banco Mundial
                  Elaboración: Los autores")


###--- diapo 
p = ggplot(df1, aes(x = year, 
                y = value)) + 
  geom_line(aes(color = variable), 
            size = 1) +
  theme_bw() +
  xlab("Año") + 
  scale_color_uchicago(name="Variables",
                       labels=c("LCO2",
                                "LENERG",
                                "LGDP"    )) +
  labs(title = 'Representación gráfica de los datos de Ecuador', 
       x = 'Año', 
       y = '',
       caption="Fuente: Banco Mundial \nElaboración: Los autores") +
  transition_reveal(year) +
  ease_aes('linear')

#animate(p, fps=15)

#animate (animatedOneCity, 
#         renderer = av_renderer ('animation.mp4'), 
#         width = 1920, height = 1080, 
#         res = 250, fps = 25) 


###################

adfTest ( db$lener,  type  =  "c" ,  title  =  NULL ,  
                description  =  NULL )

t1 = adf.test(db$lgdp)
t2 = adf.test(diff(db$lener))
tab <- map_df(list(t1, t2), tidy)

############
#seleccion optima = 1... 1 rezago
vrsel1 = dplyr::select(db, lco2, lener, lgdp) 
dlco2 = diff(vrsel1$lco2)
dlener = diff(vrsel1$lener)
dlgdp = diff(vrsel1$lgdp)
dvrsel = data.frame(dlco2 ,
                    dlener,
                    dlgdp)
opt = VARselect(dvrsel, lag.max = 4, type = "both")
opt$selection


var.diff = VAR(dvrsel, 
               p = 1, 
               type = "const",
               season = NULL)
summary(var.diff)

ru = roots(var.diff) #solo para var

#correlación serial

corr.ser = serial.test(var.diff, type = "BG")

plot(corr.ser, names = "dlco2")
plot(corr.ser, names = "dlener")
plot(corr.ser, names = "dlgdp")

# heterocedasticidad
#args(normality.test)
var1.arch = arch.test(var.diff, 
                      multivariate.only = T)

#normalidad
var1.norm = normality.test(var.diff, multivariate.only = T)

#fluctuacion
reccusum = stability(var.diff, 
                     type = "OLS-CUSUM")
  #plot(reccusum$stability$dlco2)
flucctuac = stability(var.diff, 
                      type = "fluctuation")
  #plot(flucctuac$stability$dlco2)
        ## inestabilidad en la fluctua?

#causalidad
var1.causal = causality(var.diff, 
                        cause = c("dlener", "dlgdp"))


#forecast
predicci = predict(var.diff, 
                         n.ahead = 5,
                        ci = 0.95)

#respuesta al impulso dlco2
ir.dlco2 = irf(var.diff, 
               impulse = "dlco2",
               response = c("dlener", "dlgdp"),
               n.ahead = 5,
               ortho = F,
               boot = F)

ir.all = irf(var.diff, 
               impulse = c("dlener", "dlgdp"),
               response = "dlco2",
               n.ahead = 5,
               ortho = F,
               boot = F)

# descomposicion de la varianza
var1.descomp = fevd(var.diff,
                    n.ahead = 5)
plot(var1.descomp)





######################
######################

gdp05 = read.csv("eco2/trabajo_eco2/gdp2005.csv", sep = ";")
gdp05$year = gdp05$ï..year
db.new <- data.frame(
  WDI(country = "IT",
      indicator = c("EN.ATM.CO2E.PC",  #CO2
                    "EG.USE.PCAP.KG.OE", #Uso de energía
                    "NY.GDP.PCAP.KD", #gdp per capita cte 2010
                    "NE.TRD.GNFS.ZS"),  #comercio
      start = 1970,
      end = 2011,
      extra = F)) 
db.new =   mutate(db.new, 
              lco2 = log(EN.ATM.CO2E.PC),
              lener = log(EG.USE.PCAP.KG.OE),
              lgdp = log(NY.GDP.PCAP.KD),
              lcom = log(NE.TRD.GNFS.ZS),
              lcom2 = lcom^2)



#write.csv(db.new, "eco2/trabajo_eco2/ddnewIT.csv")

vrsel1.new = dplyr::select(db.new, lco2, lener, lgdp) 
dlco2.n = diff(vrsel1.new$lco2)
dlener.n = diff(vrsel1.new$lener)
dlgdp.n = diff(vrsel1.new$lgdp)
dvrsel.n = data.frame(dlco2.n ,
                      dlener.n,
                      dlgdp.n)

opt = VARselect(dvrsel.n, lag.max = 4, type = "const")
opt$selection

var.diff.n = VAR(vrsel1.new, 
               p = 1, 
               type = "const",
               season = NULL)
#summary(var.diff.n)

ru = roots(var.diff.n)

var1.norm.n = normality.test(var.diff.n, 
                             multivariate.only = T)

var1.causal.n = causality(var.diff.n, 
                        cause = c("lco2"))

#forecast
predicci = predict(var.diff.n, 
                   n.ahead = 5,
                   ci = 0.95)

#--------------------------------------
ir.dlco2 = irf(var.diff.n, 
               impulse = "lco2",
               response = c("lener", "lgdp"),
               n.ahead = 5,
               ortho = F,
               boot = F)
#--------------------------------------


#respuesta al impulso dlco2
ir.dlco2 = irf(var.diff.n, 
               impulse = "dlco2.n",
               response = c("dlener.n", "dlgdp.n"),
               n.ahead = 5,
               ortho = F,
               boot = F)

ir.all = irf(var.diff.n, 
             impulse = c("dlener.n", "dlgdp.n"),
             response = "dlco2.n",
             n.ahead = 5,
             ortho = F,
             boot = F)

# descomposicion de la varianza
var1.descomp = fevd(var.diff.n,
                    n.ahead = 5)
plot(var1.descomp)


########
#######3
#######            VEC

#rates.co <- ca.jo(vrsel1.new, 
#                  type = "eigen", 
#                  ecdet = "const", 
#                  K = 1, 
#                  spec = 'transitory', 
#                  season = NULL)
#summary(rates.co) ## si cuadra stata
#
#vecm1<-cajorls(rates.co, r=1) ## si cuadra stata
#vecm1

niveles = dplyr::select(db.new, EN.ATM.CO2E.PC, EG.USE.PCAP.KG.OE, NY.GDP.PCAP.KD) 
opt.n = VARselect(niveles, lag.max = 4, type = "const")
opt$selection
var.nivel = VAR(niveles, 
                 p = 1, 
                 type = "const",
                 season = NULL)
ru.n = roots(var.nivel)

sc.nivel = serial.test(var.nivel)
plot(sc.nivel)



#https://www.youtube.com/watch?v=l-0DgBzdKLQ
#la desviación del año anterior del equilibrio a largo plazo se corrige en el período actual como una velocidad de ajuste del 6,7% 
#
#un cambio porcentual en el PCI se asocia con un aumento del 0,44% en el PDA en promedio ceteris paribus a corto plazo 
#
#JOJANSEN - LARGO PLAZO
#https://www.youtube.com/watch?v=syP7JiUkMng
#El CDP tiene un efecto positivo en el PDI 
#El PIB tiene un efecto negativo en el PDI 
#
