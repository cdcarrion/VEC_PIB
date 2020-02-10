################## paper peru
################## PARA R SE USAN LAS VARIABLES NIVELES, EN STATA EN en niveles

dbperu = readxl::read_xlsx("eco2/trabajo_eco2/peru.xlsx")
dpbi = diff(dbperu$pbi)
dpbman = diff(dbperu$pbiman)
dxmin = diff(dbperu$xmin)
dperu = data.frame(dpbi ,
                      dpbman,
                      dxmin)
level.peru = dbperu[2:4]
#write.csv(ch.new, "eco2/trabajo_eco2/ch.csv")

lch.new = dplyr::select(dbperu, pbi, pbiman, xmin) 

t1 = adf.test(dbperu$pbi    , k=0)
t2 = adf.test(dbperu$pbiman , k=0)
t3 = adf.test(dbperu$xmin    , k=0)
t4 = adf.test(dpbi, k=0)
t5 = adf.test(dpbman, k=0)
t6 = adf.test(dxmin, k=1)

t7 = pp.test(dbperu$pbi)
t8 = pp.test(dbperu$pbiman)
t9 = pp.test(dbperu$xmin)
t10 = pp.test(dpbi)
t11 = pp.test(dpbman)
t12 = pp.test(dxmin)

tab1 <- map_df(list(t1, t2, t3, t4, t5, t6), tidy)
tab1$tt = with(tab1, ifelse(p.value < 0.05, "Estacionario", "No Estacionario"))
arrow1 <- make_qi_row(tab1, tab1$statistic, "Army Rule (E)")


opt.ch = VARselect(level.peru, lag.max = 2, type = "const") ###  el var(3).. lag=3
opt.ch$selection

C3 <- VECM(level.peru, lag = 2, estim = "ML", include = 'const')
#VECM(level.peru, lag = 2, estim = "ML", include = 'const', r=2)  ##cuadra con paper
summary(rank.test(C3))
####---  
##### ?VECM
##### var lag -1
########### Basically, a VAR with 2 lags corresponds here to a VECM with 1 lag. 
C3.n <- VECM(level.peru, lag = 2, estim = "ML", include = 'const', r=2)
C3.n$coefficients  
t(C3.n$model.specific$beta) #transpuesta
pred_C3 <- predict(C3.n, n.ahead = 3)
ee4 = rbind(level.peru, pred_C3)
matplot(ee4, type = "l")
plot(ee4$pbi, type = "l")
par(new = TRUE)
plot(level.peru$pbi, type = "l", lty = 2, col = "red")


#https://stats.stackexchange.com/questions/203614/vecm-for-2-stationary-and-1-integrated-series?rq=1
#AQUI SE UNA EL NUMERO DE LAGS USADO EN VARselect +1    !!!!!!
rates.co <- ca.jo(level.peru, 
                  type = "eigen", 
                  ecdet = "const", 
                  K = 3,   ####  VARSELECT +1
                  spec = 'transitory', 
                  season = NULL)
summary(rates.co) ## 

vecm_1 = round(C3.n$model.specific$coint, 2)
vecm1<-cajorls(rates.co, r=2)
vecm1
summary(cajools(rates.co, r=2))
veclevel = vec2var(rates.co, r=2)
eigen(veclevel$A$A1) #https://stats.stackexchange.com/questions/169209/how-to-check-stability-condition-of-vec-estimates-in-r
st = serial.test(veclevel, type = "BG")
#plot(st)
arch.test(veclevel)
normality.test(veclevel)

pred = predict(veclevel,  n.ahead=1)
plot(pred)

irf.pr = irf(veclevel, n.ahead = 1)
imp_resp = irf(veclevel,
               n.ahead = 5,
   #            response = "dlgdp",
   #            impulse = "dlgdp", 
               response =   c ("pbi"), 
               boot = T) # intervalo
plot(irf.pr)
plot(imp_resp)


# causaldiad granger###
#library(egcm)
#plot(egcm(level.peru$pbi, level.peru$xmin))
#
#library(MSBVAR)
#grangertest(level.peru$pbi ~ level.peru$xmin, order = 2)
