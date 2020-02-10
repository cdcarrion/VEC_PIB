import delimited C:\Users\crist\Documents\eco2\trabajo_eco2\ddnewIT.csv, clear 

tsset year
gen dlco2 = d.lco2
gen dlener = d.lener
gen dgdp = d.lgdp
varsoc lco2 lener lgdp  // lag 1

varbasic lco2 lener lgdp , lags(1)

varstable, graph

varlmar //no autocorrelacion en residuos

varwle //ex post de significacion de los rezagos
vargranger

** pronostico
quietly var lco2 lener lgdp, lags(1)
predict f_lco2 
predict test1, equation(#1)
predict test2, equation( dco2 )
label variable f_dco2 "forecast"
compare f_dco2 test1
drop test1 test2
tsline lco2 f_lco2 if year>2000, lpattern(solid dash)
tsline lco2 f_lco2, lpattern(solid dash)
predict s_lco2, equation(lco2) stdp
generate l_lco2 = f_lco2 - 2*s_lco2
generate u_lco2 = f_lco2+ 2*s_lco2
twoway (rarea u_dco2 l_dco2 year, fintensity(inten10))
twoway (rarea u_lco2 l_lco2 year, fintensity(inten10)) (line lco2 f_lco2 year, lpattern(solid dash)) in -40/l, yline(0) legend(order(2 3 1) label (1 “95% CI”))



//////// CHILE
import delimited C:\Users\crist\Documents\eco2\trabajo_eco2\ch.csv, clear 
tsset year
gen dlgdp = d.lgdp
gen dlind = d.lind
gen dlmet = d.lmet
vecrank dlgdp dlind dlmet
varsoc dlgdp dlind dlmet, maxlag(8)
vecrank dlgdp dlind dlmet, lags(8)
vec dlgdp dlind dlmet, lag(1)
vecrank dlgdp dlind dlmet, max
vecstable, graph
veclmar

test ([D_dpbi]: LD.dxmin)
test ([D_dpbi]: LD.dpbiman)


//////////7 PERU
import excel "C:\Users\crist\Documents\eco2\trabajo_eco2\peru.xlsx", sheet("peru") firstrow
tsset year
		/// AQUI NO SE NECESITA DIFERENCIAR... EL PROGRAMA YA DIFERENCIA!!!!
// gen dpbi = d.pbi
// gen dpbiman = d.pbiman
// gen dxmin = d.xmin
varsoc pbi pbiman xmin , maxlag(8)
vecrank pbi pbiman xmin
vec pbi pbiman xmin , rank(2) lags(3)
vecstable, graph
dfuller dpbiman

