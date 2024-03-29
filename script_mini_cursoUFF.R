##############
# Inicio do programa
##############

# carregando pacotes

library(moments)
library(nortest)
library(tseries)
library(lmtest)
library(fUnitRoots)
library(forecast)
library(tseries)
library(ggplot2)
library('xts')
library('forecast');
library('fma')
library('expsmooth')
library('lmtest')
library('tseries')
library('Quandl')
library('fpp');
library('urca')
library(readxl)

#install.packages (tambpem pode ser usado para isntalar pacotes)
#carregar os pacotes

require(moments)
require(nortest)
require(tseries)
require(lmtest)
require(forecast)
require(ggplot2)
require(xts)
require(timeSeries)
require(seasonal)


library(readxl)
IND <- read_excel("D:/GOOGLE D R I V E/minicurosUFF/DadosReais/IND.xlsx", 
                  col_types = c("date", "numeric"))
View(IND)


#pelo R studio
#pelo import Dataser selecionar a planilha que cont�m os dados desejados
#selecionar a linha de dados que vou trabalhar da planinha
Bit=IND[,2]
Bit



#afirmar que � uma s�rie temporal
Bit=as.timeSeries(Bit)
ts(Bit)
#estatisticas descrit�vas das s�ries
summary(Bit)


plot(ts(Bit),main="Criptomoeda Bitcoin (US$)",cex.main=1.4, cex.lab=1.8,xlab="Tempo",ylab="Retorno")
grid()


########################
# log-retornos diarios
########################
#transformando uma serie temporal em um vetor
dados=c(Bit) 

# Precos em t
Pt<-dados[2:length(dados)]
# Precos em t-1
Pt_1<-dados[1:(length(dados)-1)]
dados<-log(Pt/Pt_1)



#testes de estacionariedade
adf.test(dados)  

#teste de estacionariedade
# hip�tese do teste, h1 estacion�rio,
# h0 n�oe stacion�rio. ( p-valor pequeno aceita h1)
pp.test(as.timeSeries(dados))



#####################################################
#Graficos
###################################################
plot(ts(dados),main="Criptomoeda Bitcoin (US$)",cex.main=1.4, cex.lab=1.8,xlab="Tempo",ylab="Retorno")
grid()


#fun��es de autocorrela��o (identifica��o do modelo)
# plotando autocorrelacao e autocorrelacao parcial

par(mfrow=c(1,2))#dois graficos por janela
figura=acf(dados,plot=F)
plot(figura, ylim=c(-1,1))
pacf(dados,ylim=c(-1,1))





##################################################################
#fun��o ou gere automaticamente um conjunto ideal (p, d, q) 
##################################################################
fit<-auto.arima(dados, seasonal=FALSE)
fit
#podemos colocar o compoennte sazonal ou n�o
#seasonal=FALSE
#seasonal=TRUE
#caso voc� queira colocar a ordem diretametne
fit2 = Arima(ts(dados), order=c(3,0,1))
fit2




##################################################################
#an�lise residual
##################################################################

#Se os par�metros e a estrutura da ordem do modelo forem especificados corretamente, 
#n�o esperar�amos a presen�a de autocorrela��es significativas. 
tsdisplay(residuals(fit), lag.max=45, main='(1,1,1) Model Residuals') 
checkresiduals(fit)
residuals(fit)
autoplot(fit)
#pontos vermelhos no gr�fico correspondem �s ra�zes dos polin�mios


#teste de normalidade
shapiro.test(residuals(fit))
#o valor p> 0,05 implica que a distribui��o dos dados n�o � 
#significativamente diferente da distribui��o normal.
qqnorm(residuals(fit));qqline(words1, col = 2)

#caso eu considere retirar os outlier (Outliers s�o observa��es muito diferentes da maioria das observa��es na s�rie temporal.
#Eles podem ser erros ou podem ser simplesmente incomuns)
#dados=tsclean(dados)

#teste de autocorrela��o
#h0  os res�duos s�o iid
Box.test(residuals(fit),lag=1) 
Box.test((residuals(fit))^2 ,lag=1)



#teste de heteroced�sticidade
#O teste de Phillips - Perron, conhecido na literatura como teste PP �
#uma generaliza��o do teste de Dickley - Fuller 
PP.test(residuals(fit))
#hip�tese alternativa, o res�duo � o ru�do branco n�o possui ra�z unit�ria.


##################################################################
#Previs�o
##################################################################

fcast <- forecast(fit, h=30)
plot(fcast)


##################################################################
##################################################################
##################################################################
##################################################################






#material extra:
#https://otexts.com/fpp2/hierarchical.html

##################################################################
#modelando a dependencia n�o linear
##################################################################
require(fGarch)
require(fArma)
require(akima)
require(dynamo)

Res�duo=residuals(fit)
GA=garch(Res�duo,order=c(2,1))
summary(GA)

GA2=garch(Res�duo,order=c(1,1))
summary(GA2)


Res�duosGARCH=GA2$residuals
Res�duosGARCH= Res�duosGARCH[which(Res�duosGARCH!="NA")] 
Box.test(Res�duosGARCH)

Res�duosGARCHQ = Res�duosGARCHQ [which(Res�duosGARCHQ !="NA")] 
Box.test(Res�duosGARCH) 
shapiro.test(Res�duosGARCH)

Res�duosGARCHQ=Res�duosGARCH^2
Res�duosGARCHQ = Res�duosGARCHQ [which(Res�duosGARCHQ !="NA")] 

Box.test(Res�duosGARCHQ) 
shapiro.test(Res�duosGARCHQ)

##################################################################
# Modelo Autoregressivo vetorial (VAR)
##################################################################
#https://rdrr.io/cran/vars/f/inst/doc/vars.pdf

myvector=data.frame(PIB,Renda,INFRA,COM,FBCF,TRANS)
ts1=ts(myvector, start=c(2013, 1), end=c(2018, 11), frequency=12) 
VAR(y, p = 1, type = c("const", "trend", "both", "none"),
    season = NULL, exogen = NULL, lag.max = NULL,
    ic = c("AIC", "HQ", "SC", "FPE"))





##################################################################
#An�lise Bayesiana de um Modelo Autoregressivo de Vetor com
# Volatilidade Estoc�stica e Par�metros de Varia��o Temporal
##################################################################


require(bvarsv)
myvector=data.frame(serie1,serie2,serie3)
ts1=ts(myvector, start=c(2013, 1), end=c(2018, 11), frequency=12) 

set.seed(1)
#um Modelo Autoregressivo Vetorial com Estoc�stico
#Volatilidade e par�metros vari�veis no tempo
fit=bvar.sv.tvp(ts1)
fit=bvar.sv.tvp(ts1, p = 1, tau = 40, nf = 10, pdrift = TRUE, nrep = 50000,
                nburn = 5000, thinfac = 10, itprint = 10000, save.parameters = TRUE,
                k_B = 4, k_A = 4, k_sig = 1, k_Q = 0.01, k_S = 0.1, k_W = 0.01,
                pQ = NULL, pW = NULL, pS = NULL)

#data(usmacro)
#set.seed(12)
#fit=bvar.sv.tvp(ts1, p = 2)
#ts1 - Matriz  de  dados , onde as  linhas  representam o  tempo  e as  colunas  s�o  vari�veis diferentes  .deve ter pelo menos duas colunas .}     
#Comprimento de atraso  , maior ou igual a 1 ( o padr�o )}   
#tau - Comprimento  da forma��o de amostra utilizada para a determina��o anteriores par�metros atrav�s menos quadrados ( LS ). Ou seja , dados em \ code { Y [ 1 : tau]} s�o usados para estimar par�metros anteriores via LS ; A an�lise bayesiana formal � ent�o realizada para dados  em \ code { Y [( tau + 1 ) : nrow ( Y ),]}.}
#nf - N�mero  de  per�odos de tempo futuros  para os quais as previs�es s�o calculadas ( n�mero inteiro , 1 ou maior , o padr�o � 10 ).}         
# pdrift -  Dummy, indica  se  ou  n�o  a  conta  de  par�metro  deriva  quando  simulando previs�es ( defaults  para  VERDADEIRO ).}
#nrep  -  N�mero  de  empates do MCMC  excluindo o burn - in (o padr�o � 50000 )}    
#nburn  - N�mero  de  desenhos do MCMC  usados para inicializar o amostrador (o padr�o � 5000 ). Estes chama que n�o entra no c�lculo de posteriores momentos , previs�es etc .}                
#thinfac - Fator de dilui��o  para sa�da do MCMC . Padr�es para 10 , o que significa que os previs�o sequ�ncias (\ c�digo { fc.mdraws }, \ c�digo { fc.vdraws }, \ c�digo { fc.ydraws }, ver abaixo ) conter unicamente cada d�cimo sorteio do originais sequ�ncia . Set \ code                  { thinfac } para  um  para  obter  a  sequ�ncia completa do  MCMC  .}
# itprint - Imprimir  cada \ code { itprint } - th  itera��o . Padr�es  para  10000 . Definir  a  muito  grande  valor  para  omitir  a impress�o  completamente .}
#save.parameters - Se  definir  a \ code { VERDADEIRO }, par�metro  sorteios  s�o  salvas  em listas ( estes  podem  ser  muito  grande ). Padr�es  para \ code { TRUE }.}
#k_B , k_A , k_sig , k_Q , k_W , k_S , pQ , pW , pS } { Quantidades  que  entram  nas  distribui��es anteriores  , veja os links abaixo para detalhes . Padr�es para os exatos valores usados na origem artigo por Primiceri .}                
################
#Sa�das:
################   
#Beta.postmean - 
#Posterior  significa  de  coeficientes . Esta  �  uma  matriz  de  dimens�o \ eqn {[ M , Mp + 1 , T ]}, em que \ eqn { T } denota  o  n�mero  de  per�odos de tempo ( =  n�mero  de  linhas  de \ code { Y }) e \ eqn{ M } denota  o  n�mero  de  vari�veis do sistema ( =  n�mero  de  colunas  de \ code { Y }). A  submatriz \ eqn {[ ,, t ]} representa  a  matriz de coeficientes  no tempo \ eqn { t }. A intercep��o vector � empilhado na primeira coluna ; o \ c�digo          { p } matrizes coeficientes  de dimens�o \ eqn {[ M , M ]} s�o colocadas pr�ximas a ela .} 
fit$Beta.postmean
write.table(fit$Beta.postmean, file='betateste.csv')
#H.postmean  -  Meios posteriores  de matrizes de covari�ncia de termo de erro . Esta � uma matriz de dimens�o \ eqn {[ M , M , T ]}. A submatriz \ eqn {[ ,, t ]} representa a matriz de covari�ncia no tempo \ eqn { t }.}                
fit$H.postmean
#Q.postmean , S.postmean , W.postmean } { Posterior  significa  de  v�rios  covari�ncia  matrizes .}
fit$Q.postmean
#fc.ydraws -  Observa��es futuras simuladas  . Design an�logo ao \ code { fc.mdraws }.}  
fit$fc.ydraws

predictive.density(fit, v = 1, h = 1, cdf = FALSE)
predictive.draws(fit, v = 1, h = 1)
parameter.draws(fit, type = "lag1", row = 1, col = 1)

parameter.draws(fit, type = "vcv", row = 1, col = 1)
#vcv" retorna #desenha para os elementos da matriz residual de vari�ncia-covari�ncia.

impulse.responses(fit)
#Calcula as fun��es de resposta a impulso (IRFs) de um ajuste de modelo produzido por bvar.sv.tvp. O IRF
#descreva como uma vari�vel responde a um choque em outra vari�vel, nos per�odos seguintes ao choque.
#Para habilitar o manuseio simples, essa fun��o calcula os IRFs para apenas um par de vari�veis que devem ser
#especificado previamente (consulte impulse.variable e response.variable abaixo).
impulse.responses(fit, impulse.variable = 2, response.variable = 1,
                  t = NULL, nhor = 20, scenario = 2, draw.plot = TRUE)
impulse.responses(fit, impulse.variable = 3, response.variable = 1,
                  t = NULL, nhor = 20, scenario = 2, draw.plot = TRUE)
impulse.responses(fit, impulse.variable = 4, response.variable = 1,
                  t = NULL, nhor = 20, scenario = 2, draw.plot = TRUE)
impulse.responses(fit, impulse.variable = 5, response.variable = 1,
                  t = NULL, nhor = 20, scenario = 2, draw.plot = TRUE)



