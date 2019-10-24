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
#pelo import Dataser selecionar a planilha que contém os dados desejados
#selecionar a linha de dados que vou trabalhar da planinha
Bit=IND[,2]
Bit



#afirmar que é uma série temporal
Bit=as.timeSeries(Bit)
ts(Bit)
#estatisticas descritívas das séries
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
# hipótese do teste, h1 estacionário,
# h0 nãoe stacionário. ( p-valor pequeno aceita h1)
pp.test(as.timeSeries(dados))



#####################################################
#Graficos
###################################################
plot(ts(dados),main="Criptomoeda Bitcoin (US$)",cex.main=1.4, cex.lab=1.8,xlab="Tempo",ylab="Retorno")
grid()


#funções de autocorrelação (identificação do modelo)
# plotando autocorrelacao e autocorrelacao parcial

par(mfrow=c(1,2))#dois graficos por janela
figura=acf(dados,plot=F)
plot(figura, ylim=c(-1,1))
pacf(dados,ylim=c(-1,1))





##################################################################
#função ou gere automaticamente um conjunto ideal (p, d, q) 
##################################################################
fit<-auto.arima(dados, seasonal=FALSE)
fit
#podemos colocar o compoennte sazonal ou não
#seasonal=FALSE
#seasonal=TRUE
#caso você queira colocar a ordem diretametne
fit2 = Arima(ts(dados), order=c(3,0,1))
fit2




##################################################################
#análise residual
##################################################################

#Se os parâmetros e a estrutura da ordem do modelo forem especificados corretamente, 
#não esperaríamos a presença de autocorrelações significativas. 
tsdisplay(residuals(fit), lag.max=45, main='(1,1,1) Model Residuals') 
checkresiduals(fit)
residuals(fit)
autoplot(fit)
#pontos vermelhos no gráfico correspondem às raízes dos polinômios


#teste de normalidade
shapiro.test(residuals(fit))
#o valor p> 0,05 implica que a distribuição dos dados não é 
#significativamente diferente da distribuição normal.
qqnorm(residuals(fit));qqline(words1, col = 2)

#caso eu considere retirar os outlier (Outliers são observações muito diferentes da maioria das observações na série temporal.
#Eles podem ser erros ou podem ser simplesmente incomuns)
#dados=tsclean(dados)

#teste de autocorrelação
#h0  os resíduos são iid
Box.test(residuals(fit),lag=1) 
Box.test((residuals(fit))^2 ,lag=1)



#teste de heterocedásticidade
#O teste de Phillips - Perron, conhecido na literatura como teste PP é
#uma generalização do teste de Dickley - Fuller 
PP.test(residuals(fit))
#hipótese alternativa, o resíduo é o ruído branco não possui raíz unitária.


##################################################################
#Previsão
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
#modelando a dependencia não linear
##################################################################
require(fGarch)
require(fArma)
require(akima)
require(dynamo)

Resíduo=residuals(fit)
GA=garch(Resíduo,order=c(2,1))
summary(GA)

GA2=garch(Resíduo,order=c(1,1))
summary(GA2)


ResíduosGARCH=GA2$residuals
ResíduosGARCH= ResíduosGARCH[which(ResíduosGARCH!="NA")] 
Box.test(ResíduosGARCH)

ResíduosGARCHQ = ResíduosGARCHQ [which(ResíduosGARCHQ !="NA")] 
Box.test(ResíduosGARCH) 
shapiro.test(ResíduosGARCH)

ResíduosGARCHQ=ResíduosGARCH^2
ResíduosGARCHQ = ResíduosGARCHQ [which(ResíduosGARCHQ !="NA")] 

Box.test(ResíduosGARCHQ) 
shapiro.test(ResíduosGARCHQ)

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
#Análise Bayesiana de um Modelo Autoregressivo de Vetor com
# Volatilidade Estocástica e Parâmetros de Variação Temporal
##################################################################


require(bvarsv)
myvector=data.frame(serie1,serie2,serie3)
ts1=ts(myvector, start=c(2013, 1), end=c(2018, 11), frequency=12) 

set.seed(1)
#um Modelo Autoregressivo Vetorial com Estocástico
#Volatilidade e parâmetros variáveis no tempo
fit=bvar.sv.tvp(ts1)
fit=bvar.sv.tvp(ts1, p = 1, tau = 40, nf = 10, pdrift = TRUE, nrep = 50000,
                nburn = 5000, thinfac = 10, itprint = 10000, save.parameters = TRUE,
                k_B = 4, k_A = 4, k_sig = 1, k_Q = 0.01, k_S = 0.1, k_W = 0.01,
                pQ = NULL, pW = NULL, pS = NULL)

#data(usmacro)
#set.seed(12)
#fit=bvar.sv.tvp(ts1, p = 2)
#ts1 - Matriz  de  dados , onde as  linhas  representam o  tempo  e as  colunas  são  variáveis diferentes  .deve ter pelo menos duas colunas .}     
#Comprimento de atraso  , maior ou igual a 1 ( o padrão )}   
#tau - Comprimento  da formação de amostra utilizada para a determinação anteriores parâmetros através menos quadrados ( LS ). Ou seja , dados em \ code { Y [ 1 : tau]} são usados para estimar parâmetros anteriores via LS ; A análise bayesiana formal é então realizada para dados  em \ code { Y [( tau + 1 ) : nrow ( Y ),]}.}
#nf - Número  de  períodos de tempo futuros  para os quais as previsões são calculadas ( número inteiro , 1 ou maior , o padrão é 10 ).}         
# pdrift -  Dummy, indica  se  ou  não  a  conta  de  parâmetro  deriva  quando  simulando previsões ( defaults  para  VERDADEIRO ).}
#nrep  -  Número  de  empates do MCMC  excluindo o burn - in (o padrão é 50000 )}    
#nburn  - Número  de  desenhos do MCMC  usados para inicializar o amostrador (o padrão é 5000 ). Estes chama que não entra no cálculo de posteriores momentos , previsões etc .}                
#thinfac - Fator de diluição  para saída do MCMC . Padrões para 10 , o que significa que os previsão sequências (\ código { fc.mdraws }, \ código { fc.vdraws }, \ código { fc.ydraws }, ver abaixo ) conter unicamente cada décimo sorteio do originais sequência . Set \ code                  { thinfac } para  um  para  obter  a  sequência completa do  MCMC  .}
# itprint - Imprimir  cada \ code { itprint } - th  iteração . Padrões  para  10000 . Definir  a  muito  grande  valor  para  omitir  a impressão  completamente .}
#save.parameters - Se  definir  a \ code { VERDADEIRO }, parâmetro  sorteios  são  salvas  em listas ( estes  podem  ser  muito  grande ). Padrões  para \ code { TRUE }.}
#k_B , k_A , k_sig , k_Q , k_W , k_S , pQ , pW , pS } { Quantidades  que  entram  nas  distribuições anteriores  , veja os links abaixo para detalhes . Padrões para os exatos valores usados na origem artigo por Primiceri .}                
################
#Saídas:
################   
#Beta.postmean - 
#Posterior  significa  de  coeficientes . Esta  é  uma  matriz  de  dimensão \ eqn {[ M , Mp + 1 , T ]}, em que \ eqn { T } denota  o  número  de  períodos de tempo ( =  número  de  linhas  de \ code { Y }) e \ eqn{ M } denota  o  número  de  variáveis do sistema ( =  número  de  colunas  de \ code { Y }). A  submatriz \ eqn {[ ,, t ]} representa  a  matriz de coeficientes  no tempo \ eqn { t }. A intercepção vector é empilhado na primeira coluna ; o \ código          { p } matrizes coeficientes  de dimensão \ eqn {[ M , M ]} são colocadas próximas a ela .} 
fit$Beta.postmean
write.table(fit$Beta.postmean, file='betateste.csv')
#H.postmean  -  Meios posteriores  de matrizes de covariância de termo de erro . Esta é uma matriz de dimensão \ eqn {[ M , M , T ]}. A submatriz \ eqn {[ ,, t ]} representa a matriz de covariância no tempo \ eqn { t }.}                
fit$H.postmean
#Q.postmean , S.postmean , W.postmean } { Posterior  significa  de  vários  covariância  matrizes .}
fit$Q.postmean
#fc.ydraws -  Observações futuras simuladas  . Design análogo ao \ code { fc.mdraws }.}  
fit$fc.ydraws

predictive.density(fit, v = 1, h = 1, cdf = FALSE)
predictive.draws(fit, v = 1, h = 1)
parameter.draws(fit, type = "lag1", row = 1, col = 1)

parameter.draws(fit, type = "vcv", row = 1, col = 1)
#vcv" retorna #desenha para os elementos da matriz residual de variância-covariância.

impulse.responses(fit)
#Calcula as funções de resposta a impulso (IRFs) de um ajuste de modelo produzido por bvar.sv.tvp. O IRF
#descreva como uma variável responde a um choque em outra variável, nos períodos seguintes ao choque.
#Para habilitar o manuseio simples, essa função calcula os IRFs para apenas um par de variáveis que devem ser
#especificado previamente (consulte impulse.variable e response.variable abaixo).
impulse.responses(fit, impulse.variable = 2, response.variable = 1,
                  t = NULL, nhor = 20, scenario = 2, draw.plot = TRUE)
impulse.responses(fit, impulse.variable = 3, response.variable = 1,
                  t = NULL, nhor = 20, scenario = 2, draw.plot = TRUE)
impulse.responses(fit, impulse.variable = 4, response.variable = 1,
                  t = NULL, nhor = 20, scenario = 2, draw.plot = TRUE)
impulse.responses(fit, impulse.variable = 5, response.variable = 1,
                  t = NULL, nhor = 20, scenario = 2, draw.plot = TRUE)



