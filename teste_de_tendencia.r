#######################################################
###############TESTES PRE TREND E NO ANTECIPATION######
#######################################################

#########################################################################
###############considerando todas observações da base####################
#########################################################################
#Gráficos de pre trend para valor médio de lançamento
load("~/ws_final.RData")

boxplot((merge_data_final$PC_TT_UN~merge_data_final$ANO.DO.EXERCICIO))

par(mfrow=c(2,1))

preco_lan_medio_1995=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1995])
preco_lan_medio_1996=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1996])
preco_lan_medio_1997=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1997])
preco_lan_medio_1998=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1998])
preco_lan_medio_1999=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1999])
preco_lan_medio_2000=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2000])
preco_lan_medio_2001=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2001])
preco_lan_medio_2002=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2002])
preco_lan_medio_2003=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2003])
preco_lan_medio_2004=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2004])
preco_lan_medio_2005=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2005])
preco_lan_medio_2006=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2006])
preco_lan_medio_2007=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2007])
preco_lan_medio_2008=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2008])

lowerbound_1995=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1995])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1995])
lowerbound_1996=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1996])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1996])
lowerbound_1997=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1997])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1997])
lowerbound_1998=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1998])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1998])
lowerbound_1999=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1999])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1999])
lowerbound_2000=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2000])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2000])
lowerbound_2001=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2001])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2001])
lowerbound_2002=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2002])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2002])
lowerbound_2003=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2003])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2003])
lowerbound_2004=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2004])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2004])
lowerbound_2005=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2005])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2005])
lowerbound_2006=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2006])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2006])
lowerbound_2007=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2007])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2007])
lowerbound_2008=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2008])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2008])


upperbound_1995=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1995])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1995])
upperbound_1996=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1996])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1996])
upperbound_1997=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1997])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1997])
upperbound_1998=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1998])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1998])
upperbound_1999=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1999])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1999])
upperbound_2000=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2000])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2000])
upperbound_2001=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2001])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2001])
upperbound_2002=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2002])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2002])
upperbound_2003=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2003])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2003])
upperbound_2004=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2004])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2004])
upperbound_2005=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2005])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2005])
upperbound_2006=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2006])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2006])
upperbound_2007=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2007])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2007])
upperbound_2008=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2008])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2008])


vetor_precos_lan_tempo=c(preco_lan_medio_1995,preco_lan_medio_1996,preco_lan_medio_1997,preco_lan_medio_1998,preco_lan_medio_1999,
                         preco_lan_medio_2000,preco_lan_medio_2001,preco_lan_medio_2002,
                         preco_lan_medio_2003,preco_lan_medio_2004,preco_lan_medio_2005,
                         preco_lan_medio_2006,preco_lan_medio_2007,preco_lan_medio_2008)
vetor_precos_lan_tempo_lowerbound=c(lowerbound_1995,lowerbound_1996,lowerbound_1997,lowerbound_1998,lowerbound_1999,
                                    lowerbound_2000,lowerbound_2001,lowerbound_2002,
                                    lowerbound_2003,lowerbound_2004,lowerbound_2005,
                                    lowerbound_2006,lowerbound_2007,lowerbound_2008)
vetor_precos_lan_tempo_upperbound=c(upperbound_1995,upperbound_1996,upperbound_1997,upperbound_1998,upperbound_1999,
                                    upperbound_2000,upperbound_2001,upperbound_2002,
                                    upperbound_2003,upperbound_2004,upperbound_2005,upperbound_2006
                                    ,upperbound_2007,upperbound_2008)
anos=c(1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)

###Sem corrigir inflação
plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Valor médio de lançamento do Imóvel por ano")
lines(y=vetor_precos_lan_tempo,x=anos,col="red")
plot(y=vetor_precos_lan_tempo,x=anos,ylim = c(50000,350000),pch=21,main = "Valor médio de lançamento do Imóvel por ano com limites de 1/2 desvio")
lines(y=vetor_precos_lan_tempo,x=anos,col="red")
#legend(1997,1900000,legend = c("x: Limite superior","+: Limite inferior"))
points(x=anos,y=vetor_precos_lan_tempo_lowerbound,pch=3)
points(x=anos,y=vetor_precos_lan_tempo_upperbound,pch=4)


#PT em ratio
vetor_precos_lan_tempo=c(preco_lan_medio_1996/preco_lan_medio_1995,preco_lan_medio_1997/preco_lan_medio_1996,
                         preco_lan_medio_1998/preco_lan_medio_1997,preco_lan_medio_1999/preco_lan_medio_1998,
                         preco_lan_medio_2000/preco_lan_medio_1999,preco_lan_medio_2001/preco_lan_medio_2000,
                         preco_lan_medio_2002/preco_lan_medio_2001,preco_lan_medio_2003/preco_lan_medio_2002,
                         preco_lan_medio_2004/preco_lan_medio_2003, preco_lan_medio_2005/preco_lan_medio_2004,
                         preco_lan_medio_2006/preco_lan_medio_2005, preco_lan_medio_2007/preco_lan_medio_2006,
                         preco_lan_medio_2008/preco_lan_medio_2007)
anos=c(1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)

plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Taxa anual de crescimento do preço de lançamento")
lines(y=vetor_precos_lan_tempo,x=anos,col="red")


#ln()
vetor_precos_lan_tempo=c(log(preco_lan_medio_1995),log(preco_lan_medio_1996),log(preco_lan_medio_1997),log(preco_lan_medio_1998),log(preco_lan_medio_1999),
                         log(preco_lan_medio_2000),log(preco_lan_medio_2001),
                         log(preco_lan_medio_2002),log(preco_lan_medio_2003),log(preco_lan_medio_2004),
                         log(preco_lan_medio_2005),log(preco_lan_medio_2006),log(preco_lan_medio_2007),
                         log(preco_lan_medio_2008))
anos=c(1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)

plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Log da média do preço")
lines(y=vetor_precos_lan_tempo,x=anos,col="red")


#Corrigindo inflação
preco_lan_medio_1997=preco_lan_medio_1997-preco_lan_medio_1997*0
preco_lan_medio_1998=preco_lan_medio_1998-preco_lan_medio_1997*0.0165
preco_lan_medio_1999=preco_lan_medio_1999-preco_lan_medio_1997*0.1074
preco_lan_medio_2000=preco_lan_medio_2000-preco_lan_medio_1997*0.1735
preco_lan_medio_2001=preco_lan_medio_2001-preco_lan_medio_1997*0.2635
preco_lan_medio_2002=preco_lan_medio_2002-preco_lan_medio_1997*0.4218
preco_lan_medio_2003=preco_lan_medio_2003-preco_lan_medio_1997*0.5540
preco_lan_medio_2004=preco_lan_medio_2004-preco_lan_medio_1997*0.6721
preco_lan_medio_2005=preco_lan_medio_2005-preco_lan_medio_1997*0.7672
preco_lan_medio_2006=preco_lan_medio_2006-preco_lan_medio_1997*0.8227
preco_lan_medio_2007=preco_lan_medio_2007-preco_lan_medio_1997*0.9040
preco_lan_medio_2008=preco_lan_medio_2008-preco_lan_medio_1997*1.016

lowerbound_1997=lowerbound_1997-lowerbound_1997*0
lowerbound_1998=lowerbound_1998-lowerbound_1997*0.0165
lowerbound_1999=lowerbound_1999-lowerbound_1997*0.1074
lowerbound_2000=lowerbound_2000-lowerbound_1997*0.1735
lowerbound_2001=lowerbound_2001-lowerbound_1997*0.2635
lowerbound_2002=lowerbound_2002-lowerbound_1997*0.4218
lowerbound_2003=lowerbound_2003-lowerbound_1997*0.5540
lowerbound_2004=lowerbound_2004-lowerbound_1997*0.6721
lowerbound_2005=lowerbound_2005-lowerbound_1997*0.7672
lowerbound_2006=lowerbound_2006-lowerbound_1997*0.8227
lowerbound_2007=lowerbound_2007-lowerbound_1997*0.9040
lowerbound_2008=lowerbound_2008-lowerbound_1997*1.016

upperbound_1997=upperbound_1997-upperbound_1997*0
upperbound_1998=upperbound_1998-upperbound_1997*0.0165
upperbound_1999=upperbound_1999-upperbound_1997*0.1074
upperbound_2000=upperbound_2000-upperbound_1997*0.1735
upperbound_2001=upperbound_2001-upperbound_1997*0.2635
upperbound_2002=upperbound_2002-upperbound_1997*0.4218
upperbound_2003=upperbound_2003-upperbound_1997*0.5540
upperbound_2004=upperbound_2004-upperbound_1997*0.6721
upperbound_2005=upperbound_2005-upperbound_1997*0.7672
upperbound_2006=upperbound_2006-upperbound_1997*0.8227
upperbound_2007=upperbound_2007-upperbound_1997*0.9040
upperbound_2008=upperbound_2008-upperbound_1997*1.016

#até 2004
vetor_precos_lan_tempo=c(preco_lan_medio_1997,preco_lan_medio_1998,preco_lan_medio_1998,
                         preco_lan_medio_2000,preco_lan_medio_2001,preco_lan_medio_2002,
                         preco_lan_medio_2003,preco_lan_medio_2004,preco_lan_medio_2005,preco_lan_medio_2006
                         ,preco_lan_medio_2007,preco_lan_medio_2008)
vetor_precos_lan_tempo_lowerbound=c(lowerbound_1997,lowerbound_1998,lowerbound_1999,
                                    lowerbound_2000,lowerbound_2001,lowerbound_2002,
                                    lowerbound_2003,lowerbound_2004,lowerbound_2005,
                                    lowerbound_2006,lowerbound_2007,lowerbound_2008)
vetor_precos_lan_tempo_upperbound=c(upperbound_1997,upperbound_1998,upperbound_1999,
                                    upperbound_2000,upperbound_2001,upperbound_2002,
                                    upperbound_2003,upperbound_2004,upperbound_2005,
                                    upperbound_2006,upperbound_2007,upperbound_2008)
anos=c(1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)

plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Valor médio de lançamento do Imóvel por ano (corrigido IPCA)")
lines(y=vetor_precos_lan_tempo,x=anos,col="red")
plot(y=vetor_precos_lan_tempo,x=anos,ylim = c(50000,210000),pch=21,main = "Valor médio de lançamento do Imóvel por ano com limites de 1 desvio")
lines(y=vetor_precos_lan_tempo,x=anos,col="red")
#legend(1997,600000,legend = c("x: Limite superior","+: Limite inferior"))
points(x=anos,y=vetor_precos_lan_tempo_lowerbound,pch=3)
points(x=anos,y=vetor_precos_lan_tempo_upperbound,pch=4)


#Por bairro
#sub_pref_exclui=c(4,6,7,8)
load("~/ws_final.RData")

for (k in 1:length(levels(merge_data_final$SUBPREF))){
  par(mfrow=c(3,2))
  if(k!=7&&k!=4&&k!=6&&k!=8&&k!=9&&k!=10&&k!=12&&k!=15&&k!=17&&k!=20&&k!=25&&k!=26&&k!=28){
    #Gráficos de pre trend para valor médio de lançamento
    preco_lan_medio_1995=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==1995],na.rm = TRUE)
    preco_lan_medio_1996=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==1996],na.rm = TRUE)
    preco_lan_medio_1997=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==1997],na.rm = TRUE)
    preco_lan_medio_1998=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==1998],na.rm = TRUE)
    preco_lan_medio_1999=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==1999],na.rm = TRUE)
    preco_lan_medio_2000=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2000],na.rm = TRUE)
    preco_lan_medio_2001=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2001],na.rm = TRUE)
    preco_lan_medio_2002=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2002],na.rm = TRUE)
    preco_lan_medio_2003=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2003],na.rm = TRUE)
    preco_lan_medio_2004=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2004],na.rm = TRUE)
    preco_lan_medio_2005=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2005],na.rm = TRUE)
    preco_lan_medio_2006=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2006],na.rm = TRUE)
    preco_lan_medio_2007=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2007],na.rm = TRUE)
    preco_lan_medio_2008=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2008],na.rm = TRUE)
    
    lowerbound_1995=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==1995],na.rm = TRUE)-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==1995],na.rm = TRUE)
    lowerbound_1996=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==1996],na.rm = TRUE)-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==1996],na.rm = TRUE)
    lowerbound_1997=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==1997],na.rm = TRUE)-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==1997],na.rm = TRUE)
    lowerbound_1998=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==1998],na.rm = TRUE)-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==1998],na.rm = TRUE)
    lowerbound_1999=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==1999],na.rm = TRUE)-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==1999],na.rm = TRUE)
    lowerbound_2000=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2000],na.rm = TRUE)-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2000],na.rm = TRUE)
    lowerbound_2001=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2001],na.rm = TRUE)-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2001],na.rm = TRUE)
    lowerbound_2002=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2002],na.rm = TRUE)-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2002],na.rm = TRUE)
    lowerbound_2003=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2003],na.rm = TRUE)-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2003],na.rm = TRUE)
    lowerbound_2004=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2004],na.rm = TRUE)-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2004],na.rm = TRUE)
    lowerbound_2005=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2005],na.rm = TRUE)-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2005],na.rm = TRUE)
    lowerbound_2006=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2006],na.rm = TRUE)-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2006],na.rm = TRUE)
    lowerbound_2007=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2007],na.rm = TRUE)-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2007],na.rm = TRUE)
    lowerbound_2008=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2008],na.rm = TRUE)-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2008],na.rm = TRUE)
    
    
    upperbound_1995=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==1995],na.rm = TRUE)+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==1995],na.rm = TRUE)
    upperbound_1996=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==1996],na.rm = TRUE)+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==1996],na.rm = TRUE)
    upperbound_1997=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==1997],na.rm = TRUE)+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==1997],na.rm = TRUE)
    upperbound_1998=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==1998],na.rm = TRUE)+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==1998],na.rm = TRUE)
    upperbound_1999=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==1999],na.rm = TRUE)+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==1999],na.rm = TRUE)
    upperbound_2000=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2000],na.rm = TRUE)+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2000],na.rm = TRUE)
    upperbound_2001=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2001],na.rm = TRUE)+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2001],na.rm = TRUE)
    upperbound_2002=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2002],na.rm = TRUE)+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2002],na.rm = TRUE)
    upperbound_2003=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2003],na.rm = TRUE)+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2003],na.rm = TRUE)
    upperbound_2004=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2004],na.rm = TRUE)+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2004],na.rm = TRUE)
    upperbound_2005=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2005],na.rm = TRUE)+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2005],na.rm = TRUE)
    upperbound_2006=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2006],na.rm = TRUE)+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2006],na.rm = TRUE)
    upperbound_2007=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2007],na.rm = TRUE)+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2007],na.rm = TRUE)
    upperbound_2008=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2008],na.rm = TRUE)+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$SUBPREF==levels(merge_data_final$SUBPREF)[k]]==2008],na.rm = TRUE)
    
    
    vetor_precos_lan_tempo=c(preco_lan_medio_1995,preco_lan_medio_1996,preco_lan_medio_1997,preco_lan_medio_1998,preco_lan_medio_1999,
                             preco_lan_medio_2000,preco_lan_medio_2001,preco_lan_medio_2002,
                             preco_lan_medio_2003,preco_lan_medio_2004,preco_lan_medio_2005,preco_lan_medio_2006,preco_lan_medio_2007,preco_lan_medio_2008)
    vetor_precos_lan_tempo_lowerbound=c(lowerbound_1995,lowerbound_1996,lowerbound_1997,lowerbound_1998,lowerbound_1999,
                                        lowerbound_2000,lowerbound_2001,lowerbound_2002,
                                        lowerbound_2003,lowerbound_2004,lowerbound_2005,lowerbound_2006,lowerbound_2007,lowerbound_2008)
    vetor_precos_lan_tempo_upperbound=c(upperbound_1995,upperbound_1996,upperbound_1997,upperbound_1998,upperbound_1999,
                                        upperbound_2000,upperbound_2001,upperbound_2002,
                                        upperbound_2003,upperbound_2004,upperbound_2005,upperbound_2006,upperbound_2007,upperbound_2008)
    anos=c(1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)
    
    ###Sem corrigir inflação
    plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = paste("Valor médio de lançamento do Imóvel por ano -",levels(merge_data_final$SUBPREF)[k]))
    lines(y=vetor_precos_lan_tempo,x=anos,col="red")
    plot(y=vetor_precos_lan_tempo,x=anos,pch=21,ylim = c(min(vetor_precos_lan_tempo_lowerbound),max(vetor_precos_lan_tempo_upperbound)),main = paste("Valor médio de lançamento do Imóvel por ano com limites de 1 desvio -",levels(merge_data_final$SUBPREF)[k]))
    #legend(1995,max(vetor_precos_lan_tempo_upperbound),legend = c("x: Limite superior","+: Limite inferior"))
    points(x=anos,y=vetor_precos_lan_tempo_lowerbound,pch=3)
    points(x=anos,y=vetor_precos_lan_tempo_upperbound,pch=4)
    
    
    #PT em ratio
    anos=c(1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)
    vetor_precos_lan_tempo=c(preco_lan_medio_1996/preco_lan_medio_1995,preco_lan_medio_1997/preco_lan_medio_1996,
                             preco_lan_medio_1998/preco_lan_medio_1997,preco_lan_medio_1999/preco_lan_medio_1998,
                             preco_lan_medio_2000/preco_lan_medio_1999,preco_lan_medio_2001/preco_lan_medio_2000,
                             preco_lan_medio_2002/preco_lan_medio_2001,preco_lan_medio_2003/preco_lan_medio_2002,
                             preco_lan_medio_2004/preco_lan_medio_2003, preco_lan_medio_2005/preco_lan_medio_2004,
                             preco_lan_medio_2006/preco_lan_medio_2005, preco_lan_medio_2007/preco_lan_medio_2006,
                             preco_lan_medio_2008/preco_lan_medio_2007)
    
    plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = paste("Taxa anual de crescimento do preço de lançamento",levels(merge_data_final$SUBPREF)[k]))
    lines(y=vetor_precos_lan_tempo,x=anos,col="red")
    
    
    #ln()
    vetor_precos_lan_tempo=c(log(preco_lan_medio_1995),log(preco_lan_medio_1996),log(preco_lan_medio_1997),log(preco_lan_medio_1998),log(preco_lan_medio_1999),
                             log(preco_lan_medio_2000),log(preco_lan_medio_2001),
                             log(preco_lan_medio_2002),log(preco_lan_medio_2003),log(preco_lan_medio_2004),
                             log(preco_lan_medio_2005),log(preco_lan_medio_2006),log(preco_lan_medio_2007),
                             log(preco_lan_medio_2008))
    anos=c(1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)
    
    plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = paste("Log do preço de lançamento anual",levels(merge_data_final$SUBPREF)[k]))
    lines(y=vetor_precos_lan_tempo,x=anos,col="red")
    
    
    #Corrigindo inflação
    preco_lan_medio_1997=preco_lan_medio_1997-preco_lan_medio_1997*0
    preco_lan_medio_1998=preco_lan_medio_1998-preco_lan_medio_1997*0.0165
    preco_lan_medio_1999=preco_lan_medio_1999-preco_lan_medio_1997*0.1074
    preco_lan_medio_2000=preco_lan_medio_2000-preco_lan_medio_1997*0.1735
    preco_lan_medio_2001=preco_lan_medio_2001-preco_lan_medio_1997*0.2635
    preco_lan_medio_2002=preco_lan_medio_2002-preco_lan_medio_1997*0.4218
    preco_lan_medio_2003=preco_lan_medio_2003-preco_lan_medio_1997*0.5540
    preco_lan_medio_2004=preco_lan_medio_2004-preco_lan_medio_1997*0.6721
    
    lowerbound_1997=lowerbound_1997-lowerbound_1997*0
    lowerbound_1998=lowerbound_1998-lowerbound_1997*0.0165
    lowerbound_1999=lowerbound_1999-lowerbound_1997*0.1074
    lowerbound_2000=lowerbound_2000-lowerbound_1997*0.1735
    lowerbound_2001=lowerbound_2001-lowerbound_1997*0.2635
    lowerbound_2002=lowerbound_2002-lowerbound_1997*0.4218
    lowerbound_2003=lowerbound_2003-lowerbound_1997*0.5540
    lowerbound_2004=lowerbound_2004-lowerbound_1997*0.6721
    
    upperbound_1997=upperbound_1997-upperbound_1997*0
    upperbound_1998=upperbound_1998-upperbound_1997*0.0165
    upperbound_1999=upperbound_1999-upperbound_1997*0.1074
    upperbound_2000=upperbound_2000-upperbound_1997*0.1735
    upperbound_2001=upperbound_2001-upperbound_1997*0.2635
    upperbound_2002=upperbound_2002-upperbound_1997*0.4218
    upperbound_2003=upperbound_2003-upperbound_1997*0.5540
    upperbound_2004=upperbound_2004-upperbound_1997*0.6721
    
    #até 2004
    vetor_precos_lan_tempo=c(preco_lan_medio_1997,preco_lan_medio_1998,preco_lan_medio_1998,
                             preco_lan_medio_2000,preco_lan_medio_2001,preco_lan_medio_2002,
                             preco_lan_medio_2003,preco_lan_medio_2004)
    vetor_precos_lan_tempo_lowerbound=c(lowerbound_1997,lowerbound_1998,lowerbound_1999,
                                        lowerbound_2000,lowerbound_2001,lowerbound_2002,
                                        lowerbound_2003,lowerbound_2004)
    vetor_precos_lan_tempo_upperbound=c(upperbound_1997,upperbound_1998,upperbound_1999,
                                        upperbound_2000,upperbound_2001,upperbound_2002,
                                        upperbound_2003,upperbound_2004)
    anos=c(1997,1998,1999,2000,2001,2002,2003,2004)
    
    
    #até 2002
    vetor_precos_lan_tempo=c(preco_lan_medio_1997,preco_lan_medio_1998,preco_lan_medio_1999,
                             preco_lan_medio_2000,preco_lan_medio_2001,preco_lan_medio_2002)
    vetor_precos_lan_tempo_lowerbound=c(lowerbound_1997,lowerbound_1998,lowerbound_1999,
                                        lowerbound_2000,lowerbound_2001,lowerbound_2002)
    vetor_precos_lan_tempo_upperbound=c(upperbound_1997,upperbound_1998,upperbound_1999,
                                        upperbound_2000,upperbound_2001,upperbound_2002)
    anos=c(1997,1998,1999,2000,2001,2002)
    
    
    plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = paste("Valor médio de lançamento do Imóvel por ano (corrigido IPCA)",levels(merge_data_final$SUBPREF)[k]))
    lines(y=vetor_precos_lan_tempo,x=anos,col="red")
    plot(y=vetor_precos_lan_tempo,x=anos,pch=21,ylim = c(min(vetor_precos_lan_tempo_lowerbound),max(vetor_precos_lan_tempo_upperbound)),main =paste("Valor médio de lançamento do Imóvel por ano com limites de 1 desvio",levels(merge_data_final$SUBPREF)[k]))
    #legend(1995,max(vetor_precos_lan_tempo_upperbound),legend = c("x: Limite superior","+: Limite inferior"))
    points(x=anos,y=vetor_precos_lan_tempo_lowerbound,pch=3)
    points(x=anos,y=vetor_precos_lan_tempo_upperbound,pch=4)
    
    
  }
}



#########################################################################
#######considerando tier de aliquota real observações da base############
#########################################################################
#construindo tier  da aliquota real
load("~/ws_final.RData")

#tier 1
par(mfrow=c(2,1))
tier1=0.8834246 
preco_lan_medio_1995=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==1995])
preco_lan_medio_1996=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==1996])
preco_lan_medio_1997=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==1997])
preco_lan_medio_1998=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==1998])
preco_lan_medio_1999=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==1999])
preco_lan_medio_2000=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2000])
preco_lan_medio_2001=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2001])
preco_lan_medio_2002=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2002])
preco_lan_medio_2003=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2003])
preco_lan_medio_2004=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2004])
preco_lan_medio_2005=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2005])
preco_lan_medio_2006=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2006])
preco_lan_medio_2007=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2007])
preco_lan_medio_2008=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2008])


lowerbound_1995=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==1995])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==1995])
lowerbound_1996=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==1996])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==1996])
lowerbound_1997=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==1997])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==1997])
lowerbound_1998=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==1998])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==1998])
lowerbound_1999=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==1999])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==1999])
lowerbound_2000=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2000])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2000])
lowerbound_2001=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2001])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2001])
lowerbound_2002=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2002])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2002])
lowerbound_2003=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2003])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2003])
lowerbound_2004=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2004])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2004])
lowerbound_2005=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2005])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2005])
lowerbound_2006=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2006])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2006])
lowerbound_2007=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2007])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2007])
lowerbound_2008=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2008])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2008])


upperbound_1995=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==1995])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==1995])
upperbound_1996=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==1996])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==1996])
upperbound_1997=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==1997])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==1997])
upperbound_1998=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==1998])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==1998])
upperbound_1999=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==1999])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==1999])
upperbound_2000=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2000])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2000])
upperbound_2001=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2001])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2001])
upperbound_2002=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2002])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2002])
upperbound_2003=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2003])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2003])
upperbound_2004=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2004])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2004])
upperbound_2005=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2005])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2005])
upperbound_2006=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2006])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2006])
upperbound_2007=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2007])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2007])
upperbound_2008=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2008])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<tier1]==2008])


vetor_precos_lan_tempo=c(preco_lan_medio_1995,preco_lan_medio_1996,preco_lan_medio_1997,preco_lan_medio_1998,preco_lan_medio_1998,
                         preco_lan_medio_2000,preco_lan_medio_2001,preco_lan_medio_2002,
                         preco_lan_medio_2003,preco_lan_medio_2004,preco_lan_medio_2005,preco_lan_medio_2006,
                         preco_lan_medio_2007,preco_lan_medio_2008)
vetor_precos_lan_tempo_lowerbound=c(lowerbound_1995,lowerbound_1996,lowerbound_1997,lowerbound_1998,lowerbound_1999,
                                    lowerbound_2000,lowerbound_2001,lowerbound_2002,
                                    lowerbound_2003,lowerbound_2004,lowerbound_2005,
                                    lowerbound_2006,lowerbound_2007,lowerbound_2008)
vetor_precos_lan_tempo_upperbound=c(upperbound_1995,upperbound_1996,upperbound_1997,upperbound_1998,upperbound_1999,
                                    upperbound_2000,upperbound_2001,upperbound_2002,
                                    upperbound_2003,upperbound_2004,
                                    upperbound_2005,upperbound_2006,upperbound_2007,upperbound_2008)
anos=c(1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)

###Sem corrigir inflação
plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Valor médio de lançamento do Imóvel por ano")
lines(y=vetor_precos_lan_tempo,x=anos,col="red")
plot(y=vetor_precos_lan_tempo,x=anos,ylim = c(min(vetor_precos_lan_tempo_lowerbound),max(vetor_precos_lan_tempo_upperbound)),pch=21,main = "Valor médio de lançamento do Imóvel por ano com limites de 1 desvio")
#legend(1997,1900000,legend = c("x: Limite superior","+: Limite inferior"))
points(x=anos,y=vetor_precos_lan_tempo_lowerbound,pch=3)
points(x=anos,y=vetor_precos_lan_tempo_upperbound,pch=4)


#PT em ratio
vetor_precos_lan_tempo=c(preco_lan_medio_1996/preco_lan_medio_1995,preco_lan_medio_1997/preco_lan_medio_1996,
                         preco_lan_medio_1998/preco_lan_medio_1997,preco_lan_medio_1999/preco_lan_medio_1998,
                         preco_lan_medio_2000/preco_lan_medio_1999,preco_lan_medio_2001/preco_lan_medio_2000,
                         preco_lan_medio_2002/preco_lan_medio_2001,preco_lan_medio_2003/preco_lan_medio_2002,
                         preco_lan_medio_2004/preco_lan_medio_2003, preco_lan_medio_2005/preco_lan_medio_2004,
                         preco_lan_medio_2006/preco_lan_medio_2005, preco_lan_medio_2007/preco_lan_medio_2006,
                         preco_lan_medio_2008/preco_lan_medio_2007)
anos=c(1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)

plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Taxa anual de crescimento do preço de lançamento")
lines(y=vetor_precos_lan_tempo,x=anos,col="red")


#ln()
vetor_precos_lan_tempo=c(log(preco_lan_medio_1995),log(preco_lan_medio_1996),log(preco_lan_medio_1997),log(preco_lan_medio_1998),log(preco_lan_medio_1999),
                         log(preco_lan_medio_2000),log(preco_lan_medio_2001),
                         log(preco_lan_medio_2002),log(preco_lan_medio_2003),log(preco_lan_medio_2004),
                         log(preco_lan_medio_2005),log(preco_lan_medio_2006),log(preco_lan_medio_2007),
                         log(preco_lan_medio_2008))
anos=c(1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)

plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Log da média do preço")
lines(y=vetor_precos_lan_tempo,x=anos,col="red")


#Corrigindo inflação
preco_lan_medio_1997=preco_lan_medio_1997-preco_lan_medio_1997*0
preco_lan_medio_1998=preco_lan_medio_1998-preco_lan_medio_1997*0.0165
preco_lan_medio_1999=preco_lan_medio_1999-preco_lan_medio_1997*0.1074
preco_lan_medio_2000=preco_lan_medio_2000-preco_lan_medio_1997*0.1735
preco_lan_medio_2001=preco_lan_medio_2001-preco_lan_medio_1997*0.2635
preco_lan_medio_2002=preco_lan_medio_2002-preco_lan_medio_1997*0.4218
preco_lan_medio_2003=preco_lan_medio_2003-preco_lan_medio_1997*0.5540
preco_lan_medio_2004=preco_lan_medio_2004-preco_lan_medio_1997*0.6721
preco_lan_medio_2005=preco_lan_medio_2005-preco_lan_medio_1997*0.7672
preco_lan_medio_2006=preco_lan_medio_2006-preco_lan_medio_1997*0.8227
preco_lan_medio_2007=preco_lan_medio_2007-preco_lan_medio_1997*0.9040
preco_lan_medio_2008=preco_lan_medio_2008-preco_lan_medio_1997*1.016

lowerbound_1997=lowerbound_1997-lowerbound_1997*0
lowerbound_1998=lowerbound_1998-lowerbound_1997*0.0165
lowerbound_1999=lowerbound_1999-lowerbound_1997*0.1074
lowerbound_2000=lowerbound_2000-lowerbound_1997*0.1735
lowerbound_2001=lowerbound_2001-lowerbound_1997*0.2635
lowerbound_2002=lowerbound_2002-lowerbound_1997*0.4218
lowerbound_2003=lowerbound_2003-lowerbound_1997*0.5540
lowerbound_2004=lowerbound_2004-lowerbound_1997*0.6721
lowerbound_2005=lowerbound_2005-lowerbound_1997*0.7672
lowerbound_2006=lowerbound_2006-lowerbound_1997*0.8227
lowerbound_2007=lowerbound_2007-lowerbound_1997*0.9040
lowerbound_2008=lowerbound_2008-lowerbound_1997*1.016

upperbound_1997=upperbound_1997+upperbound_1997*0
upperbound_1998=upperbound_1998+upperbound_1997*0.0165
upperbound_1999=upperbound_1999+upperbound_1997*0.1074
upperbound_2000=upperbound_2000+upperbound_1997*0.1735
upperbound_2001=upperbound_2001+upperbound_1997*0.2635
upperbound_2002=upperbound_2002+upperbound_1997*0.4218
upperbound_2003=upperbound_2003+upperbound_1997*0.5540
upperbound_2004=upperbound_2004+upperbound_1997*0.6721
upperbound_2005=upperbound_2005+upperbound_1997*0.7672
upperbound_2006=upperbound_2006+upperbound_1997*0.8227
upperbound_2007=upperbound_2007+upperbound_1997*0.9040
upperbound_2008=upperbound_2008+upperbound_1997*1.016

#até 2004
vetor_precos_lan_tempo=c(preco_lan_medio_1997,preco_lan_medio_1998,preco_lan_medio_1999,
                         preco_lan_medio_2000,preco_lan_medio_2001,preco_lan_medio_2002,
                         preco_lan_medio_2003,preco_lan_medio_2004,preco_lan_medio_2005,preco_lan_medio_2006
                         ,preco_lan_medio_2007,preco_lan_medio_2008)
vetor_precos_lan_tempo_lowerbound=c(lowerbound_1997,lowerbound_1998,lowerbound_1999,
                                    lowerbound_2000,lowerbound_2001,lowerbound_2002,
                                    lowerbound_2003,lowerbound_2004,lowerbound_2005,
                                    lowerbound_2006,lowerbound_2007,lowerbound_2008)
vetor_precos_lan_tempo_upperbound=c(upperbound_1997,upperbound_1998,upperbound_1999,
                                    upperbound_2000,upperbound_2001,upperbound_2002,
                                    upperbound_2003,upperbound_2004,upperbound_2005,
                                    upperbound_2006,upperbound_2007,upperbound_2008)
anos=c(1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)

plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Valor médio de lançamento do Imóvel por ano (corrigido IPCA)")
lines(y=vetor_precos_lan_tempo,x=anos,col="red")
plot(y=vetor_precos_lan_tempo,x=anos,ylim = c(-250000,600000),pch=21,main = "Valor médio de lançamento do Imóvel por ano com limites de 1 desvio")
#legend(1997,600000,legend = c("x: Limite superior","+: Limite inferior"))
points(x=anos,y=vetor_precos_lan_tempo_lowerbound,pch=3)
points(x=anos,y=vetor_precos_lan_tempo_upperbound,pch=4)

#tier 2
load("~/ws_final.RData")

par(mfrow=c(2,1))
tier2=0.9429696  
tier1=0.8834246 
preco_lan_medio_1995=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==1995])
preco_lan_medio_1996=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==1996])
preco_lan_medio_1997=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==1997])
preco_lan_medio_1998=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==1998])
preco_lan_medio_1999=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==1999])
preco_lan_medio_2000=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2000])
preco_lan_medio_2001=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2001])
preco_lan_medio_2002=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2002])
preco_lan_medio_2003=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2003])
preco_lan_medio_2004=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2004])
preco_lan_medio_2005=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2005])
preco_lan_medio_2006=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2006])
preco_lan_medio_2007=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2007])
preco_lan_medio_2008=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2008])


lowerbound_1995=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==1995])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==1995])
lowerbound_1996=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==1996])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==1996])
lowerbound_1997=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==1997])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==1997])
lowerbound_1998=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==1998])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==1998])
lowerbound_1999=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==1999])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==1999])
lowerbound_2000=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2000])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2000])
lowerbound_2001=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2001])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2001])
lowerbound_2002=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2002])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2002])
lowerbound_2003=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2003])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2003])
lowerbound_2004=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2004])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2004])
lowerbound_2005=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2005])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2005])
lowerbound_2006=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2006])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2006])
lowerbound_2007=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2007])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2007])
lowerbound_2008=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2008])-0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2008])


upperbound_1995=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==1995])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==1995])
upperbound_1996=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==1996])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==1996])
upperbound_1997=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==1997])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==1997])
upperbound_1998=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==1998])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==1998])
upperbound_1999=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==1999])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==1999])
upperbound_2000=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2000])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2000])
upperbound_2001=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2001])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2001])
upperbound_2002=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2002])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2002])
upperbound_2003=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2003])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2003])
upperbound_2004=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2004])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2004])
upperbound_2005=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2005])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2005])
upperbound_2006=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2006])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2006])
upperbound_2007=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2007])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2007])
upperbound_2008=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2008])+0.5*sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier2]>=tier1]==2008])


vetor_precos_lan_tempo=c(preco_lan_medio_1995,preco_lan_medio_1996,preco_lan_medio_1997,preco_lan_medio_1998,preco_lan_medio_1998,
                         preco_lan_medio_2000,preco_lan_medio_2001,preco_lan_medio_2002,
                         preco_lan_medio_2003,preco_lan_medio_2004,preco_lan_medio_2005,preco_lan_medio_2006,
                         preco_lan_medio_2007,preco_lan_medio_2008)
vetor_precos_lan_tempo_lowerbound=c(lowerbound_1995,lowerbound_1996,lowerbound_1997,lowerbound_1998,lowerbound_1999,
                                    lowerbound_2000,lowerbound_2001,lowerbound_2002,
                                    lowerbound_2003,lowerbound_2004,lowerbound_2005,
                                    lowerbound_2006,lowerbound_2007,lowerbound_2008)
vetor_precos_lan_tempo_upperbound=c(upperbound_1995,upperbound_1996,upperbound_1997,upperbound_1998,upperbound_1999,
                                    upperbound_2000,upperbound_2001,upperbound_2002,
                                    upperbound_2003,upperbound_2004,
                                    upperbound_2005,upperbound_2006,upperbound_2007,upperbound_2008)
anos=c(1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)

###Sem corrigir inflação
plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Valor médio de lançamento do Imóvel por ano")
lines(y=vetor_precos_lan_tempo,x=anos,col="red")
plot(y=vetor_precos_lan_tempo,x=anos,ylim = c(min(vetor_precos_lan_tempo_lowerbound),max(vetor_precos_lan_tempo_upperbound)),pch=21,main = "Valor médio de lançamento do Imóvel por ano com limites de 1 desvio")
#legend(1997,1900000,legend = c("x: Limite superior","+: Limite inferior"))
points(x=anos,y=vetor_precos_lan_tempo_lowerbound,pch=3)
points(x=anos,y=vetor_precos_lan_tempo_upperbound,pch=4)


#PT em ratio
vetor_precos_lan_tempo=c(preco_lan_medio_1996/preco_lan_medio_1995,preco_lan_medio_1997/preco_lan_medio_1996,
                         preco_lan_medio_1998/preco_lan_medio_1997,preco_lan_medio_1999/preco_lan_medio_1998,
                         preco_lan_medio_2000/preco_lan_medio_1999,preco_lan_medio_2001/preco_lan_medio_2000,
                         preco_lan_medio_2002/preco_lan_medio_2001,preco_lan_medio_2003/preco_lan_medio_2002,
                         preco_lan_medio_2004/preco_lan_medio_2003, preco_lan_medio_2005/preco_lan_medio_2004,
                         preco_lan_medio_2006/preco_lan_medio_2005, preco_lan_medio_2007/preco_lan_medio_2006,
                         preco_lan_medio_2008/preco_lan_medio_2007)
anos=c(1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)

plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Taxa anual de crescimento do preço de lançamento")
lines(y=vetor_precos_lan_tempo,x=anos,col="red")


#ln()
vetor_precos_lan_tempo=c(log(preco_lan_medio_1995),log(preco_lan_medio_1996),log(preco_lan_medio_1997),log(preco_lan_medio_1998),log(preco_lan_medio_1999),
                         log(preco_lan_medio_2000),log(preco_lan_medio_2001),
                         log(preco_lan_medio_2002),log(preco_lan_medio_2003),log(preco_lan_medio_2004),
                         log(preco_lan_medio_2005),log(preco_lan_medio_2006),log(preco_lan_medio_2007),
                         log(preco_lan_medio_2008))
anos=c(1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)

plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Log da média do preço")
lines(y=vetor_precos_lan_tempo,x=anos,col="red")


#Corrigindo inflação
preco_lan_medio_1997=preco_lan_medio_1997-preco_lan_medio_1997*0
preco_lan_medio_1998=preco_lan_medio_1998-preco_lan_medio_1997*0.0165
preco_lan_medio_1999=preco_lan_medio_1999-preco_lan_medio_1997*0.1074
preco_lan_medio_2000=preco_lan_medio_2000-preco_lan_medio_1997*0.1735
preco_lan_medio_2001=preco_lan_medio_2001-preco_lan_medio_1997*0.2635
preco_lan_medio_2002=preco_lan_medio_2002-preco_lan_medio_1997*0.4218
preco_lan_medio_2003=preco_lan_medio_2003-preco_lan_medio_1997*0.5540
preco_lan_medio_2004=preco_lan_medio_2004-preco_lan_medio_1997*0.6721
preco_lan_medio_2005=preco_lan_medio_2005-preco_lan_medio_1997*0.7672
preco_lan_medio_2006=preco_lan_medio_2006-preco_lan_medio_1997*0.8227
preco_lan_medio_2007=preco_lan_medio_2007-preco_lan_medio_1997*0.9040
preco_lan_medio_2008=preco_lan_medio_2008-preco_lan_medio_1997*1.016

lowerbound_1997=lowerbound_1997-lowerbound_1997*0
lowerbound_1998=lowerbound_1998-lowerbound_1997*0.0165
lowerbound_1999=lowerbound_1999-lowerbound_1997*0.1074
lowerbound_2000=lowerbound_2000-lowerbound_1997*0.1735
lowerbound_2001=lowerbound_2001-lowerbound_1997*0.2635
lowerbound_2002=lowerbound_2002-lowerbound_1997*0.4218
lowerbound_2003=lowerbound_2003-lowerbound_1997*0.5540
lowerbound_2004=lowerbound_2004-lowerbound_1997*0.6721
lowerbound_2005=lowerbound_2005-lowerbound_1997*0.7672
lowerbound_2006=lowerbound_2006-lowerbound_1997*0.8227
lowerbound_2007=lowerbound_2007-lowerbound_1997*0.9040
lowerbound_2008=lowerbound_2008-lowerbound_1997*1.016

upperbound_1997=upperbound_1997+upperbound_1997*0
upperbound_1998=upperbound_1998+upperbound_1997*0.0165
upperbound_1999=upperbound_1999+upperbound_1997*0.1074
upperbound_2000=upperbound_2000+upperbound_1997*0.1735
upperbound_2001=upperbound_2001+upperbound_1997*0.2635
upperbound_2002=upperbound_2002+upperbound_1997*0.4218
upperbound_2003=upperbound_2003+upperbound_1997*0.5540
upperbound_2004=upperbound_2004+upperbound_1997*0.6721
upperbound_2005=upperbound_2005+upperbound_1997*0.7672
upperbound_2006=upperbound_2006+upperbound_1997*0.8227
upperbound_2007=upperbound_2007+upperbound_1997*0.9040
upperbound_2008=upperbound_2008+upperbound_1997*1.016

#até 2004
vetor_precos_lan_tempo=c(preco_lan_medio_1997,preco_lan_medio_1998,preco_lan_medio_1998,
                         preco_lan_medio_2000,preco_lan_medio_2001,preco_lan_medio_2002,
                         preco_lan_medio_2003,preco_lan_medio_2004,preco_lan_medio_2005,preco_lan_medio_2006
                         ,preco_lan_medio_2007,preco_lan_medio_2008)
vetor_precos_lan_tempo_lowerbound=c(lowerbound_1997,lowerbound_1998,lowerbound_1999,
                                    lowerbound_2000,lowerbound_2001,lowerbound_2002,
                                    lowerbound_2003,lowerbound_2004,lowerbound_2005,
                                    lowerbound_2006,lowerbound_2007,lowerbound_2008)
vetor_precos_lan_tempo_upperbound=c(upperbound_1997,upperbound_1998,upperbound_1999,
                                    upperbound_2000,upperbound_2001,upperbound_2002,
                                    upperbound_2003,upperbound_2004,upperbound_2005,
                                    upperbound_2006,upperbound_2007,upperbound_2008)
anos=c(1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)

plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Valor médio de lançamento do Imóvel por ano (corrigido IPCA)")
lines(y=vetor_precos_lan_tempo,x=anos,col="red")
plot(y=vetor_precos_lan_tempo,x=anos,ylim = c(-250000,600000),pch=21,main = "Valor médio de lançamento do Imóvel por ano com limites de 1 desvio")
#legend(1997,600000,legend = c("x: Limite superior","+: Limite inferior"))
points(x=anos,y=vetor_precos_lan_tempo_lowerbound,pch=3)
points(x=anos,y=vetor_precos_lan_tempo_upperbound,pch=4)

#tier 3
load("~/ws_final.RData")

par(mfrow=c(2,1))
tier2=0.9429696  
tier3=1.0631982
preco_lan_medio_1995=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==1995])
preco_lan_medio_1996=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==1996])
preco_lan_medio_1997=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==1997])
preco_lan_medio_1998=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==1998])
preco_lan_medio_1999=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==1999])
preco_lan_medio_2000=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2000])
preco_lan_medio_2001=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2001])
preco_lan_medio_2002=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2002])
preco_lan_medio_2003=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2003])
preco_lan_medio_2004=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2004])
preco_lan_medio_2005=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2005])
preco_lan_medio_2006=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2006])
preco_lan_medio_2007=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2007])
preco_lan_medio_2008=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2008])


lowerbound_1995=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==1995])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==1995])
lowerbound_1996=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==1996])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==1996])
lowerbound_1997=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==1997])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==1997])
lowerbound_1998=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==1998])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==1998])
lowerbound_1999=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==1999])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==1999])
lowerbound_2000=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2000])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2000])
lowerbound_2001=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2001])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2001])
lowerbound_2002=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2002])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2002])
lowerbound_2003=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2003])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2003])
lowerbound_2004=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2004])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2004])
lowerbound_2005=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2005])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2005])
lowerbound_2006=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2006])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2006])
lowerbound_2007=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2007])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2007])
lowerbound_2008=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2008])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2008])


upperbound_1995=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==1995])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==1995])
upperbound_1996=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==1996])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==1996])
upperbound_1997=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==1997])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==1997])
upperbound_1998=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==1998])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==1998])
upperbound_1999=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==1999])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==1999])
upperbound_2000=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2000])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2000])
upperbound_2001=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2001])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2001])
upperbound_2002=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2002])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2002])
upperbound_2003=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2003])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2003])
upperbound_2004=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2004])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2004])
upperbound_2005=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2005])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2005])
upperbound_2006=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2006])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2006])
upperbound_2007=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2007])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2007])
upperbound_2008=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2008])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier3]>=tier2]==2008])


vetor_precos_lan_tempo=c(preco_lan_medio_1995,preco_lan_medio_1996,preco_lan_medio_1997,preco_lan_medio_1998,preco_lan_medio_1999,
                         preco_lan_medio_2000,preco_lan_medio_2001,preco_lan_medio_2002,
                         preco_lan_medio_2003,preco_lan_medio_2004,preco_lan_medio_2005,preco_lan_medio_2006,
                         preco_lan_medio_2007,preco_lan_medio_2008)
vetor_precos_lan_tempo_lowerbound=c(lowerbound_1995,lowerbound_1996,lowerbound_1997,lowerbound_1998,lowerbound_1999,
                                    lowerbound_2000,lowerbound_2001,lowerbound_2002,
                                    lowerbound_2003,lowerbound_2004,lowerbound_2005,
                                    lowerbound_2006,lowerbound_2007,lowerbound_2008)
vetor_precos_lan_tempo_upperbound=c(upperbound_1995,upperbound_1996,upperbound_1997,upperbound_1998,upperbound_1999,
                                    upperbound_2000,upperbound_2001,upperbound_2002,
                                    upperbound_2003,upperbound_2004,
                                    upperbound_2005,upperbound_2006,upperbound_2007,upperbound_2008)
anos=c(1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)

###Sem corrigir inflação
plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Valor médio de lançamento do Imóvel por ano")
lines(y=vetor_precos_lan_tempo,x=anos,col="red")
plot(y=vetor_precos_lan_tempo,x=anos,ylim = c(min(vetor_precos_lan_tempo_lowerbound),max(vetor_precos_lan_tempo_upperbound)),pch=21,main = "Valor médio de lançamento do Imóvel por ano com limites de 1 desvio")
#legend(1997,1900000,legend = c("x: Limite superior","+: Limite inferior"))
points(x=anos,y=vetor_precos_lan_tempo_lowerbound,pch=3)
points(x=anos,y=vetor_precos_lan_tempo_upperbound,pch=4)


#PT em ratio
vetor_precos_lan_tempo=c(preco_lan_medio_1996/preco_lan_medio_1995,preco_lan_medio_1997/preco_lan_medio_1996,
                         preco_lan_medio_1998/preco_lan_medio_1997,preco_lan_medio_1999/preco_lan_medio_1998,
                         preco_lan_medio_2000/preco_lan_medio_1999,preco_lan_medio_2001/preco_lan_medio_2000,
                         preco_lan_medio_2002/preco_lan_medio_2001,preco_lan_medio_2003/preco_lan_medio_2002,
                         preco_lan_medio_2004/preco_lan_medio_2003, preco_lan_medio_2005/preco_lan_medio_2004,
                         preco_lan_medio_2006/preco_lan_medio_2005, preco_lan_medio_2007/preco_lan_medio_2006,
                         preco_lan_medio_2008/preco_lan_medio_2007)
anos=c(1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)

plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Taxa anual de crescimento do preço de lançamento")
lines(y=vetor_precos_lan_tempo,x=anos,col="red")


#ln()
vetor_precos_lan_tempo=c(log(preco_lan_medio_1995),log(preco_lan_medio_1996),log(preco_lan_medio_1997),log(preco_lan_medio_1998),log(preco_lan_medio_1999),
                         log(preco_lan_medio_2000),log(preco_lan_medio_2001),
                         log(preco_lan_medio_2002),log(preco_lan_medio_2003),log(preco_lan_medio_2004),
                         log(preco_lan_medio_2005),log(preco_lan_medio_2006),log(preco_lan_medio_2007),
                         log(preco_lan_medio_2008))
anos=c(1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)

plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Log da média do preço")
lines(y=vetor_precos_lan_tempo,x=anos,col="red")


#Corrigindo inflação
preco_lan_medio_1997=preco_lan_medio_1997-preco_lan_medio_1997*0
preco_lan_medio_1998=preco_lan_medio_1998-preco_lan_medio_1997*0.0165
preco_lan_medio_1999=preco_lan_medio_1999-preco_lan_medio_1997*0.1074
preco_lan_medio_2000=preco_lan_medio_2000-preco_lan_medio_1997*0.1735
preco_lan_medio_2001=preco_lan_medio_2001-preco_lan_medio_1997*0.2635
preco_lan_medio_2002=preco_lan_medio_2002-preco_lan_medio_1997*0.4218
preco_lan_medio_2003=preco_lan_medio_2003-preco_lan_medio_1997*0.5540
preco_lan_medio_2004=preco_lan_medio_2004-preco_lan_medio_1997*0.6721
preco_lan_medio_2005=preco_lan_medio_2005-preco_lan_medio_1997*0.7672
preco_lan_medio_2006=preco_lan_medio_2006-preco_lan_medio_1997*0.8227
preco_lan_medio_2007=preco_lan_medio_2007-preco_lan_medio_1997*0.9040
preco_lan_medio_2008=preco_lan_medio_2008-preco_lan_medio_1997*1.016

lowerbound_1997=lowerbound_1997-lowerbound_1997*0
lowerbound_1998=lowerbound_1998-lowerbound_1997*0.0165
lowerbound_1999=lowerbound_1999-lowerbound_1997*0.1074
lowerbound_2000=lowerbound_2000-lowerbound_1997*0.1735
lowerbound_2001=lowerbound_2001-lowerbound_1997*0.2635
lowerbound_2002=lowerbound_2002-lowerbound_1997*0.4218
lowerbound_2003=lowerbound_2003-lowerbound_1997*0.5540
lowerbound_2004=lowerbound_2004-lowerbound_1997*0.6721
lowerbound_2005=lowerbound_2005-lowerbound_1997*0.7672
lowerbound_2006=lowerbound_2006-lowerbound_1997*0.8227
lowerbound_2007=lowerbound_2007-lowerbound_1997*0.9040
lowerbound_2008=lowerbound_2008-lowerbound_1997*1.016

upperbound_1997=upperbound_1997+upperbound_1997*0
upperbound_1998=upperbound_1998+upperbound_1997*0.0165
upperbound_1999=upperbound_1999+upperbound_1997*0.1074
upperbound_2000=upperbound_2000+upperbound_1997*0.1735
upperbound_2001=upperbound_2001+upperbound_1997*0.2635
upperbound_2002=upperbound_2002+upperbound_1997*0.4218
upperbound_2003=upperbound_2003+upperbound_1997*0.5540
upperbound_2004=upperbound_2004+upperbound_1997*0.6721
upperbound_2005=upperbound_2005+upperbound_1997*0.7672
upperbound_2006=upperbound_2006+upperbound_1997*0.8227
upperbound_2007=upperbound_2007+upperbound_1997*0.9040
upperbound_2008=upperbound_2008+upperbound_1997*1.016

#até 2004
vetor_precos_lan_tempo=c(preco_lan_medio_1997,preco_lan_medio_1998,preco_lan_medio_1998,
                         preco_lan_medio_2000,preco_lan_medio_2001,preco_lan_medio_2002,
                         preco_lan_medio_2003,preco_lan_medio_2004,preco_lan_medio_2005,preco_lan_medio_2006
                         ,preco_lan_medio_2007,preco_lan_medio_2008)
vetor_precos_lan_tempo_lowerbound=c(lowerbound_1997,lowerbound_1998,lowerbound_1999,
                                    lowerbound_2000,lowerbound_2001,lowerbound_2002,
                                    lowerbound_2003,lowerbound_2004,lowerbound_2005,
                                    lowerbound_2006,lowerbound_2007,lowerbound_2008)
vetor_precos_lan_tempo_upperbound=c(upperbound_1997,upperbound_1998,upperbound_1999,
                                    upperbound_2000,upperbound_2001,upperbound_2002,
                                    upperbound_2003,upperbound_2004,upperbound_2005,
                                    upperbound_2006,upperbound_2007,upperbound_2008)
anos=c(1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)

plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Valor médio de lançamento do Imóvel por ano (corrigido IPCA)")
lines(y=vetor_precos_lan_tempo,x=anos,col="red")
plot(y=vetor_precos_lan_tempo,x=anos,ylim = c(-250000,600000),pch=21,main = "Valor médio de lançamento do Imóvel por ano com limites de 1 desvio")
#legend(1997,600000,legend = c("x: Limite superior","+: Limite inferior"))
points(x=anos,y=vetor_precos_lan_tempo_lowerbound,pch=3)
points(x=anos,y=vetor_precos_lan_tempo_upperbound,pch=4)


#tier 4
load("~/ws_final.RData")

par(mfrow=c(2,1))
tier3=1.0631982
tier4=1.5959552 
preco_lan_medio_1995=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==1995])
preco_lan_medio_1996=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==1996])
preco_lan_medio_1997=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==1997])
preco_lan_medio_1998=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==1998])
preco_lan_medio_1999=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==1999])
preco_lan_medio_2000=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2000])
preco_lan_medio_2001=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2001])
preco_lan_medio_2002=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2002])
preco_lan_medio_2003=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2003])
preco_lan_medio_2004=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2004])
preco_lan_medio_2005=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2005])
preco_lan_medio_2006=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2006])
preco_lan_medio_2007=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2007])
preco_lan_medio_2008=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2008])


lowerbound_1995=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==1995])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==1995])
lowerbound_1996=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==1996])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==1996])
lowerbound_1997=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==1997])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==1997])
lowerbound_1998=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==1998])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==1998])
lowerbound_1999=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==1999])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==1999])
lowerbound_2000=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2000])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2000])
lowerbound_2001=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2001])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2001])
lowerbound_2002=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2002])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2002])
lowerbound_2003=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2003])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2003])
lowerbound_2004=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2004])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2004])
lowerbound_2005=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2005])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2005])
lowerbound_2006=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2006])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2006])
lowerbound_2007=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2007])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2007])
lowerbound_2008=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2008])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2008])


upperbound_1995=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==1995])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==1995])
upperbound_1996=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==1996])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==1996])
upperbound_1997=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==1997])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==1997])
upperbound_1998=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==1998])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==1998])
upperbound_1999=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==1999])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==1999])
upperbound_2000=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2000])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2000])
upperbound_2001=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2001])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2001])
upperbound_2002=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2002])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2002])
upperbound_2003=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2003])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2003])
upperbound_2004=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2004])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2004])
upperbound_2005=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2005])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2005])
upperbound_2006=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2006])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2006])
upperbound_2007=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2007])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2007])
upperbound_2008=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2008])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tier4]>=tier3]==2008])


vetor_precos_lan_tempo=c(preco_lan_medio_1995,preco_lan_medio_1996,preco_lan_medio_1997,preco_lan_medio_1998,preco_lan_medio_1999,
                         preco_lan_medio_2000,preco_lan_medio_2001,preco_lan_medio_2002,
                         preco_lan_medio_2003,preco_lan_medio_2004,preco_lan_medio_2005,preco_lan_medio_2006,
                         preco_lan_medio_2007,preco_lan_medio_2008)
vetor_precos_lan_tempo_lowerbound=c(lowerbound_1995,lowerbound_1996,lowerbound_1997,lowerbound_1998,lowerbound_1999,
                                    lowerbound_2000,lowerbound_2001,lowerbound_2002,
                                    lowerbound_2003,lowerbound_2004,lowerbound_2005,
                                    lowerbound_2006,lowerbound_2007,lowerbound_2008)
vetor_precos_lan_tempo_upperbound=c(upperbound_1995,upperbound_1996,upperbound_1997,upperbound_1998,upperbound_1999,
                                    upperbound_2000,upperbound_2001,upperbound_2002,
                                    upperbound_2003,upperbound_2004,
                                    upperbound_2005,upperbound_2006,upperbound_2007,upperbound_2008)
anos=c(1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)

###Sem corrigir inflação
plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Valor médio de lançamento do Imóvel por ano")
lines(y=vetor_precos_lan_tempo,x=anos,col="red")
plot(y=vetor_precos_lan_tempo,x=anos,ylim = c(min(vetor_precos_lan_tempo_lowerbound),max(vetor_precos_lan_tempo_upperbound)),pch=21,main = "Valor médio de lançamento do Imóvel por ano com limites de 1 desvio")
#legend(1997,1900000,legend = c("x: Limite superior","+: Limite inferior"))
points(x=anos,y=vetor_precos_lan_tempo_lowerbound,pch=3)
points(x=anos,y=vetor_precos_lan_tempo_upperbound,pch=4)


#PT em ratio
vetor_precos_lan_tempo=c(preco_lan_medio_1996/preco_lan_medio_1995,preco_lan_medio_1997/preco_lan_medio_1996,
                         preco_lan_medio_1998/preco_lan_medio_1997,preco_lan_medio_1999/preco_lan_medio_1998,
                         preco_lan_medio_2000/preco_lan_medio_1999,preco_lan_medio_2001/preco_lan_medio_2000,
                         preco_lan_medio_2002/preco_lan_medio_2001,preco_lan_medio_2003/preco_lan_medio_2002,
                         preco_lan_medio_2004/preco_lan_medio_2003, preco_lan_medio_2005/preco_lan_medio_2004,
                         preco_lan_medio_2006/preco_lan_medio_2005, preco_lan_medio_2007/preco_lan_medio_2006,
                         preco_lan_medio_2008/preco_lan_medio_2007)
anos=c(1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)

plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Taxa anual de crescimento do preço de lançamento")
lines(y=vetor_precos_lan_tempo,x=anos,col="red")


#ln()
vetor_precos_lan_tempo=c(log(preco_lan_medio_1995),log(preco_lan_medio_1996),log(preco_lan_medio_1997),log(preco_lan_medio_1998),log(preco_lan_medio_1999),
                         log(preco_lan_medio_2000),log(preco_lan_medio_2001),
                         log(preco_lan_medio_2002),log(preco_lan_medio_2003),log(preco_lan_medio_2004),
                         log(preco_lan_medio_2005),log(preco_lan_medio_2006),log(preco_lan_medio_2007),
                         log(preco_lan_medio_2008))
anos=c(1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)

plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Log da média do preço")
lines(y=vetor_precos_lan_tempo,x=anos,col="red")


#Corrigindo inflação
preco_lan_medio_1997=preco_lan_medio_1997-preco_lan_medio_1997*0
preco_lan_medio_1998=preco_lan_medio_1998-preco_lan_medio_1997*0.0165
preco_lan_medio_1999=preco_lan_medio_1999-preco_lan_medio_1997*0.1074
preco_lan_medio_2000=preco_lan_medio_2000-preco_lan_medio_1997*0.1735
preco_lan_medio_2001=preco_lan_medio_2001-preco_lan_medio_1997*0.2635
preco_lan_medio_2002=preco_lan_medio_2002-preco_lan_medio_1997*0.4218
preco_lan_medio_2003=preco_lan_medio_2003-preco_lan_medio_1997*0.5540
preco_lan_medio_2004=preco_lan_medio_2004-preco_lan_medio_1997*0.6721
preco_lan_medio_2005=preco_lan_medio_2005-preco_lan_medio_1997*0.7672
preco_lan_medio_2006=preco_lan_medio_2006-preco_lan_medio_1997*0.8227
preco_lan_medio_2007=preco_lan_medio_2007-preco_lan_medio_1997*0.9040
preco_lan_medio_2008=preco_lan_medio_2008-preco_lan_medio_1997*1.016

lowerbound_1997=lowerbound_1997-lowerbound_1997*0
lowerbound_1998=lowerbound_1998-lowerbound_1997*0.0165
lowerbound_1999=lowerbound_1999-lowerbound_1997*0.1074
lowerbound_2000=lowerbound_2000-lowerbound_1997*0.1735
lowerbound_2001=lowerbound_2001-lowerbound_1997*0.2635
lowerbound_2002=lowerbound_2002-lowerbound_1997*0.4218
lowerbound_2003=lowerbound_2003-lowerbound_1997*0.5540
lowerbound_2004=lowerbound_2004-lowerbound_1997*0.6721
lowerbound_2005=lowerbound_2005-lowerbound_1997*0.7672
lowerbound_2006=lowerbound_2006-lowerbound_1997*0.8227
lowerbound_2007=lowerbound_2007-lowerbound_1997*0.9040
lowerbound_2008=lowerbound_2008-lowerbound_1997*1.016

upperbound_1997=upperbound_1997+upperbound_1997*0
upperbound_1998=upperbound_1998+upperbound_1997*0.0165
upperbound_1999=upperbound_1999+upperbound_1997*0.1074
upperbound_2000=upperbound_2000+upperbound_1997*0.1735
upperbound_2001=upperbound_2001+upperbound_1997*0.2635
upperbound_2002=upperbound_2002+upperbound_1997*0.4218
upperbound_2003=upperbound_2003+upperbound_1997*0.5540
upperbound_2004=upperbound_2004+upperbound_1997*0.6721
upperbound_2005=upperbound_2005+upperbound_1997*0.7672
upperbound_2006=upperbound_2006+upperbound_1997*0.8227
upperbound_2007=upperbound_2007+upperbound_1997*0.9040
upperbound_2008=upperbound_2008+upperbound_1997*1.016

#até 2004
vetor_precos_lan_tempo=c(preco_lan_medio_1997,preco_lan_medio_1998,preco_lan_medio_1999,
                         preco_lan_medio_2000,preco_lan_medio_2001,preco_lan_medio_2002,
                         preco_lan_medio_2003,preco_lan_medio_2004,preco_lan_medio_2005,preco_lan_medio_2006
                         ,preco_lan_medio_2007,preco_lan_medio_2008)
vetor_precos_lan_tempo_lowerbound=c(lowerbound_1997,lowerbound_1998,lowerbound_1999,
                                    lowerbound_2000,lowerbound_2001,lowerbound_2002,
                                    lowerbound_2003,lowerbound_2004,lowerbound_2005,
                                    lowerbound_2006,lowerbound_2007,lowerbound_2008)
vetor_precos_lan_tempo_upperbound=c(upperbound_1997,upperbound_1998,upperbound_1999,
                                    upperbound_2000,upperbound_2001,upperbound_2002,
                                    upperbound_2003,upperbound_2004,upperbound_2005,
                                    upperbound_2006,upperbound_2007,upperbound_2008)
anos=c(1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)

plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Valor médio de lançamento do Imóvel por ano (corrigido IPCA)")
lines(y=vetor_precos_lan_tempo,x=anos,col="red")
plot(y=vetor_precos_lan_tempo,x=anos,ylim = c(-250000,600000),pch=21,main = "Valor médio de lançamento do Imóvel por ano com limites de 1 desvio")
#legend(1997,600000,legend = c("x: Limite superior","+: Limite inferior"))
points(x=anos,y=vetor_precos_lan_tempo_lowerbound,pch=3)
points(x=anos,y=vetor_precos_lan_tempo_upperbound,pch=4)


quantile(merge_data_final$aliquota_real)

#ln da Média geométrica
load("~/ws_final.RData")

par(mfrow=c(2,1))

preco_lan_medio_1995=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1995]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1995])))
preco_lan_medio_1996=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1996]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1996])))
preco_lan_medio_1997=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1997]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1997])))
preco_lan_medio_1998=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1998]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1998])))
preco_lan_medio_1999=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1999]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1999])))
preco_lan_medio_2000=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2000]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2000])))
preco_lan_medio_2001=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2001]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2001])))
preco_lan_medio_2002=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2002]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2002])))
preco_lan_medio_2003=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2003]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2003])))
preco_lan_medio_2004=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2004]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2004])))
preco_lan_medio_2005=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2005]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2005])))
preco_lan_medio_2006=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2006]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2006])))
preco_lan_medio_2007=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2007]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2007])))
preco_lan_medio_2008=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2008]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2008])))


vetor_precos_lan_tempo=c(preco_lan_medio_1995,preco_lan_medio_1996,preco_lan_medio_1997,preco_lan_medio_1998,preco_lan_medio_1999,
                         preco_lan_medio_2000,preco_lan_medio_2001,preco_lan_medio_2002,
                         preco_lan_medio_2003,preco_lan_medio_2004,preco_lan_medio_2005,preco_lan_medio_2006,
                         preco_lan_medio_2007,preco_lan_medio_2008)
anos=c(1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)

###Sem corrigir inflação
plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Média geométrica valor lançamento do Imóvel por ano")
lines(y=vetor_precos_lan_tempo,x=anos,col="red")

#PT em ratio
vetor_precos_lan_tempo=c(preco_lan_medio_1996/preco_lan_medio_1995,preco_lan_medio_1997/preco_lan_medio_1996,
                         preco_lan_medio_1998/preco_lan_medio_1997,preco_lan_medio_1999/preco_lan_medio_1998,
                         preco_lan_medio_2000/preco_lan_medio_1999,preco_lan_medio_2001/preco_lan_medio_2000,
                         preco_lan_medio_2002/preco_lan_medio_2001,preco_lan_medio_2003/preco_lan_medio_2002,
                         preco_lan_medio_2004/preco_lan_medio_2003,preco_lan_medio_2005/preco_lan_medio_2004,
                         preco_lan_medio_2006/preco_lan_medio_2005,preco_lan_medio_2007/preco_lan_medio_2006,
                         preco_lan_medio_2008/preco_lan_medio_2007)
anos=c(1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)

plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Taxa anual de crescimento da média geométrica")
lines(y=vetor_precos_lan_tempo,x=anos,col="red")


#ln()
vetor_precos_lan_tempo=c(log(preco_lan_medio_1995),log(preco_lan_medio_1996),log(preco_lan_medio_1997),log(preco_lan_medio_1998),log(preco_lan_medio_1999),
                         log(preco_lan_medio_2000),log(preco_lan_medio_2001),
                         log(preco_lan_medio_2002),log(preco_lan_medio_2003),log(preco_lan_medio_2004),
                         log(preco_lan_medio_2005),log(preco_lan_medio_2006),log(preco_lan_medio_2007),log(preco_lan_medio_2008))
anos=c(1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)

plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Log média geométrica do preço")
lines(y=vetor_precos_lan_tempo,x=anos,col="red")


#Corrigindo inflação
preco_lan_medio_1997=preco_lan_medio_1997-preco_lan_medio_1997*0
preco_lan_medio_1998=preco_lan_medio_1998-preco_lan_medio_1997*0.0165
preco_lan_medio_1999=preco_lan_medio_1999-preco_lan_medio_1997*0.1074
preco_lan_medio_2000=preco_lan_medio_2000-preco_lan_medio_1997*0.1735
preco_lan_medio_2001=preco_lan_medio_2001-preco_lan_medio_1997*0.2635
preco_lan_medio_2002=preco_lan_medio_2002-preco_lan_medio_1997*0.4218
preco_lan_medio_2003=preco_lan_medio_2003-preco_lan_medio_1997*0.5540
preco_lan_medio_2004=preco_lan_medio_2004-preco_lan_medio_1997*0.6721
preco_lan_medio_2005=preco_lan_medio_2005-preco_lan_medio_1997*0.7672
preco_lan_medio_2006=preco_lan_medio_2006-preco_lan_medio_1997*0.8227
preco_lan_medio_2007=preco_lan_medio_2007-preco_lan_medio_1997*0.9040
preco_lan_medio_2008=preco_lan_medio_2008-preco_lan_medio_1997*1.016


#até 2004
vetor_precos_lan_tempo=c(preco_lan_medio_1997,preco_lan_medio_1998,preco_lan_medio_1999,
                         preco_lan_medio_2000,preco_lan_medio_2001,preco_lan_medio_2002,
                         preco_lan_medio_2003,preco_lan_medio_2004,preco_lan_medio_2005,
                         preco_lan_medio_2006,preco_lan_medio_2007,preco_lan_medio_2008)
anos=c(1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)


plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Média geométrica preço (corrigido IPCA)")
lines(y=vetor_precos_lan_tempo,x=anos,col="red")

