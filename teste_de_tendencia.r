#######################################################
###############TESTES PRE TREND E NO ANTECIPATION######
#######################################################

#########################################################################
###############considerando todas observações da base####################
#########################################################################
#Gráficos de pre trend para valor médio de lançamento
preco_lan_medio_1997=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1997])
preco_lan_medio_1998=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1998])
preco_lan_medio_1999=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1999])
preco_lan_medio_2000=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2000])
preco_lan_medio_2001=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2001])
preco_lan_medio_2002=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2002])
preco_lan_medio_2003=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2003])
preco_lan_medio_2004=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2004])

lowerbound_1997=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1997])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1997])
lowerbound_1998=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1998])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1998])
lowerbound_1999=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1999])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1999])
lowerbound_2000=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2000])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2000])
lowerbound_2001=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2001])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2001])
lowerbound_2002=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2002])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2002])
lowerbound_2003=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2003])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2003])
lowerbound_2004=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2004])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2004])


upperbound_1997=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1997])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1997])
upperbound_1998=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1998])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1998])
upperbound_1999=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1999])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1999])
upperbound_2000=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2000])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2000])
upperbound_2001=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2001])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2001])
upperbound_2002=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2002])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2002])
upperbound_2003=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2003])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2003])
upperbound_2004=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2004])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2004])


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

###Sem corrigir inflação
plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Valor médio de lançamento do Imóvel por ano")
lines(y=vetor_precos_lan_tempo,x=anos,col="red")
plot(y=vetor_precos_lan_tempo,x=anos,ylim = c(-550000,2000000),pch=21,main = "Valor médio de lançamento do Imóvel por ano com limites de 1 desvio")
legend(1997,1900000,legend = c("x: Limite superior","+: Limite inferior"))
points(x=anos,y=vetor_precos_lan_tempo_lowerbound,pch=3)
points(x=anos,y=vetor_precos_lan_tempo_upperbound,pch=4)


#PT em ratio
vetor_precos_lan_tempo=c(preco_lan_medio_1998/preco_lan_medio_1997,preco_lan_medio_1999/preco_lan_medio_1998,
                         preco_lan_medio_2000/preco_lan_medio_1999,preco_lan_medio_2001/preco_lan_medio_2000,
                         preco_lan_medio_2002/preco_lan_medio_2001,preco_lan_medio_2003/preco_lan_medio_2002,
                         preco_lan_medio_2004/preco_lan_medio_2003)
anos=c(1998,1999,2000,2001,2002,2003,2004)

plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Taxa anual de crescimento do preço de lançamento")
lines(y=vetor_precos_lan_tempo,x=anos,col="red")


#ln()
vetor_precos_lan_tempo=c(log(preco_lan_medio_1997),log(preco_lan_medio_1998),log(preco_lan_medio_1999),
                         log(preco_lan_medio_2000),log(preco_lan_medio_2001),
                         log(preco_lan_medio_2002),log(preco_lan_medio_2003),log(preco_lan_medio_2004))
anos=c(1997,1998,1999,2000,2001,2002,2003,2004)

plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Taxa anual de crescimento do preço de lançamento")
lines(y=vetor_precos_lan_tempo,x=anos,col="red")


#Corrigindo inflação
preco_lan_medio_1997=preco_lan_medio_1997-preco_lan_medio_1997*0
preco_lan_medio_1998=preco_lan_medio_1998-preco_lan_medio_1998*0.0165
preco_lan_medio_1999=preco_lan_medio_1999-preco_lan_medio_1999*0.1074
preco_lan_medio_2000=preco_lan_medio_2000-preco_lan_medio_2000*0.1735
preco_lan_medio_2001=preco_lan_medio_2001-preco_lan_medio_2001*0.2635
preco_lan_medio_2002=preco_lan_medio_2002-preco_lan_medio_2002*0.4218
preco_lan_medio_2003=preco_lan_medio_2003-preco_lan_medio_2003*0.5540
preco_lan_medio_2004=preco_lan_medio_2004-preco_lan_medio_2004*0.6721

lowerbound_1997=lowerbound_1997-lowerbound_1997*0
lowerbound_1998=lowerbound_1998-lowerbound_1998*0.0165
lowerbound_1999=lowerbound_1999-lowerbound_1999*0.1074
lowerbound_2000=lowerbound_2000-lowerbound_2000*0.1735
lowerbound_2001=lowerbound_2001-lowerbound_2001*0.2635
lowerbound_2002=lowerbound_2002-lowerbound_2002*0.4218
lowerbound_2003=lowerbound_2003-lowerbound_2003*0.5540
lowerbound_2004=lowerbound_2004-lowerbound_2004*0.6721

upperbound_1997=upperbound_1997-upperbound_1997*0
upperbound_1998=upperbound_1998-upperbound_1998*0.0165
upperbound_1999=upperbound_1999-upperbound_1999*0.1074
upperbound_2000=upperbound_2000-upperbound_2000*0.1735
upperbound_2001=upperbound_2001-upperbound_2001*0.2635
upperbound_2002=upperbound_2002-upperbound_2002*0.4218
upperbound_2003=upperbound_2003-upperbound_2003*0.5540
upperbound_2004=upperbound_2004-upperbound_2004*0.6721

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


plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Valor médio de lançamento do Imóvel por ano (corrigido IPCA)")
lines(y=vetor_precos_lan_tempo,x=anos,col="red")
plot(y=vetor_precos_lan_tempo,x=anos,ylim = c(-250000,600000),pch=21,main = "Valor médio de lançamento do Imóvel por ano com limites de 1 desvio")
legend(1997,600000,legend = c("x: Limite superior","+: Limite inferior"))
points(x=anos,y=vetor_precos_lan_tempo_lowerbound,pch=3)
points(x=anos,y=vetor_precos_lan_tempo_upperbound,pch=4)


#Por bairro




#########################################################################
#######considerando tier de aliquota real observações da base############
#########################################################################
