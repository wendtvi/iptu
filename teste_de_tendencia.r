#######################################################
###############TESTES PRE TREND E NO ANTECIPATION######
#######################################################
dataset_pretrend=matrix(NA,ncol = 4,nrow = length(merge_data_final$INDEX_MERGE))
dataset_pretrend[,1]=merge_data_final$ANO.DO.EXERCICIO
dataset_pretrend[,2]=merge_data_final$BAIRRO.DO.IMOVEL
dataset_pretrend[,3]=merge_data_final$vv
dataset_pretrend[,4]=merge_data_final$PC_TT_UN
dataset_pretrend[,5]=merge_data_final$aliquota_real

#Teste distribuição normal
preco_lan_medio_1997=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1997])
preco_lan_medio_1998=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1998])
preco_lan_medio_1999=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1999])
preco_lan_medio_2000=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2000])
preco_lan_medio_2001=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2001])
preco_lan_medio_2002=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2002])

lowerbound_1997=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1997])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1997])
lowerbound_1998=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1998])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1998])
lowerbound_1999=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1999])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1999])
lowerbound_2000=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2000])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2000])
lowerbound_2001=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2001])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2001])
lowerbound_2002=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2002])-sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2002])


upperbound_1997=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1997])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1997])
upperbound_1998=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1998])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1998])
upperbound_1999=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1999])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==1999])
upperbound_2000=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2000])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2000])
upperbound_2001=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2001])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2001])
upperbound_2002=mean(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2002])+sd(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO==2002])


vetor_precos_lan_tempo=c(preco_lan_medio_1997,preco_lan_medio_1998,preco_lan_medio_1998,
                         preco_lan_medio_2000,preco_lan_medio_2001,preco_lan_medio_2002)
vetor_precos_lan_tempo_lowerbound=c(lowerbound_1997,lowerbound_1998,lowerbound_1999,
                                    lowerbound_2000,lowerbound_2001,lowerbound_2002)
vetor_precos_lan_tempo_upperbound=c(upperbound_1997,upperbound_1998,upperbound_1999,
                                    upperbound_2000,upperbound_2001,upperbound_2002)
anos=c(1997,1998,1999,2000,2001,2002)
plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Valor médio de lançamento do Imóvel por ano")
plot(y=vetor_precos_lan_tempo,x=anos,ylim = c(-250000,1000000),pch=21,main = "Valor médio de lançamento do Imóvel por ano com limites de 1 desvio")
legend(1997,900000,legend = c("x: Limite superior","+: Limite inferior"))
points(x=anos,y=vetor_precos_lan_tempo_lowerbound,pch=3)
points(x=anos,y=vetor_precos_lan_tempo_upperbound,pch=4)


#No antecipation




