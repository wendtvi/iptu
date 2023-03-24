#install.packages("fixest")
library(fixest)

##############################################################
##################DESCRITIVAS#################################
##############################################################
table(base_final$merge_data_final.ANO_LAN)/length(base_final$merge_data_final.ANO_LAN)
table(base_final$merge_data_final.SUBPREF)/length(base_final$merge_data_final.SUBPREF)
table(base_final$merge_data_final.TIPO.DE.USO.DO.IMOVEL)/length(base_final$merge_data_final.TIPO.DE.USO.DO.IMOVEL)
table(base_final$merge_data_final.ELEV)/length(base_final$merge_data_final.ELEV)
table(base_final$merge_data_final.BANH_UNID)/length(base_final$merge_data_final.BANH_UNID)
table(base_final$merge_data_final.FATOR.DE.OBSOLESCENCIA)
table(base_final$merge_data_final.FATOR.TIPO.DE.CONDOMINIO)
table(base_final$merge_data_final.FATOR.TIPO.DE.PROFUNDIDADE)
table(base_final$merge_data_final.FATOR.TIPO.DE.TERRENO)
summary(base_final$merge_data_final.PC_TT_UN)
summary(base_final$merge_data_final.AR_UT_UNID)
summary(base_final$merge_data_final.vv_construcao)
summary(base_final$merge_data_final.vv_terreno)
summary(base_final$merge_data_final.vv)
summary(base_final$merge_data_final.aliquota_real)
table(base_final$merge_data_final.Binaria_pos_trat)/length(base_final$merge_data_final.Binaria_pos_trat)
table(base_final$merge_data_final.tier_iptu_pre_trat)/length(base_final$merge_data_final.tier_iptu_pre_trat)
table(base_final$merge_data_final.tier_iptu_pos_trat)/length(base_final$merge_data_final.tier_iptu_pos_trat)



base_final[,length(base_final)+1]=as.numeric(base_final$merge_data_final.ANO_LAN==1995)
names(base_final)[ncol(base_final)]="binaria_1995"
base_final[,length(base_final)+1]=as.numeric(base_final$merge_data_final.ANO_LAN==1996)
names(base_final)[ncol(base_final)]="binaria_1996"
base_final[,length(base_final)+1]=as.numeric(base_final$merge_data_final.ANO_LAN==1997)
names(base_final)[ncol(base_final)]="binaria_1997"
base_final[,length(base_final)+1]=as.numeric(base_final$merge_data_final.ANO_LAN==1998)
names(base_final)[ncol(base_final)]="binaria_1998"
base_final[,length(base_final)+1]=as.numeric(base_final$merge_data_final.ANO_LAN==1999)
names(base_final)[ncol(base_final)]="binaria_1999"
base_final[,length(base_final)+1]=as.numeric(base_final$merge_data_final.ANO_LAN==2000)
names(base_final)[ncol(base_final)]="binaria_2000"
base_final[,length(base_final)+1]=as.numeric(base_final$merge_data_final.ANO_LAN==2001)
names(base_final)[ncol(base_final)]="binaria_2001"
base_final[,length(base_final)+1]=as.numeric(base_final$merge_data_final.ANO_LAN==2002)
names(base_final)[ncol(base_final)]="binaria_2002"
base_final[,length(base_final)+1]=as.numeric(base_final$merge_data_final.ANO_LAN==2003)
names(base_final)[ncol(base_final)]="binaria_2003"
base_final[,length(base_final)+1]=as.numeric(base_final$merge_data_final.ANO_LAN==2004)
names(base_final)[ncol(base_final)]="binaria_2004"
base_final[,length(base_final)+1]=as.numeric(base_final$merge_data_final.ANO_LAN==2005)
names(base_final)[ncol(base_final)]="binaria_2005"
base_final[,length(base_final)+1]=as.numeric(base_final$merge_data_final.ANO_LAN==2006)
names(base_final)[ncol(base_final)]="binaria_2006"
base_final[,length(base_final)+1]=as.numeric(base_final$merge_data_final.ANO_LAN==2007)
names(base_final)[ncol(base_final)]="binaria_2007"
base_final[,length(base_final)+1]=as.numeric(base_final$merge_data_final.ANO_LAN==2008)
names(base_final)[ncol(base_final)]="binaria_2008"

base_final[,length(base_final)+1]=0
for(k in 1:length(base_final$merge_data_final.INDEX_MERGE)){
  if(base_final$merge_data_final.aliquota_real[k]>0.9){
    if(base_final$merge_data_final.aliquota_real[k]<1.10){
      base_final[k,length(base_final)]=1
    }
  }
}
names(base_final)[ncol(base_final)]="Grupo_tratado"

base_final[,length(base_final)+1]=as.numeric(as.numeric(base_final$merge_data_final.ANO_LAN)<2002)
names(base_final)[ncol(base_final)]="Pre_tratamento"
base_final[,length(base_final)+1]=as.numeric(as.numeric(base_final$merge_data_final.ANO_LAN)>=2002)
names(base_final)[ncol(base_final)]="Pos_tratamento"

table(base_final$Grupo_tratado)/length(base_final$Grupo_tratado)
table(base_final$Pos_tratamento)/length(base_final$Pos_tratamento)
table(base_final$Pos_tratamento*base_final$Grupo_tratado)/length(base_final$Pos_tratamento)

names(base_final)=gsub("merge_data_final.",names(base_final),replacement = "")



#######################################################
#################SEM COVARIAVEIS#######################
#######################################################
#DID 2X2
base_final[,length(base_final)+1]=0
for(k in 1:length(base_final$INDEX_MERGE)){
  if(base_final$aliquota_real[k]>0){
    if(base_final$aliquota_real[k]<0.9){
      base_final[k,length(base_final)]=1
    }
  }
}
base_final_lm=base_final[base_final[,length(base_final)]==0,]

base_final_lm=base_final[as.numeric(base_final$ANO_LAN)<2003,]
base_final_lm=base_final_lm[as.numeric(base_final_lm$ANO_LAN)>2000,]
#base_final_lm=base_final_lm[as.numeric(base_final_lm$ANO_LAN)!=2001,]
#Regressão 1 - sem cov
lm_1=lm(log(base_final_lm$PC_TT_UN)~base_final_lm$Pos_tratamento*base_final_lm$Grupo_tratado+base_final_lm$Grupo_tratado+base_final_lm$Pos_tratamento+
          base_final_lm$ELEV+base_final_lm$SUBPREF)
summary(lm_1)




base_final_lm=read.csv2("C:/Users/vitoria.wendt/Downloads/base_final.csv")

base_final_lm[,length(base_final_lm)+1]=0
for(k in 1:length(base_final_lm$Indice.da.Observacao)){
  if(base_final_lm$Aliquota.Real.Pos.Tratamento[k]>0){
    if(base_final_lm$Aliquota.Real.Pos.Tratamento[k]<1){
      base_final_lm[k,length(base_final_lm)]=1
    }
  }
}
names(base_final_lm)[ncol(base_final_lm)]="Grupo_tratado"

did=feols(log(Preco.de.Lancamento.do.Imovel) ~(Grupo_tratado):i(Ano.de.Lancamento,2002)|Grupo_tratado+Ano.de.Lancamento,data = base_final_lm)
summary(did)
iplot(did)












options(scipen=999)












#####################################################################
#NAO USEI
base_final_lm=rbind(base_final[base_final$binaria_2000==1,],base_final[base_final$binaria_1995==1,])
#base_final_lm=base_final
#Regressão 1 - sem cov
lm_1=lm(log(base_final_lm$PC_TT_UN)~base_final_lm$binaria_1995+base_final_lm$Grupo_tratado+
          base_final_lm$binaria_1995*base_final_lm$Grupo_tratado)
summary(lm_1)

base_final_lm=rbind(base_final[base_final$binaria_2000==1,],base_final[base_final$binaria_1996==1,])
#base_final_lm=base_final
#Regressão 1 - sem cov
lm_1=lm(log(base_final_lm$PC_TT_UN)~base_final_lm$binaria_1996+base_final_lm$Grupo_tratado+
          base_final_lm$binaria_1996*base_final_lm$Grupo_tratado)
summary(lm_1)


base_final_lm=rbind(base_final[base_final$binaria_2000==1,],base_final[base_final$binaria_1997==1,])
#base_final_lm=base_final
#Regressão 1 - sem cov
lm_1=lm(log(base_final_lm$PC_TT_UN)~base_final_lm$binaria_1997+base_final_lm$Grupo_tratado+
          base_final_lm$binaria_1997*base_final_lm$Grupo_tratado)
summary(lm_1)


base_final_lm=rbind(base_final[base_final$binaria_2000==1,],base_final[base_final$binaria_1998==1,])
#base_final_lm=base_final
#Regressão 1 - sem cov
lm_1=lm(log(base_final_lm$PC_TT_UN)~base_final_lm$binaria_1998+base_final_lm$Grupo_tratado+
          base_final_lm$binaria_1998*base_final_lm$Grupo_tratado)
summary(lm_1)


base_final_lm=rbind(base_final[base_final$binaria_2000==1,],base_final[base_final$binaria_1999==1,])
#base_final_lm=base_final
#Regressão 1 - sem cov
lm_1=lm(log(base_final_lm$PC_TT_UN)~base_final_lm$binaria_1999+base_final_lm$Grupo_tratado+
          base_final_lm$binaria_1999*base_final_lm$Grupo_tratado)
summary(lm_1)


base_final_lm=rbind(base_final[base_final$binaria_2000==1,],base_final[base_final$binaria_2000==1,])
#base_final_lm=base_final
#Regressão 1 - sem cov
lm_1=lm(log(base_final_lm$PC_TT_UN)~base_final_lm$binaria_2000+base_final_lm$Grupo_tratado+
          base_final_lm$binaria_2000*base_final_lm$Grupo_tratado)
summary(lm_1)

base_final_lm=rbind(base_final[base_final$binaria_2000==1,],base_final[base_final$binaria_2001==1,])
#base_final_lm=base_final
#Regressão 1 - sem cov
lm_1=lm(log(base_final_lm$PC_TT_UN)~base_final_lm$binaria_2001+base_final_lm$Grupo_tratado+
          base_final_lm$binaria_2001*base_final_lm$Grupo_tratado)
summary(lm_1)


base_final_lm=rbind(base_final[base_final$binaria_2000==1,],base_final[base_final$binaria_2002==1,])
#base_final_lm=base_final
#Regressão 1 - sem cov
lm_1=lm(log(base_final_lm$PC_TT_UN)~base_final_lm$binaria_2002+base_final_lm$Grupo_tratado+
          base_final_lm$binaria_2002*base_final_lm$Grupo_tratado)
summary(lm_1)

base_final_lm=rbind(base_final[base_final$binaria_2000==1,],base_final[base_final$binaria_2003==1,])
#base_final_lm=base_final
#Regressão 1 - sem cov
lm_1=lm(log(base_final_lm$PC_TT_UN)~base_final_lm$binaria_2003+base_final_lm$Grupo_tratado+
          base_final_lm$binaria_2003*base_final_lm$Grupo_tratado)
summary(lm_1)


base_final_lm=rbind(base_final[base_final$binaria_2000==1,],base_final[base_final$binaria_2004==1,])
#base_final_lm=base_final
#Regressão 1 - sem cov
lm_1=lm(log(base_final_lm$PC_TT_UN)~base_final_lm$binaria_2004+base_final_lm$Grupo_tratado+
          base_final_lm$binaria_2004*base_final_lm$Grupo_tratado)
summary(lm_1)




#Base completa
base_final_lm=base_final
#Regressão 1 - sem cov
lm_1=lm(log(base_final_lm$PC_TT_UN)~base_final_lm$binaria_2004+base_final_lm$Grupo_tratado+base_final_lm$binaria_2008+
          base_final_lm$binaria_2008*base_final_lm$Grupo_tratado+base_final_lm$binaria_2007+
          base_final_lm$binaria_2007*base_final_lm$Grupo_tratado+base_final_lm$binaria_2006+
          base_final_lm$binaria_2006*base_final_lm$Grupo_tratado+base_final_lm$binaria_2005+
          base_final_lm$binaria_2005*base_final_lm$Grupo_tratado+
          base_final_lm$binaria_2004*base_final_lm$Grupo_tratado+base_final_lm$binaria_2003+
          base_final_lm$binaria_2003*base_final_lm$Grupo_tratado+base_final_lm$binaria_2002+
          base_final_lm$binaria_2002*base_final_lm$Grupo_tratado+base_final_lm$binaria_2001+
          base_final_lm$binaria_2001*base_final_lm$Grupo_tratado+base_final_lm$binaria_2000+
          base_final_lm$binaria_2000*base_final_lm$Grupo_tratado+base_final_lm$binaria_1999+
          base_final_lm$binaria_1999*base_final_lm$Grupo_tratado+base_final_lm$binaria_1998+
          base_final_lm$binaria_1998*base_final_lm$Grupo_tratado+base_final_lm$binaria_1997+
          base_final_lm$binaria_1997*base_final_lm$Grupo_tratado+base_final_lm$binaria_1996+
          base_final_lm$binaria_1996*base_final_lm$Grupo_tratado+base_final_lm$binaria_1995+
          base_final_lm$binaria_1995*base_final_lm$Grupo_tratado+base_final_lm$ELEV+base_final_lm$AR_UT_UNID+base_final_lm$SUBPREF+base_final_lm$BANH_UNID)
summary(lm_1)
coef_regressao=c(0.40411,0.28831,	0.81627,	0.52449,	0.44266,	0.57778,	0.78585,0.74316,0.87510,0.86783,0.81749,0.98784,0.78772	)
anos=c(1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)
residuos_reg=c(0.14300,0.14232,0.13602,0.13025,0.12839,0.13039,0.12761,0.12488,0.12306,0.12282,0.12462,0.11285,0.1204)




plot(y=coef_regressao,x=anos,type = 'l',ylim = c(0.15,1.2))
lines(y=coef_regressao+residuos_reg,x=anos,col="blue")
lines(y=coef_regressao-residuos_reg,x=anos,col="green")
abline(v=2001,col="red")











#Regressão 2 - cov elevador, area util
lm_2=lm(log(base_final_lm$PC_TT_UN)~base_final_lm$ELEV+base_final_lm$AR_UT_UNID+base_final_lm$binaria_1995+base_final_lm$Grupo_tratado+
          base_final_lm$binaria_1995*base_final_lm$Grupo_tratado)
summary(lm_2)


#ATT sem covariáveis

vetor_ATT=vector()
anos=c(1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)

for (k in 1:length(anos)){
  vetor_ATT[k]=log(prod(merge_data_final$PC_TT_UN[merge_data_final$ANO_LAN[merge_data_final$aliquota_real<=0.89]==anos[k]]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO_LAN[merge_data_final$aliquota_real<=0.89]==anos[k]]))))-
    log(
      prod(merge_data_final$PC_TT_UN[merge_data_final$ANO_LAN[merge_data_final$aliquota_real<=0.89]==anos[k-1]]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO_LAN[merge_data_final$aliquota_real<=0.89]==anos[k-1]])))*
        prod(merge_data_final$PC_TT_UN[merge_data_final$ANO_LAN[merge_data_final$aliquota_real[merge_data_final$aliquota_real<=2]>0.89]==anos[k]]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO_LAN[merge_data_final$aliquota_real[merge_data_final$aliquota_real<=2]>0.89]==anos[k]])))/
        prod(merge_data_final$PC_TT_UN[merge_data_final$ANO_LAN[merge_data_final$aliquota_real[merge_data_final$aliquota_real<=2]>0.89]==anos[k-1]]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO_LAN[merge_data_final$aliquota_real[merge_data_final$aliquota_real<=2]>0.89]==anos[k-1]])))
    ) 
}


plot(y=vetor_ATT,x=anos,type = 'l')

#DiD com cov elevador, área util
