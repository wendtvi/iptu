

##############################################################
##################DESCRITIVAS#################################
##############################################################
table(base_final$merge_data_final.ANO.DO.EXERCICIO)/length(base_final$merge_data_final.ANO.DO.EXERCICIO)
table(base_final$merge_data_final.SUBPREF)/length(base_final$merge_data_final.SUBPREF)
table(base_final$merge_data_final.TIPO.DE.USO.DO.IMOVEL)/length(base_final$merge_data_final.TIPO.DE.USO.DO.IMOVEL)
table(base_final$merge_data_final.ELEV)/length(base_final$merge_data_final.ELEV)
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



base_final[,length(base_final)+1]=as.numeric(base_final$merge_data_final.ANO.DO.EXERCICIO==1995)
names(base_final)[ncol(base_final)]="binaria_1995"
base_final[,length(base_final)+1]=as.numeric(base_final$merge_data_final.ANO.DO.EXERCICIO==1996)
names(base_final)[ncol(base_final)]="binaria_1996"
base_final[,length(base_final)+1]=as.numeric(base_final$merge_data_final.ANO.DO.EXERCICIO==1997)
names(base_final)[ncol(base_final)]="binaria_1997"
base_final[,length(base_final)+1]=as.numeric(base_final$merge_data_final.ANO.DO.EXERCICIO==1998)
names(base_final)[ncol(base_final)]="binaria_1998"
base_final[,length(base_final)+1]=as.numeric(base_final$merge_data_final.ANO.DO.EXERCICIO==1999)
names(base_final)[ncol(base_final)]="binaria_1999"
base_final[,length(base_final)+1]=as.numeric(base_final$merge_data_final.ANO.DO.EXERCICIO==2000)
names(base_final)[ncol(base_final)]="binaria_2000"
base_final[,length(base_final)+1]=as.numeric(base_final$merge_data_final.ANO.DO.EXERCICIO==2001)
names(base_final)[ncol(base_final)]="binaria_2001"
base_final[,length(base_final)+1]=as.numeric(base_final$merge_data_final.ANO.DO.EXERCICIO==2002)
names(base_final)[ncol(base_final)]="binaria_2002"
base_final[,length(base_final)+1]=as.numeric(base_final$merge_data_final.ANO.DO.EXERCICIO==2003)
names(base_final)[ncol(base_final)]="binaria_2003"
base_final[,length(base_final)+1]=as.numeric(base_final$merge_data_final.ANO.DO.EXERCICIO==2004)
names(base_final)[ncol(base_final)]="binaria_2004"
base_final[,length(base_final)+1]=as.numeric(base_final$merge_data_final.ANO.DO.EXERCICIO==2005)
names(base_final)[ncol(base_final)]="binaria_2005"
base_final[,length(base_final)+1]=as.numeric(base_final$merge_data_final.ANO.DO.EXERCICIO==2006)
names(base_final)[ncol(base_final)]="binaria_2006"
base_final[,length(base_final)+1]=as.numeric(base_final$merge_data_final.ANO.DO.EXERCICIO==2007)
names(base_final)[ncol(base_final)]="binaria_2007"
base_final[,length(base_final)+1]=as.numeric(base_final$merge_data_final.ANO.DO.EXERCICIO==2008)
names(base_final)[ncol(base_final)]="binaria_2008"


base_final[,length(base_final)+1]=as.numeric(base_final$merge_data_final.aliquota_real<0.89)
names(base_final)[ncol(base_final)]="Grupo_tratado"
base_final[,length(base_final)+1]=as.numeric(base_final$merge_data_final.aliquota_real>=0.89)
names(base_final)[ncol(base_final)]="Grupo_controle"

base_final[,length(base_final)+1]=as.numeric(as.numeric(base_final$merge_data_final.ANO.DO.EXERCICIO)<=2001)
names(base_final)[ncol(base_final)]="Pre_tratamento"
base_final[,length(base_final)+1]=as.numeric(as.numeric(base_final$merge_data_final.ANO.DO.EXERCICIO)>2001)
names(base_final)[ncol(base_final)]="Pos_tratamento"


table(base_final$Grupo_controle)/length(base_final$Grupo_controle)
table(base_final$Pre_tratamento)/length(base_final$Pre_tratamento)

names(base_final)=gsub("merge_data_final.",names(base_final),replacement = "")



#######################################################
#################SEM COVARIAVEIS#######################
#######################################################
#DID 2X2
base_final_lm=base_final[as.numeric(base_final$ANO.DO.EXERCICIO)<2009,]
base_final_lm=base_final[as.numeric(base_final$ANO.DO.EXERCICIO)>1994,]
#Regressão 1 - sem cov
lm_1=lm(log(base_final_lm$PC_TT_UN)~base_final_lm$Pos_tratamento*base_final_lm$Grupo_tratado+base_final_lm$Grupo_tratado+base_final_lm$Pos_tratamento)
summary(lm_1)



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
lm_1=lm(log(base_final_lm$PC_TT_UN)~base_final_lm$binaria_2004+base_final_lm$Grupo_tratado+
          base_final_lm$binaria_2004*base_final_lm$Grupo_tratado+base_final_lm$binaria_2003+
          base_final_lm$binaria_2003*base_final_lm$Grupo_tratado+base_final_lm$binaria_2002+
          base_final_lm$binaria_2002*base_final_lm$Grupo_tratado+base_final_lm$binaria_2001+
          base_final_lm$binaria_2001*base_final_lm$Grupo_tratado+base_final_lm$binaria_2000+
          base_final_lm$binaria_2000*base_final_lm$Grupo_tratado+base_final_lm$binaria_1999+
          base_final_lm$binaria_1999*base_final_lm$Grupo_tratado+base_final_lm$binaria_1998+
          base_final_lm$binaria_1998*base_final_lm$Grupo_tratado+base_final_lm$binaria_1997+
          base_final_lm$binaria_1997*base_final_lm$Grupo_tratado+base_final_lm$binaria_1996+
          base_final_lm$binaria_1996*base_final_lm$Grupo_tratado+base_final_lm$binaria_1995+
          base_final_lm$binaria_1995*base_final_lm$Grupo_tratado)
summary(lm_1)















#Regressão 2 - cov elevador, area util
lm_2=lm(log(base_final_lm$PC_TT_UN)~base_final_lm$ELEV+base_final_lm$AR_UT_UNID+base_final_lm$binaria_1995+base_final_lm$Grupo_tratado+
          base_final_lm$binaria_1995*base_final_lm$Grupo_tratado)
summary(lm_2)


#ATT sem covariáveis

vetor_ATT=vector()
anos=c(1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)

for (k in 1:length(anos)){
  vetor_ATT[k]=log(prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<=0.89]==anos[k]]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<=0.89]==anos[k]]))))-
    log(
      prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<=0.89]==anos[k-1]]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real<=0.89]==anos[k-1]])))*
        prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<=2]>0.89]==anos[k]]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<=2]>0.89]==anos[k]])))/
        prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<=2]>0.89]==anos[k-1]]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<=2]>0.89]==anos[k-1]])))
    ) 
}


plot(y=vetor_ATT,x=anos,type = 'l')

#DiD com cov elevador, área util
