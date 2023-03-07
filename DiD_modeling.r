merge_data_final=merge_data_final[merge_data_final$PC_TT_UN<=quantile(merge_data_final$PC_TT_UN,probs = seq(0, 1, 0.10))[10],]

base_final=data.frame(merge_data_final$INDEX_MERGE,merge_data_final$ANO.DO.EXERCICIO,merge_data_final$NUMERO.DO.CONDOMINIO,merge_data_final$BAIRRO.DO.IMOVEL,
                      merge_data_final$CEP.DO.IMOVEL,merge_data_final$QUANTIDADE.DE.ESQUINAS.FRENTES,merge_data_final$FRACAO.IDEAL,merge_data_final$ANO.DA.CONSTRUCAO.CORRIGIDO,
                      merge_data_final$TIPO.DE.TERRENO,merge_data_final$TIPO.DE.USO.DO.IMOVEL,merge_data_final$FATOR.DE.OBSOLESCENCIA,merge_data_final$SCORE_STR_INDEX,
                      merge_data_final$SCORE_PC_TERR_CONST,merge_data_final$SUBPREF,merge_data_final$DORM_UNID,merge_data_final$BANH_UNID,merge_data_final$GAR_UNID,
                      merge_data_final$ELEV,merge_data_final$COB,merge_data_final$BLOCOS,merge_data_final$ANDARES,merge_data_final$AR_UT_UNID,merge_data_final$AR_TT_UNID,
                      merge_data_final$GAR_EMP,merge_data_final$PC_TT_UN,merge_data_final$AR_TT_UNID,merge_data_final$AR_UT_UNID,merge_data_final$vv_construcao,merge_data_final$FATOR.TIPO.DE.PROFUNDIDADE,
                      merge_data_final$FATOR.TIPO.DE.TERRENO,merge_data_final$FATOR.TIPO.DE.CONDOMINIO,merge_data_final$SUBDIVISAO.URBANA,merge_data_final$vv_terreno,merge_data_final$indice_condominio,
                      merge_data_final$vv,merge_data_final$Binaria_pos_trat,merge_data_final$tier_iptu_pre_trat,merge_data_final$valor_iptu_pre_trat,merge_data_final$tier_iptu_pos_trat,
                      merge_data_final$valor_iptu_pos_trat,merge_data_final$vv_tier20k,merge_data_final$vv_tier50k,merge_data_final$vv_tier100k,merge_data_final$vv_tier200k,merge_data_final$vv_tier400k,
                      merge_data_final$vv_tier401k, merge_data_final$aliquota_real)   

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
table(base_final$)
summary(base_final$merge_data_final.PC_TT_UN)
summary(base_final$merge_data_final.AR_UT_UNID)
summary(base_final$merge_data_final.vv_construcao)
summary(base_final$merge_data_final.vv_terreno)
summary(base_final$merge_data_final.vv)
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


base_final[,length(base_final)+1]=as.numeric(base_final$merge_data_final.aliquota_real<=0.89)
names(base_final)[ncol(base_final)]="Grupo_tratado"
base_final[,length(base_final)+1]=as.numeric(base_final$merge_data_final.aliquota_real>0.89)
names(base_final)[ncol(base_final)]="Grupo_controle"

names(base_final)=gsub("merge_data_final.",names(base_final),replacement = "")

#DID 2X2
base_final_lm=rbind(base_final[base_final$binaria_2001==1,],base_final[base_final$binaria_2002==1,])
#Regressão 1 - sem cov
lm_1=lm((base_final_lm$PC_TT_UN)~base_final_lm$binaria_2002+base_final_lm$Grupo_tratado+
          base_final_lm$binaria_2002*base_final_lm$Grupo_tratado)
summary(lm_1)

#Regressão 2 - cov elevador, area util
lm_2=lm(log(base_final_lm$PC_TT_UN)~base_final_lm$ELEV+base_final_lm$AR_UT_UNID+base_final_lm$binaria_2002+base_final_lm$Grupo_tratado+
          base_final_lm$binaria_2002*base_final_lm$Grupo_tratado)
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
