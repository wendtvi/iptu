load("~/ws_final.RData")
merge_data_final=merge_data_final[merge_data_final$PC_TT_UN<=quantile(merge_data_final$PC_TT_UN,probs = seq(0, 1, 0.2))[5],]

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


base_final_temp=base_final[base_final$merge_data_final.TIPO.DE.USO.DO.IMOVEL=="resid\xeancia coletiva (mais de uma resid\xeancia no lote), exclusive corti\xe7o",]
base_final_temp=rbind(base_final_temp,base_final[base_final$merge_data_final.TIPO.DE.USO.DO.IMOVEL=="flat, residencial",])
base_final_temp=rbind(base_final_temp,base_final[base_final$merge_data_final.TIPO.DE.USO.DO.IMOVEL=="apartamento",])
base_final_temp=rbind(base_final_temp,base_final[base_final$merge_data_final.TIPO.DE.USO.DO.IMOVEL=="flat, n\xe3o residencial",])
base_final_temp=rbind(base_final_temp,base_final[base_final$merge_data_final.TIPO.DE.USO.DO.IMOVEL=="pr\xe9dio com uso exclusivamente residencial, n\xe3o em condom\xednio",])
base_final_temp=rbind(base_final_temp,base_final[base_final$merge_data_final.TIPO.DE.USO.DO.IMOVEL=="resid\xeancia",])
base_final_temp=rbind(base_final_temp,base_final[base_final$merge_data_final.TIPO.DE.USO.DO.IMOVEL=="resid\xeancia e outro uso (predomin\xe2ncia residencial)",])
base_final_temp=rbind(base_final_temp,base_final[base_final$merge_data_final.TIPO.DE.USO.DO.IMOVEL=="pr\xe9dio com uso misto, predomin\xe2ncia de uso residencial, n\xe3o em condom\xednio",])
base_final_temp=rbind(base_final_temp,base_final[base_final$merge_data_final.TIPO.DE.USO.DO.IMOVEL=="resid\xeancia",])

base_final=base_final_temp

ipca=read.csv2("ipca.csv")

for (i in 1:nrow(base_final)){
  for (k in 1:nrow(ipca)){
    if(ipca$YEAR[k]==base_final$merge_data_final.ANO.DO.EXERCICIO[i])base_final$merge_data_final.vv[i]=base_final$merge_data_final.vv[i]*ipca[k,4]
  }
}

#for (i in 1:nrow(base_final)){
#  for (k in 1:nrow(ipca)){
#    if(ipca$YEAR[k]==base_final$merge_data_final.ANO.DO.EXERCICIO[i])base_final$merge_data_final.PC_TT_UN[i]=base_final$merge_data_final.PC_TT_UN[i]*ipca[k,4]
#  }
#}

base_final$merge_data_final.tier_iptu_pos_trat=0.0
base_final_temp=base_final[base_final$merge_data_final.vv<=20000,]
base_final=base_final[base_final$merge_data_final.vv>20000,]
base_final$merge_data_final.tier_iptu_pos_trat=0.008
base_final_temp=rbind(base_final_temp,base_final[base_final$merge_data_final.vv<=50000,])
base_final=base_final[base_final$merge_data_final.vv>50000,]
base_final$merge_data_final.tier_iptu_pos_trat=0.01
base_final_temp=rbind(base_final_temp,base_final[base_final$merge_data_final.vv<=100000,])
base_final=base_final[base_final$merge_data_final.vv>100000,]
base_final$merge_data_final.tier_iptu_pos_trat=0.012
base_final_temp=rbind(base_final_temp,base_final[base_final$merge_data_final.vv<=200000,])
base_final=base_final[base_final$merge_data_final.vv>200000,]
base_final$merge_data_final.tier_iptu_pos_trat=0.014
base_final_temp=rbind(base_final_temp,base_final[base_final$merge_data_final.vv<=400000,])
base_final=base_final[base_final$merge_data_final.vv>400000,]
base_final$merge_data_final.tier_iptu_pos_trat=0.016
base_final_temp=rbind(base_final_temp,base_final[base_final$merge_data_final.vv>400000,])

base_final=base_final_temp

#base_final=base_final[base_final$merge_data_final.vv<=475582.27,]


table(base_final$merge_data_final.tier_iptu_pos_trat)/length(base_final$merge_data_final.tier_iptu_pos_trat)


base_final$merge_data_final.vv_tier20k=0
base_final$merge_data_final.vv_tier50k=0
base_final$merge_data_final.vv_tier100k=0
base_final$merge_data_final.vv_tier200k=0
base_final$merge_data_final.vv_tier400k=0
base_final$merge_data_final.vv_tier401k=0

for (j in 1:length(base_final$tier_iptu_pos_trat)){
  base_final$merge_data_final.vv_tier20k[j]=min(base_final$merge_data_final.vv[j],20000)
  base_final$merge_data_final.vv_tier50k[j]=min(base_final$merge_data_final.vv[j]-20000,50000)
  base_final$merge_data_final.vv_tier100k[j]=min(base_final$merge_data_final.vv[j]-50000,50000)
  base_final$merge_data_final.vv_tier200k[j]=min(base_final$merge_data_final.vv[j]-100000,100000)
  base_final$merge_data_final.vv_tier400k[j]=min(base_final$merge_data_final.vv[j]-200000,200000)
  base_final$merge_data_final.vv_tier401k[j]=base_final$merge_data_final.vv[j]-400000
}
base_final$merge_data_final.vv_tier50k[base_final$merge_data_final.vv_tier20k<0]=0
base_final$merge_data_final.vv_tier50k[base_final$merge_data_final.vv_tier50k<0]=0
base_final$merge_data_final.vv_tier100k[base_final$merge_data_final.vv_tier100k<0]=0
base_final$merge_data_final.vv_tier200k[base_final$merge_data_final.vv_tier200k<0]=0
base_final$merge_data_final.vv_tier400k[base_final$merge_data_final.vv_tier400k<0]=0
base_final$merge_data_final.vv_tier401k[base_final$merge_data_final.vv_tier401k<0]=0

base_final$merge_data_final.valor_iptu_pos_trat=base_final$merge_data_final.valor_iptu_pos_trat+(base_final$merge_data_final.vv_tier50k*(-0.002)+base_final$merge_data_final.vv_tier100k*0+base_final$merge_data_final.vv_tier200k*0.002+
                                                                                                   base_final$merge_data_final.vv_tier400k*0.004+base_final$merge_data_final.vv_tier401k*0.006)
base_final$merge_data_final.valor_iptu_pos_trat[base_final$merge_data_final.vv<=20000]=0


base_final$merge_data_final.aliquota_real=base_final$merge_data_final.valor_iptu_pos_trat/base_final$merge_data_final.valor_iptu_pre_trat




