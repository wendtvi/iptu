#setwd("C:/Users/vitor/Downloads")

load("~/ws_final.RData")
#Retira 10% dos valores mais extremos de Preço de Lançamento
merge_data_final=merge_data_final[merge_data_final$PC_TT_UN<=quantile(merge_data_final$PC_TT_UN,probs = seq(0, 1, 0.05))[18],]

#Seleciona da base completa apenas variáveis relevantes
base_final=data.frame(merge_data_final$INDEX_MERGE,merge_data_final$ANO_LAN,
                      merge_data_final$CEP.DO.IMOVEL,
                      merge_data_final$TIPO.DE.TERRENO,merge_data_final$TIPO.DE.USO.DO.IMOVEL,
                      merge_data_final$SUBPREF,merge_data_final$DORM_UNID,merge_data_final$BANH_UNID,merge_data_final$GAR_UNID,
                      merge_data_final$ELEV,merge_data_final$COB,merge_data_final$BLOCOS,merge_data_final$ANDARES,merge_data_final$AR_UT_UNID,merge_data_final$AR_TT_UNID,
                      merge_data_final$GAR_EMP,merge_data_final$PC_TT_UN,merge_data_final$vv_construcao,
                      merge_data_final$vv_terreno,merge_data_final$indice_condominio,
                      merge_data_final$vv,merge_data_final$tier_iptu_pre_trat,merge_data_final$valor_iptu_pre_trat,merge_data_final$tier_iptu_pos_trat,
                      merge_data_final$valor_iptu_pos_trat,merge_data_final$vv_tier50k,merge_data_final$vv_tier100k,merge_data_final$vv_tier200k,merge_data_final$vv_tier400k,
                      merge_data_final$vv_tier401k, merge_data_final$aliquota_real)   


#Retira todas observações que não são de tipo residência
base_final_temp=base_final[base_final$merge_data_final.TIPO.DE.USO.DO.IMOVEL=="resid\xeancia coletiva (mais de uma resid\xeancia no lote), exclusive corti\xe7o",]
base_final_temp=rbind(base_final_temp,base_final[base_final$merge_data_final.TIPO.DE.USO.DO.IMOVEL=="flat, residencial",])
base_final_temp=rbind(base_final_temp,base_final[base_final$merge_data_final.TIPO.DE.USO.DO.IMOVEL=="apartamento",])
base_final_temp=rbind(base_final_temp,base_final[base_final$merge_data_final.TIPO.DE.USO.DO.IMOVEL=="flat, n\xe3o residencial",])
base_final_temp=rbind(base_final_temp,base_final[base_final$merge_data_final.TIPO.DE.USO.DO.IMOVEL=="pr\xe9dio com uso exclusivamente residencial, n\xe3o em condom\xednio",])
base_final_temp=rbind(base_final_temp,base_final[base_final$merge_data_final.TIPO.DE.USO.DO.IMOVEL=="resid\xeancia",])
base_final_temp=rbind(base_final_temp,base_final[base_final$merge_data_final.TIPO.DE.USO.DO.IMOVEL=="resid\xeancia e outro uso (predomin\xe2ncia residencial)",])
base_final_temp=rbind(base_final_temp,base_final[base_final$merge_data_final.TIPO.DE.USO.DO.IMOVEL=="pr\xe9dio com uso misto, predomin\xe2ncia de uso residencial, n\xe3o em condom\xednio",])
base_final_temp=rbind(base_final_temp,base_final[base_final$merge_data_final.TIPO.DE.USO.DO.IMOVEL=="resid\xeancia",])
base_final_temp$merge_data_final.TIPO.DE.USO.DO.IMOVEL="Residência"

base_final=base_final_temp

#Corrigi valor venal por IPCA (Na tese do Yuri ele comenta que a prefeitura corrigi por IPCA o VV para cálculo de faixa de IPTU)
setwd("C:/Users/vitor/Downloads")
ipca=read.csv2("ipca.csv")
for (i in 1:nrow(base_final)){
  for (k in 1:nrow(ipca)){
    if(ipca$YEAR[k]==base_final$merge_data_final.ANO_LAN[i])base_final$merge_data_final.vv[i]=base_final$merge_data_final.vv[i]*ipca[k,4]
  }
}

#Recalcula tiers de nova alíquota de IPTU por VV corrigido
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

#base_final=base_final[base_final$merge_data_final.vv<=518318.897,]

table(base_final$merge_data_final.tier_iptu_pos_trat)/length(base_final$merge_data_final.tier_iptu_pos_trat)

#Separa VV total por faixa de IPTU 
base_final$merge_data_final.vv_tier50k=0
base_final$merge_data_final.vv_tier100k=0
base_final$merge_data_final.vv_tier200k=0
base_final$merge_data_final.vv_tier400k=0
base_final$merge_data_final.vv_tier401k=0

for (j in 1:length(base_final$merge_data_final.tier_iptu_pos_trat)){
  base_final$merge_data_final.vv_tier50k[j]=min(base_final$merge_data_final.vv[j],50000)
  base_final$merge_data_final.vv_tier100k[j]=min(base_final$merge_data_final.vv[j]-50000,50000)
  base_final$merge_data_final.vv_tier200k[j]=min(base_final$merge_data_final.vv[j]-100000,100000)
  base_final$merge_data_final.vv_tier400k[j]=min(base_final$merge_data_final.vv[j]-200000,200000)
  base_final$merge_data_final.vv_tier401k[j]=base_final$merge_data_final.vv[j]-400000
}
base_final$merge_data_final.vv_tier50k[base_final$merge_data_final.vv_tier50k<0]=0
base_final$merge_data_final.vv_tier100k[base_final$merge_data_final.vv_tier100k<0]=0
base_final$merge_data_final.vv_tier200k[base_final$merge_data_final.vv_tier200k<0]=0
base_final$merge_data_final.vv_tier400k[base_final$merge_data_final.vv_tier400k<0]=0
base_final$merge_data_final.vv_tier401k[base_final$merge_data_final.vv_tier401k<0]=0

#Cálculo do valor de IPTU pré tratamento
base_final$merge_data_final.valor_iptu_pre_trat=base_final$merge_data_final.vv*base_final$merge_data_final.tier_iptu_pre_trat

#Cálculo do valor de IPTU pós mudança de alíquota
base_final$merge_data_final.valor_iptu_pos_trat=base_final$merge_data_final.valor_iptu_pre_trat
base_final$merge_data_final.valor_iptu_pos_trat=base_final$merge_data_final.valor_iptu_pos_trat+(base_final$merge_data_final.vv_tier50k*(-0.002)+base_final$merge_data_final.vv_tier100k*0+base_final$merge_data_final.vv_tier200k*0.002+
                                                                                                   base_final$merge_data_final.vv_tier400k*0.004+base_final$merge_data_final.vv_tier401k*0.006)
base_final$merge_data_final.valor_iptu_pos_trat[base_final$merge_data_final.vv<=20000]=0

#Calcula alíquota real
base_final$merge_data_final.aliquota_real=base_final$merge_data_final.valor_iptu_pos_trat/base_final$merge_data_final.valor_iptu_pre_trat


merge_data_final=base_final
names(merge_data_final)=gsub("merge_data_final.",names(merge_data_final),replacement = "")

#Redefine nome das variáveis (apenas para tornar base mais legível)
names(merge_data_final)=c("Indice.da.Observacao","Ano.de.Lancamento","Cep.do.Imovel","Tipo.de.Terreno","Tipo.de.Imovel","Subprefeitura","Numero.de.Dormitorios",
                          "Numero.de.Banheiros","Numero.de.Garagens.do.Imovel","Numero.de.Elevadores.do.Empreendimento","Cobertura","Blocos",
                          "Andares.do.Empreendimento","Area.Util.do.Imovel","Area.Total.do.Imovel","Numero.de.Garagens.do.Empreendimento","Preco.de.Lancamento.do.Imovel","Valor.Venal.da.Construcao","Valor.Venal.do.Terreno",
                          "Indice.de.Condominio","Valor.Venal.Total","Aliquota.Nominal.Pre.Tratamento","Valor.IPTU.Pre.Tratamento","Aliquota.Nominal.Pos.Tratamento",
                          "Valor.IPTU.Pos.Tratamento","Valor.Venal.tier.50k","Valor.Venal.tier.100k","Valor.Venal.tier.200k","Valor.Venal.tier.400k","Valor.Venal.tier.superior",
                          "Aliquota.Real.Pos.Tratamento")


write.csv2(merge_data_final,"~/base_final.csv")
