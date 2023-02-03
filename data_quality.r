

############################################################################
############################################################################
#################################DATA QUALITY###############################
############################################################################
############################################################################

#Gráficos principais
par(mfrow=c(1,1))
plot(log(merge_data_final$vv),log(merge_data_final$PC_TT_UN), main = "Preço de lançamento vs Valor Venal em log",xlab = "log do Valor Venal",ylab = "log do Preço de Lançamento")

par(mfrow=c(1,2))
hist(merge_data_final$AREA.DO.TERRENO*merge_data_final$VALOR.DO.M2.DO.TERRENO,breaks = 500,xlim = c(0,8000000),main = "Histograma de dados da base Geosampa", xlab = "valor m² multiplicado pela metragem")
hist(as.double(merge_data_final$AR_TT_UNID)*as.double(merge_data_final$PC_M2_AT),breaks = 50,xlim = c(0,8000000),main = "Histograma de dados da base de Lançamentos", xlab = "valor m² multiplicado pela metragem")


par(mfrow=c(1,1))
plot(as.factor(merge_data$tier_iptu_pos_trat))
hist((merge_data$aliquota_real),breaks = 20)


#retira observações com valor do terreno = 1 (erros na base)
merge_data_final=merge_data_final[merge_data_final$AREA.DO.TERRENO!=1,]


#Gráficos adicionais

merge_data_final$PC_TT_UN=as.double(gsub(",",".",as.character(merge_data_final$PC_TT_UN)))
merge_data_final$PC_M2_AT=as.double(gsub(",",".",as.character(merge_data_final$PC_M2_AT)))
merge_data_final$AR_UT_UNID=as.double(gsub(",",".",as.character(merge_data_final$AR_UT_UNID)))
merge_data_final$PC_M2_AU=as.double(gsub(",",".",as.character(merge_data_final$PC_M2_AU)))
merge_data_final$AR_TT_UNID=as.double(gsub(",",".",as.character(merge_data_final$AR_TT_UNID)))

merge_data_final=merge_data_final[duplicated(merge_data_final$INDEX_MERGE)==FALSE,]
#merge_data_final=merge_data_final[as.double(gsub(",",".",as.character(merge_data_final$vv)))<200000000000000,]
vv_normal=(merge_data_final$vv-mean(merge_data_final$vv,na.rm = TRUE))*sqrt(length(merge_data_final$vv))/sqrt(var(merge_data_final$vv,na.rm = TRUE))
vv_terreno_normal=(merge_data_final$vv_terreno-mean(merge_data_final$vv_terreno,na.rm = TRUE))*sqrt(length(merge_data_final$vv_terreno))/sqrt(var(merge_data_final$vv_terreno,na.rm = TRUE))
vv_const_normal=(merge_data_final$vv_construcao-mean(merge_data_final$vv_construcao,na.rm = TRUE))*sqrt(length(merge_data_final$vv_construcao))/sqrt(var(merge_data_final$vv_construcao,na.rm = TRUE))
pc_lanca_normal=(merge_data_final$PC_TT_UN-mean(merge_data_final$PC_TT_UN,na.rm = TRUE))*sqrt(length(merge_data_final$PC_TT_UN))/sqrt(var(merge_data_final$PC_TT_UN,na.rm = TRUE))


plot((merge_data_final$PC_TT_UN),(merge_data_final$VALOR.DO.M2.DO.TERRENO))
cor(log(merge_data_final$vv),log(merge_data_final$PC_TT_UN))
plot(file_yuri$pv,file_yuri$m2av)
plot(log(merge_data_final$vv),log(merge_data_final$VALOR.DO.M2.DO.TERRENO*merge_data_final$AREA.DO.TERRENO)+
       merge_data_final$VALOR.DO.M2.DE.CONSTRUCAO*merge_data_final$AREA.CONSTRUIDA)
plot(log(as.double(merge_data_final$PC_M2_AT)*as.double(merge_data_final$AR_TT_UNID)+
           as.double(merge_data_final$PC_M2_AU)*as.double(merge_data_final$AR_UT_UNID)),log(merge_data_final$VALOR.DO.M2.DO.TERRENO*merge_data_final$AREA.DO.TERRENO+
                                                                                              merge_data_final$VALOR.DO.M2.DE.CONSTRUCAO*merge_data_final$AREA.CONSTRUIDA))

hist(merge_data_final$VALOR.DO.M2.DO.TERRENO,breaks = 10)
hist(as.double(merge_data_final$PC_M2_AT),breaks = 10)
hist(merge_data_final$AREA.DO.TERRENO,breaks = 50)
hist(as.double(merge_data_final$AR_TT_UNID),breaks = 50)
