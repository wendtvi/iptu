

############################################################################
############################################################################
#################################DATA QUALITY###############################
############################################################################
############################################################################


merge_data$PC_TT_UN=as.double(gsub(",",".",as.character(merge_data$PC_TT_UN)))
merge_data$PC_M2_AT=as.double(gsub(",",".",as.character(merge_data$PC_M2_AT)))
merge_data$AR_UT_UNID=as.double(gsub(",",".",as.character(merge_data$AR_UT_UNID)))
merge_data$PC_M2_AU=as.double(gsub(",",".",as.character(merge_data$PC_M2_AU)))
merge_data$AR_TT_UNID=as.double(gsub(",",".",as.character(merge_data$AR_TT_UNID)))

merge_data=merge_data[duplicated(merge_data$INDEX_MERGE)==FALSE,]
#merge_data=merge_data[as.double(gsub(",",".",as.character(merge_data$vv)))<200000000000000,]
vv_normal=(merge_data$vv-mean(merge_data$vv,na.rm = TRUE))*sqrt(length(merge_data$vv))/sqrt(var(merge_data$vv,na.rm = TRUE))
vv_terreno_normal=(merge_data$vv_terreno-mean(merge_data$vv_terreno,na.rm = TRUE))*sqrt(length(merge_data$vv_terreno))/sqrt(var(merge_data$vv_terreno,na.rm = TRUE))
vv_const_normal=(merge_data$vv_construcao-mean(merge_data$vv_construcao,na.rm = TRUE))*sqrt(length(merge_data$vv_construcao))/sqrt(var(merge_data$vv_construcao,na.rm = TRUE))
pc_lanca_normal=(merge_data$PC_TT_UN-mean(merge_data$PC_TT_UN,na.rm = TRUE))*sqrt(length(merge_data$PC_TT_UN))/sqrt(var(merge_data$PC_TT_UN,na.rm = TRUE))

plot(vv_normal,pc_lanca_normal)
plot(vv_const_normal,pc_lanca_normal)
plot(vv_terreno_normal,pc_lanca_normal)


hist(merge_data$VALOR.DO.M2.DO.TERRENO,breaks = 50)
hist(as.double(merge_data$PC_M2_AT),breaks = 50)
hist(merge_data$AREA.DO.TERRENO,breaks = 50)
hist(as.double(merge_data$AR_TT_UNID),breaks = 50)
hist(merge_data$AREA.DO.TERRENO*merge_data$VALOR.DO.M2.DO.TERRENO,breaks = 50)
hist(as.double(merge_data$AR_TT_UNID)*as.double(merge_data$PC_M2_AT),breaks = 50)

plot(file_yuri$pv,file_yuri$m2av)
plot(log(merge_data$vv),log(merge_data$VALOR.DO.M2.DO.TERRENO*merge_data$AREA.DO.TERRENO)+
       merge_data$VALOR.DO.M2.DE.CONSTRUCAO*merge_data$AREA.CONSTRUIDA)
plot(log(as.double(merge_data$PC_M2_AT)*as.double(merge_data$AR_TT_UNID)+
           as.double(merge_data$PC_M2_AU)*as.double(merge_data$AR_UT_UNID)),log(merge_data$VALOR.DO.M2.DO.TERRENO*merge_data$AREA.DO.TERRENO+
                                                                                  merge_data$VALOR.DO.M2.DE.CONSTRUCAO*merge_data$AREA.CONSTRUIDA))


plot(log(merge_data$vv),log(merge_data$PC_TT_UN))
plot(log(merge_data$vv),log(merge_data$PC_TT_UN))
plot((merge_data$vv),(merge_data$PC_TT_UN))
line(log(merge_data$vv),log(merge_data$PC_TT_UN))

plot((merge_data$PC_TT_UN),(merge_data$VALOR.DO.M2.DO.TERRENO))
plot(log(merge_data$vv),log(merge_data$PC_TT_UN))
cor(log(merge_data$vv),log(merge_data$PC_TT_UN))


#retira observações com valor do terreno = 1 (erros na base)
merge_data=merge_data[merge_data$AREA.DO.TERRENO!=1,]
