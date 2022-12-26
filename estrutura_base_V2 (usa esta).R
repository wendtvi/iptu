#ESTRUTURA BASE DE IPTU (Centro de Estudos da Metrópole CEM)
#install.packages("haven")
#install.packages("foreign")
#install.packages("RecordLinkage")
library(RecordLinkage)
library(foreign)
library(haven)
#file_yuri=read_dta("G:/.shortcut-targets-by-id/1RoOXMnfWB1HXnG_ZYAvPJXB8hKlTZ5O0/iptu/data/clean/main_dataset_yuri.dta")
#file_completo_imoveis=read_dta("G:/.shortcut-targets-by-id/1RoOXMnfWB1HXnG_ZYAvPJXB8hKlTZ5O0/iptu/data/imoveis_runcompleto.dta")
file_dbf=read.dbf("G:/.shortcut-targets-by-id/1RoOXMnfWB1HXnG_ZYAvPJXB8hKlTZ5O0/iptu/data/raw/LANRES_85_13_RMSP_CEM.dbf")
file_dbf=file_dbf[file_dbf$MUNICIPIO=="SAO PAULO",]
file_dbf[,ncol(file_dbf)+1]=NA
file_dbf$LOGRADOURO=as.character(file_dbf$LOGRADOURO)
file_dbf$LOGRADOURO[11362]="Rua Constancio"
file_dbf$LOGRADOURO[13247]="Colonia D'Assuncao"
file_dbf=file_dbf[file_dbf$ANO_LAN>1994,]
#file_dbf=file_dbf[file_dbf$ANO_LAN<=2001,]
file_dbf$CEP=as.character(file_dbf$CEP)
file_dbf[,ncol(file_dbf)]=toupper(paste(as.character(file_dbf$LOGRADOURO),as.character(file_dbf$NUM),as.character(file_dbf$CEP),sep=""))
file_dbf[,ncol(file_dbf)]=gsub(" ","",file_dbf[,ncol(file_dbf)])
file_dbf[,ncol(file_dbf)]=gsub("'","",file_dbf[,ncol(file_dbf)])
colnames(file_dbf)[length(colnames(file_dbf))]="INDEX_MERGE"
iconv(file_dbf$INDEX_MERGE, from = 'UTF-8', to = 'ASCII//TRANSLIT')

file_dbf$PC_TT_UN=as.double(gsub(",",".",as.character(file_dbf$PC_TT_UN)))
file_dbf$PC_M2_AT=as.double(gsub(",",".",as.character(file_dbf$PC_M2_AT)))
file_dbf$AR_UT_UNID=as.double(gsub(",",".",as.character(file_dbf$AR_UT_UNID)))
file_dbf$PC_M2_AU=as.double(gsub(",",".",as.character(file_dbf$PC_M2_AU)))
file_dbf$AR_TT_UNID=as.double(gsub(",",".",as.character(file_dbf$AR_TT_UNID)))


#ESTRUTURA BASES DE CÁLCULO VENAL (GEOSAMPA)
for(i in 1995:2015) {
  if (i!=2011){
    year = paste("data_", i, sep = "")
    file_temp=paste("G:/.shortcut-targets-by-id/1RoOXMnfWB1HXnG_ZYAvPJXB8hKlTZ5O0/iptu/data/raw/EG_",i,sep ="")
    file=paste(file_temp,".csv",sep="")
    data_matrix=read.csv2(file)
    data_matrix=data_matrix[data_matrix$ANO.DA.CONSTRUCAO.CORRIGIDO>1990,]
    data_matrix[,ncol(data_matrix)+1]=toupper(paste(as.character(data_matrix$NOME.DE.LOGRADOURO.DO.IMOVEL),
                                                    as.character(data_matrix$NUMERO.DO.IMOVEL),as.character(data_matrix$CEP.DO.IMOVEL),sep=""))
    data_matrix[,ncol(data_matrix)]=gsub("R          ","",data_matrix[,ncol(data_matrix)])
    data_matrix[,ncol(data_matrix)]=gsub("AV         ","",data_matrix[,ncol(data_matrix)])
    data_matrix[,ncol(data_matrix)]=gsub(" ","",data_matrix[,ncol(data_matrix)])
    data_matrix[,ncol(data_matrix)]=gsub("'","",data_matrix[,ncol(data_matrix)])
    colnames(data_matrix)[length(colnames(data_matrix))]="INDEX_MERGE"
    iconv(data_matrix$INDEX_MERGE, from = 'UTF-8', to = 'ASCII//TRANSLIT')
    data_matrix[,ncol(data_matrix)+1]=NA
    colnames(data_matrix)[length(colnames(data_matrix))]="SCORE_STR_INDEX"
    data_matrix[,ncol(data_matrix)+1]=NA
    colnames(data_matrix)[length(colnames(data_matrix))]="SCORE_PC_TERR_CONST"
    file_dbf_temp=file_dbf
    file_dbf_temp=file_dbf_temp[file_dbf_temp$ANO_LAN<=i,]
    #file_dbf_temp=file_dbf_temp[file_dbf_temp$ANO_LAN>=i-5,]
    rel_precos_terr_const=-1
    n=1
    l=1
    file_dbf_temp=file_dbf_temp[order(file_dbf_temp$INDEX_MERGE),]
    data_matrix=data_matrix[order(data_matrix$INDEX_MERGE),]
    data_matrix_temp=data_matrix[1,]
    data_matrix_temp=data_matrix_temp[-1,]
    p=0
    for(n in 1:length(file_dbf_temp$ID)){
      l=1
      for (l in 1:length(data_matrix$INDEX_MERGE)){
        score_str_matching=levenshteinSim(data_matrix$INDEX_MERGE[l],file_dbf_temp$INDEX_MERGE[n])
        rel_precos_terr_const=(  as.double(gsub(",",".",as.character(file_dbf_temp$AR_TT_UNID[n])))+
                                 as.double(gsub(",",".",as.character(file_dbf_temp$AR_UT_UNID[n]))))/
          ((data_matrix$AREA.DO.TERRENO[l])+
             (data_matrix$AREA.CONSTRUIDA[l]))
        if (score_str_matching>0.80 && (rel_precos_terr_const>0.7 && rel_precos_terr_const<1.4)){
          p=p+1
          data_matrix$INDEX_MERGE[l]=file_dbf_temp$INDEX_MERGE[n]
          data_matrix$SCORE_STR_INDEX[l]=score_str_matching
          data_matrix$SCORE_PC_TERR_CONST[l]=rel_precos_terr_const
          data_matrix_temp[p,]=data_matrix[l,]
          data_matrix=data_matrix[-l,]
          break
        }
      }
    }
    merge_data_temp=merge(data_matrix_temp,file_dbf_temp,by="INDEX_MERGE")
    assign(year, merge_data_temp)
    rm(data_matrix,data_matrix_temp)
    if(i==1995)merge_data=merge_data_temp
    if(i!=1995){
      merge_data=rbind(merge_data,merge_data_temp,all = TRUE)
    }
    rm(merge_data_temp)
  }
}

merge_data$vv_construcao="NA"
merge_data$vv_construcao=merge_data$AREA.CONSTRUIDA*merge_data$VALOR.DO.M2.DE.CONSTRUCAO*merge_data$FATOR.DE.OBSOLESCENCIA

#construir fatores de profundidade, tipo de terreno, condominio
merge_data$FATOR.TIPO.DE.PROFUNDIDADE="NA"
merge_data$FATOR.TIPO.DE.TERRENO="NA"
merge_data$FATOR.TIPO.DE.CONDOMINIO="NA"
for(i in 1:nrow(merge_data)){
  if(merge_data$TIPO.DE.TERRENO[i]=="de 2 ou mais fre de duas ou mais") merge_data$FATOR.TIPO.DE.TERRENO=1#provisario
  if(merge_data$TIPO.DE.TERRENO[i]=="de esquina de esquina, em Z desvio ferroviar") merge_data$FATOR.TIPO.DE.TERRENO=1#provisario
  if(merge_data$TIPO.DE.TERRENO[i]=="lote de fundos") merge_data$FATOR.TIPO.DE.TERRENO=0.6
  if(merge_data$TIPO.DE.TERRENO[i]=="lote encravado") merge_data$FATOR.TIPO.DE.TERRENO=0.5
  if(merge_data$TIPO.DE.TERRENO[i]=="normal") merge_data$FATOR.TIPO.DE.TERRENO=1#provisario
  if(merge_data$TIPO.DE.TERRENO[i]=="terreno interno") merge_data$FATOR.TIPO.DE.TERRENO=0.7
  if(merge_data$TESTADA.PARA.CALCULO[i]<=10)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.7071
  if(merge_data$TESTADA.PARA.CALCULO[i]==11)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.7416
  if(merge_data$TESTADA.PARA.CALCULO[i]==12)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.7746
  if(merge_data$TESTADA.PARA.CALCULO[i]==13)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.8062
  if(merge_data$TESTADA.PARA.CALCULO[i]==14)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.8367
  if(merge_data$TESTADA.PARA.CALCULO[i]==15)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.866
  if(merge_data$TESTADA.PARA.CALCULO[i]==16)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.8944
  if(merge_data$TESTADA.PARA.CALCULO[i]==17)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.922
  if(merge_data$TESTADA.PARA.CALCULO[i]==18)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.9487
  if(merge_data$TESTADA.PARA.CALCULO[i]==19)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.9747
  if(merge_data$TESTADA.PARA.CALCULO[i]>=20 && merge_data$TESTADA.PARA.CALCULO[i]<=40)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=1
  if(merge_data$TESTADA.PARA.CALCULO[i]==41)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.9877
  if(merge_data$TESTADA.PARA.CALCULO[i]==42)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.9759
  if(merge_data$TESTADA.PARA.CALCULO[i]==43)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.9645
  if(merge_data$TESTADA.PARA.CALCULO[i]==44)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.9535
  if(merge_data$TESTADA.PARA.CALCULO[i]==45)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.9428
  if(merge_data$TESTADA.PARA.CALCULO[i]==46)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.9325
  if(merge_data$TESTADA.PARA.CALCULO[i]==47)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.9225
  if(merge_data$TESTADA.PARA.CALCULO[i]==48)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.9129
  if(merge_data$TESTADA.PARA.CALCULO[i]==49)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.9035
  if(merge_data$TESTADA.PARA.CALCULO[i]==50)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.8944
  if(merge_data$TESTADA.PARA.CALCULO[i]==51)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.8856
  if(merge_data$TESTADA.PARA.CALCULO[i]==52)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.8771
  if(merge_data$TESTADA.PARA.CALCULO[i]==53)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.8687
  if(merge_data$TESTADA.PARA.CALCULO[i]==54)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.8607
  if(merge_data$TESTADA.PARA.CALCULO[i]==55)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.8528
  if(merge_data$TESTADA.PARA.CALCULO[i]==56)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.8452
  if(merge_data$TESTADA.PARA.CALCULO[i]==57)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.8377
  if(merge_data$TESTADA.PARA.CALCULO[i]==58)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.8305
  if(merge_data$TESTADA.PARA.CALCULO[i]==59)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.8234
  if(merge_data$TESTADA.PARA.CALCULO[i]==60)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.8165
  if(merge_data$TESTADA.PARA.CALCULO[i]==61)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.8098
  if(merge_data$TESTADA.PARA.CALCULO[i]==62)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.8032
  if(merge_data$TESTADA.PARA.CALCULO[i]==63)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.7968
  if(merge_data$TESTADA.PARA.CALCULO[i]==64)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.7906
  if(merge_data$TESTADA.PARA.CALCULO[i]==65)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.7845
  if(merge_data$TESTADA.PARA.CALCULO[i]==66)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.7785
  if(merge_data$TESTADA.PARA.CALCULO[i]==67)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.7727
  if(merge_data$TESTADA.PARA.CALCULO[i]==68)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.767
  if(merge_data$TESTADA.PARA.CALCULO[i]==69)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.7614
  if(merge_data$TESTADA.PARA.CALCULO[i]==70)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.7559
  if(merge_data$TESTADA.PARA.CALCULO[i]==71)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.7506
  if(merge_data$TESTADA.PARA.CALCULO[i]==72)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.7454
  if(merge_data$TESTADA.PARA.CALCULO[i]==73)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.7402
  if(merge_data$TESTADA.PARA.CALCULO[i]==74)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.7352
  if(merge_data$TESTADA.PARA.CALCULO[i]==75)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.7303
  if(merge_data$TESTADA.PARA.CALCULO[i]==76)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.7255
  if(merge_data$TESTADA.PARA.CALCULO[i]==77)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.7207
  if(merge_data$TESTADA.PARA.CALCULO[i]==78)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.7161
  if(merge_data$TESTADA.PARA.CALCULO[i]==79)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.7116
  if(merge_data$TESTADA.PARA.CALCULO[i]==80)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.7071
  if(merge_data$TESTADA.PARA.CALCULO[i]==81 ||merge_data$TESTADA.PARA.CALCULO[i]==82)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.6984
  if(merge_data$TESTADA.PARA.CALCULO[i]==83||merge_data$TESTADA.PARA.CALCULO[i]==84)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.6901
  if(merge_data$TESTADA.PARA.CALCULO[i]==85||merge_data$TESTADA.PARA.CALCULO[i]==86)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.682
  if(merge_data$TESTADA.PARA.CALCULO[i]==87||merge_data$TESTADA.PARA.CALCULO[i]==88)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.6742
  if(merge_data$TESTADA.PARA.CALCULO[i]==89||merge_data$TESTADA.PARA.CALCULO[i]==90)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.6667
  if(merge_data$TESTADA.PARA.CALCULO[i]==91||merge_data$TESTADA.PARA.CALCULO[i]==92)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.6594
  if(merge_data$TESTADA.PARA.CALCULO[i]==93||merge_data$TESTADA.PARA.CALCULO[i]==94)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.6523
  if(merge_data$TESTADA.PARA.CALCULO[i]==95||merge_data$TESTADA.PARA.CALCULO[i]==96)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.6455
  if(merge_data$TESTADA.PARA.CALCULO[i]==97||merge_data$TESTADA.PARA.CALCULO[i]==98)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.6389
  if(merge_data$TESTADA.PARA.CALCULO[i]==99||merge_data$TESTADA.PARA.CALCULO[i]==100)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.6325
  if(merge_data$TESTADA.PARA.CALCULO[i]>=101 && merge_data$TESTADA.PARA.CALCULO[i]<=105)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.6172
  if(merge_data$TESTADA.PARA.CALCULO[i]>=106 && merge_data$TESTADA.PARA.CALCULO[i]<=110)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.603
  if(merge_data$TESTADA.PARA.CALCULO[i]>=111 && merge_data$TESTADA.PARA.CALCULO[i]<=115)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.5898
  if(merge_data$TESTADA.PARA.CALCULO[i]>=116 && merge_data$TESTADA.PARA.CALCULO[i]<=120)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.5774
  if(merge_data$TESTADA.PARA.CALCULO[i]>=121 && merge_data$TESTADA.PARA.CALCULO[i]<=125)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.5657
  if(merge_data$TESTADA.PARA.CALCULO[i]>=126 && merge_data$TESTADA.PARA.CALCULO[i]<=130)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.5547
  if(merge_data$TESTADA.PARA.CALCULO[i]>=131 && merge_data$TESTADA.PARA.CALCULO[i]<=135)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.5443
  if(merge_data$TESTADA.PARA.CALCULO[i]>=136 && merge_data$TESTADA.PARA.CALCULO[i]<=140)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.5345
  if(merge_data$TESTADA.PARA.CALCULO[i]>=141 && merge_data$TESTADA.PARA.CALCULO[i]<=145)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.5252
  if(merge_data$TESTADA.PARA.CALCULO[i]>=146 && merge_data$TESTADA.PARA.CALCULO[i]<=150)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.5164
  if(merge_data$TESTADA.PARA.CALCULO[i]>=151 && merge_data$TESTADA.PARA.CALCULO[i]<=160)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.5
  if(merge_data$TESTADA.PARA.CALCULO[i]>=161 && merge_data$TESTADA.PARA.CALCULO[i]<=170)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.4851
  if(merge_data$TESTADA.PARA.CALCULO[i]>=171 && merge_data$TESTADA.PARA.CALCULO[i]<=180)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.4714
  if(merge_data$TESTADA.PARA.CALCULO[i]>=181 && merge_data$TESTADA.PARA.CALCULO[i]<=190)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.4588
  if(merge_data$TESTADA.PARA.CALCULO[i]>=191 && merge_data$TESTADA.PARA.CALCULO[i]<=200)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.4472
  if(merge_data$TESTADA.PARA.CALCULO[i]>=201)merge_data$FATOR.TIPO.DE.PROFUNDIDADE=0.4472
  if(merge_data$NUMERO.DO.CONDOMINIO[i]=="00-0")merge_data$FATOR.TIPO.DE.CONDOMINIO=1
  if(merge_data$NUMERO.DO.CONDOMINIO[i]!="00-0")merge_data$FATOR.TIPO.DE.CONDOMINIO=1.6
}


merge_data$vv_terreno="NA"
merge_data$vv_terreno=merge_data$AREA.DO.TERRENO*merge_data$VALOR.DO.M2.DO.TERRENO*merge_data$FRACAO.IDEAL*
  merge_data$FATOR.TIPO.DE.PROFUNDIDADE*merge_data$FATOR.TIPO.DE.TERRENO*merge_data$FATOR.TIPO.DE.CONDOMINIO

#merge_data$vv_terreno=merge_data$vv_terreno/1.6

merge_data$vv="NA"
merge_data$vv=merge_data$vv_terreno+merge_data$vv_construcao

#Cria variável binária para período pós tratamento
merge_data$Binaria_pos_trat=0
merge_data$Binaria_pos_trat[merge_data$ANO.DO.EXERCICIO>2001]=1

#Calcula tier de imposto
merge_data$tier_iptu_pre_trat=0.01
merge_data$valor_iptu_pre_trat=merge_data$tier_iptu_pre_trat*merge_data$vv
plot(log(merge_data$valor_iptu_pre_trat))
merge_data$tier_iptu_pos_trat=0.01
merge_data$tier_iptu_pos_trat[merge_data$vv<=50000]=-0.002
merge_data$tier_iptu_pos_trat[merge_data$vv[merge_data$vv<=100000]>50000]=0
merge_data$tier_iptu_pos_trat[merge_data$vv[merge_data$vv<=200000]>100000]=0.002
merge_data$tier_iptu_pos_trat[merge_data$vv[merge_data$vv<=400000]>200000]=0.004
merge_data$tier_iptu_pos_trat[merge_data$vv>400000]=0.006
merge_data$valor_iptu_pos_trat=merge_data$valor_iptu_pre_trat

merge_data$vv_tier50k=0
merge_data$vv_tier100k=0
merge_data$vv_tier200k=0
merge_data$vv_tier400k=0
merge_data$vv_tier401k=0

for (j in 1:length(merge_data$tier_iptu_pos_trat)){
  merge_data$vv_tier50k[j]=min(merge_data$vv[j],50000)
  merge_data$vv_tier100k[j]=min(merge_data$vv[j]-50000,50000)
  merge_data$vv_tier200k[j]=min(merge_data$vv[j]-100000,100000)
  merge_data$vv_tier400k[j]=min(merge_data$vv[j]-200000,200000)
  merge_data$vv_tier401k[j]=merge_data$vv[j]-400000
}
merge_data$vv_tier50k[merge_data$vv_tier50k<0]=0
merge_data$vv_tier100k[merge_data$vv_tier100k<0]=0
merge_data$vv_tier200k[merge_data$vv_tier200k<0]=0
merge_data$vv_tier400k[merge_data$vv_tier400k<0]=0
merge_data$vv_tier401k[merge_data$vv_tier401k<0]=0

merge_data$valor_iptu_pos_trat=merge_data$valor_iptu_pos_trat+(merge_data$vv_tier50k*(-0.002)+merge_data$vv_tier100k*0+merge_data$vv_tier200k*0.002+
                                                                 merge_data$vv_tier400k*0.004+merge_data$vv_tier401k*0.006)

hist(merge_data$valor_iptu_pos_trat-merge_data$valor_iptu_pre_trat,xlim = c(-20000,100000),breaks = 100,main = "Diferença entre valor iptu pré e pós tratamento")

plot(as.factor(merge_data$tier_iptu_pos_trat),main="Frequência de observações em cada tier do IPTU após 2001")



save.image("ws.RData")
