#install.packages("fixest")
library(fixest)
base_final=read.csv2("C:/Users/vitoria.wendt/Downloads/base_final (1).csv")

base_final[,length(base_final)+1]=0
for(k in 1:length(base_final$Indice.da.Observacao)){
  if(base_final$Aliquota.Real.Pos.Tratamento[k]>0.9){
    if(base_final$Aliquota.Real.Pos.Tratamento[k]<1.10){
      base_final[k,length(base_final)]=1
    }
  }
}
names(base_final)[ncol(base_final)]="Grupo_controle"

base_final[,length(base_final)+1]=1
base_final[base_final$Grupo_controle==1,length(base_final)]=0
names(base_final)[ncol(base_final)]="Grupo_tratado"

base_final[,length(base_final)+1]=as.numeric(as.numeric(base_final$Ano.de.Lancamento)<2002)
names(base_final)[ncol(base_final)]="Pre_tratamento"
base_final[,length(base_final)+1]=as.numeric(as.numeric(base_final$Ano.de.Lancamento)>=2002)
names(base_final)[ncol(base_final)]="Pos_tratamento"

table(base_final$Grupo_tratado)/length(base_final$Grupo_tratado)
table(base_final$Pos_tratamento)/length(base_final$Pos_tratamento)
table(base_final$Pos_tratamento*base_final$Grupo_tratado)/length(base_final$Pos_tratamento)


#######################################################
#################SEM COVARIAVEIS#######################
#######################################################
base_final_lm=base_final[as.numeric(base_final$Aliquota.Real.Pos.Tratamento)>0.9,]
#base_final_lm=base_final_lm[as.numeric(base_final_lm$ANO_LAN)>2000,]
#base_final_lm=base_final_lm[as.numeric(base_final_lm$ANO_LAN)!=2001,]
did=feols(log(Preco.de.Lancamento.do.Imovel) ~(Grupo_tratado):i(Ano.de.Lancamento,2001)|Grupo_tratado+Ano.de.Lancamento+
            Numero.de.Elevadores.do.Empreendimento+Subprefeitura+,
           cluster=c("Ano.de.Lancamento"),data = base_final_lm)
summary(did)
iplot(did)

