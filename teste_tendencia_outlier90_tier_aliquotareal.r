#########################################################################
#######considerando tier de aliquota real observações da base############
#########################################################################
#construindo tier  da aliquota real
#load("~/ws_final.RData")
#merge_data_final=merge_data_final[merge_data_final$PC_TT_UN>quantile(merge_data_final$PC_TT_UN,probs = seq(0, 1, 0.10))[2],]
#merge_data_final=merge_data_final[merge_data_final$PC_TT_UN<=quantile(merge_data_final$PC_TT_UN,probs = seq(0, 1, 0.1))[10],]

merge_data_final=base_final

names(merge_data_final)=gsub("merge_data_final.",names(merge_data_final),replacement = "")

#for (i in 1:nrow(merge_data_final)){
#  for (k in 1:nrow(ipca)){
#    if(ipca$YEAR[k]==merge_data_final$ANO.DO.EXERCICIO[i])merge_data_final$PC_TT_UN[i]=merge_data_final$PC_TT_UN[i]*ipca[k,4]
#  }
#}



vetor_percentis=quantile(merge_data_final$aliquota_real, probs = seq(0,1,0.33))
vetor_cores=c("red","green","yellow", "blue", "darkgoldenrod4", "gray", "orange", "cyan", "chocolate","black")
for (i in 2:length(vetor_percentis)){
  tiert0=vetor_percentis[i-1]
  tiert1=vetor_percentis[i]
  
  merge_data_final_temp=merge_data_final[merge_data_final$aliquota_real>=tiert0,]
  merge_data_final_temp=merge_data_final_temp[merge_data_final_temp$aliquota_real<tiert1,]
  
  
  preco_lan_medio_1995=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1995]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1995])))
  preco_lan_medio_1996=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1996]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1996])))
  preco_lan_medio_1997=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1997]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1997])))
  preco_lan_medio_1998=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1998]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1998])))
  preco_lan_medio_1999=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1999]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1999])))
  preco_lan_medio_2000=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2000]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2000])))
  preco_lan_medio_2001=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2001]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2001])))
  preco_lan_medio_2002=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2002]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2002])))
  preco_lan_medio_2003=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2003]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2003])))
  preco_lan_medio_2004=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2004]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2004])))
  preco_lan_medio_2005=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2005]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2005])))
  preco_lan_medio_2006=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2006]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2006])))
  preco_lan_medio_2007=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2007]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2007])))
  preco_lan_medio_2008=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2008]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2008])))
  
  
  vetor_precos_lan_tempo=c(preco_lan_medio_1995,preco_lan_medio_1996,preco_lan_medio_1997,preco_lan_medio_1998,preco_lan_medio_1998,
                           preco_lan_medio_2000,preco_lan_medio_2001,preco_lan_medio_2002,
                           preco_lan_medio_2003,preco_lan_medio_2004,preco_lan_medio_2005,preco_lan_medio_2006,
                           preco_lan_medio_2007,preco_lan_medio_2008)
  anos=c(1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)
  
  ###Sem corrigir inflação
  if (i==2){ 
    plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Média geométrica valor de lançamento por ano",ylim = c(60000,260000))
    abline(v=2001,col="red")
  }
  lines(y=vetor_precos_lan_tempo,x=anos,col=vetor_cores[i-1])
  #legend(1997,1900000,legend = c("x: Limite superior","+: Limite inferior"))
  
  
}

legend(1996,230000,legend = format(round(vetor_percentis[1:length(vetor_percentis)-1], 2), nsmall = 2),col = vetor_cores,cex = .7,fill  = vetor_cores[1:length(vetor_percentis)-1])


#2x2
#load("~/ws_final.RData")
#merge_data_final=merge_data_final[merge_data_final$PC_TT_UN>quantile(merge_data_final$PC_TT_UN,probs = seq(0, 1, 0.10))[2],]
#merge_data_final=merge_data_final[merge_data_final$PC_TT_UN<=quantile(merge_data_final$PC_TT_UN,probs = seq(0, 1, 0.10))[10],]

vetor_percentis=quantile(merge_data_final$aliquota_real, probs = seq(0,1,0.33))
vetor_cores=c("red","green","yellow", "blue", "pink", "gray", "orange", "cyan", "chocolate","black")
for (i in 2:length(vetor_percentis)){
  tiert0=vetor_percentis[i-1]
  tiert1=vetor_percentis[i]
  
  merge_data_final_temp=merge_data_final[merge_data_final$aliquota_real>=tiert0,]
  merge_data_final_temp=merge_data_final_temp[merge_data_final_temp$aliquota_real<tiert1,]
  
  
  preco_lan_medio_2000=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2000]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2000])))
  preco_lan_medio_2001=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2001]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2001])))
  preco_lan_medio_2002=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2002]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2002])))

  
  vetor_precos_lan_tempo=c(
    log(preco_lan_medio_2000),log(preco_lan_medio_2001),log(preco_lan_medio_2002))
  anos=c(2000,2001,2002)
  
  ###Sem corrigir inflação
  if (i==2){ 
    plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Log da média geométrica valor de lançamento por ano",ylim = c(11.0,12.1))
    abline(v=2001,col="red")
  }
  lines(y=vetor_precos_lan_tempo,x=anos,col=vetor_cores[i-1])
  #legend(1997,1900000,legend = c("x: Limite superior","+: Limite inferior"))
  
  
}
legend(2001.5,11.4,legend = format(round(vetor_percentis[1:length(vetor_percentis)-1], 2), nsmall = 2),col = vetor_cores,cex = .7,fill  = vetor_cores[1:length(vetor_percentis)-1])



#ln()
#load("~/ws_final.RData")

#merge_data_final=merge_data_final[merge_data_final$PC_TT_UN<=quantile(merge_data_final$PC_TT_UN,probs = seq(0, 1, 0.10))[10],]

vetor_percentis=quantile(merge_data_final$aliquota_real, probs = seq(0,1,0.33))
vetor_cores=c("red","green","yellow", "blue", "darkgoldenrod4", "gray", "orange", "cyan", "chocolate","black","red","green","yellow", "blue", "pink", "gray", "orange", "cyan", "chocolate","black")


for (i in 2:length(vetor_percentis)){
  tiert0=vetor_percentis[i-1]
  tiert1=vetor_percentis[i]
  merge_data_final_temp=merge_data_final[merge_data_final$aliquota_real>=tiert0,]
  merge_data_final_temp=merge_data_final_temp[merge_data_final_temp$aliquota_real<tiert1,]
  
  
  preco_lan_medio_1995=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1995]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1995])))
  preco_lan_medio_1996=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1996]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1996])))
  preco_lan_medio_1997=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1997]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1997])))
  preco_lan_medio_1998=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1998]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1998])))
  preco_lan_medio_1999=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1999]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1999])))
  preco_lan_medio_2000=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2000]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2000])))
  preco_lan_medio_2001=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2001]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2001])))
  preco_lan_medio_2002=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2002]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2002])))
  preco_lan_medio_2003=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2003]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2003])))
  preco_lan_medio_2004=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2004]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2004])))
  preco_lan_medio_2005=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2005]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2005])))
  preco_lan_medio_2006=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2006]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2006])))
  preco_lan_medio_2007=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2007]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2007])))
  preco_lan_medio_2008=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2008]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2008])))
  
  
  vetor_precos_lan_tempo=c(log(preco_lan_medio_1995),log(preco_lan_medio_1996),log(preco_lan_medio_1997),log(preco_lan_medio_1998),log(preco_lan_medio_1999),
                           log(preco_lan_medio_2000),log(preco_lan_medio_2001),
                           log(preco_lan_medio_2002),log(preco_lan_medio_2003),log(preco_lan_medio_2004),
                           log(preco_lan_medio_2005),log(preco_lan_medio_2006),log(preco_lan_medio_2007),
                           log(preco_lan_medio_2008))
  anos=c(1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)
  
  ###Sem corrigir inflação
  if (i==2){ 
    plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Log da média geométrica valor de lançamento por ano e percentil (limite inferior)",ylim = c(11.30,12.5))
    abline(v=2001,col="red")
  }
  lines(y=vetor_precos_lan_tempo,x=anos,col=vetor_cores[i-1])
  #legend(1997,1900000,legend = c("x: Limite superior","+: Limite inferior"))
  
  
}
legend(1996,11,legend = format(round(vetor_percentis[1:length(vetor_percentis)-1], 2), nsmall = 2),col = vetor_cores,cex = .7,fill  = vetor_cores[1:length(vetor_percentis)-1])
