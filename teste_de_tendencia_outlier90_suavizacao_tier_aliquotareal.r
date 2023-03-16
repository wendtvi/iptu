#########################################################################
#######considerando tier de aliquota real observações da base############
#########################################################################
#construindo tier  da aliquota real
#load("~/ws_final.RData")
#merge_data_final=merge_data_final[merge_data_final$PC_TT_UN>quantile(merge_data_final$PC_TT_UN,probs = seq(0, 1, 0.10))[2],]
#merge_data_final=merge_data_final[merge_data_final$PC_TT_UN<=quantile(merge_data_final$PC_TT_UN,probs = seq(0, 1, 0.10))[10],]

#vetor_percentis=quantile(merge_data_final$aliquota_real, probs = seq(0,1,0.1))
vetor_percentis=c(0,0.95,1,1.05,2)
vetor_cores=c("red","green","blue", "blue", "pink", "gray", "orange", "cyan", "chocolate","black")
for (i in 2:length(vetor_percentis)){
  tiert0_inf=vetor_percentis[1]
  tiert1_inf=vetor_percentis[i]
  
  merge_data_final_temp=merge_data_final[merge_data_final$aliquota_real>=tiert0_inf,]
  merge_data_final_temp=merge_data_final_temp[merge_data_final_temp$aliquota_real<=tiert1_inf,]
  
  
  
  inf_preco_lan_medio_1995=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1995]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1995])))
  inf_preco_lan_medio_1996=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1996]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1996])))
  inf_preco_lan_medio_1997=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1997]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1997])))
  inf_preco_lan_medio_1998=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1998]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1998])))
  inf_preco_lan_medio_1999=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1999]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1999])))
  inf_preco_lan_medio_2000=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2000]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2000])))
  inf_preco_lan_medio_2001=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2001]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2001])))
  inf_preco_lan_medio_2002=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2002]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2002])))
  inf_preco_lan_medio_2003=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2003]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2003])))
  inf_preco_lan_medio_2004=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2004]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2004])))
  inf_preco_lan_medio_2005=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2005]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2005])))
  inf_preco_lan_medio_2006=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2006]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2006])))
  inf_preco_lan_medio_2007=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2007]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2007])))
  inf_preco_lan_medio_2008=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2008]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2008])))
  
  
  tiert0_sup=vetor_percentis[length(vetor_percentis)-i+1]
  tiert1_sup=vetor_percentis[length(vetor_percentis)]
  
  merge_data_final_temp=merge_data_final[merge_data_final$aliquota_real>=tiert0_sup,]
  merge_data_final_temp=merge_data_final_temp[merge_data_final_temp$aliquota_real<=tiert1_sup,]
  
  
  sup_preco_lan_medio_1995=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1995]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1995])))
  sup_preco_lan_medio_1996=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1996]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1996])))
  sup_preco_lan_medio_1997=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1997]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1997])))
  sup_preco_lan_medio_1998=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1998]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1998])))
  sup_preco_lan_medio_1999=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1999]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1999])))
  sup_preco_lan_medio_2000=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2000]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2000])))
  sup_preco_lan_medio_2001=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2001]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2001])))
  sup_preco_lan_medio_2002=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2002]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2002])))
  sup_preco_lan_medio_2003=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2003]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2003])))
  sup_preco_lan_medio_2004=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2004]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2004])))
  sup_preco_lan_medio_2005=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2005]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2005])))
  sup_preco_lan_medio_2006=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2006]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2006])))
  sup_preco_lan_medio_2007=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2007]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2007])))
  sup_preco_lan_medio_2008=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2008]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2008])))
  
  
  inf_vetor_precos_lan_tempo=c(inf_preco_lan_medio_1995,inf_preco_lan_medio_1996,inf_preco_lan_medio_1997,inf_preco_lan_medio_1998,inf_preco_lan_medio_1998,
                               inf_preco_lan_medio_2000,inf_preco_lan_medio_2001,inf_preco_lan_medio_2002,
                               inf_preco_lan_medio_2003,inf_preco_lan_medio_2004,inf_preco_lan_medio_2005,inf_preco_lan_medio_2006,
                               inf_preco_lan_medio_2007,inf_preco_lan_medio_2008)
  sup_vetor_precos_lan_tempo=c(sup_preco_lan_medio_1995,sup_preco_lan_medio_1996,sup_preco_lan_medio_1997,sup_preco_lan_medio_1998,sup_preco_lan_medio_1998,
                               sup_preco_lan_medio_2000,sup_preco_lan_medio_2001,sup_preco_lan_medio_2002,
                               sup_preco_lan_medio_2003,sup_preco_lan_medio_2004,sup_preco_lan_medio_2005,sup_preco_lan_medio_2006,
                               sup_preco_lan_medio_2007,sup_preco_lan_medio_2008)
  anos=c(1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)
  
  ###Sem corrigir inflação
  plot(y=inf_vetor_precos_lan_tempo,x=anos,pch=21,main = "Média geométrica valor de lançamento por ano",ylim = c(100000,300000),type = 'l')
  abline(v=2001,col="red")
  lines(y=inf_vetor_precos_lan_tempo,x=anos,col="red")
  lines(y=sup_vetor_precos_lan_tempo,x=anos,col="blue")
  legend(2004,120000,legend = c(paste("<",format(round(tiert1_inf, 2), nsmall = 2)),paste(">",format(round(tiert0_sup, 2), nsmall = 2))),col = c("red","blue"),cex = .7,fill  = c("red","blue"))
  
}


#2x2
#load("~/ws_final.RData")
#merge_data_final=merge_data_final[merge_data_final$PC_TT_UN>quantile(merge_data_final$PC_TT_UN,probs = seq(0, 1, 0.10))[2],]
#merge_data_final=merge_data_final[merge_data_final$PC_TT_UN<=quantile(merge_data_final$PC_TT_UN,probs = seq(0, 1, 0.10))[10],]

#vetor_percentis=quantile(merge_data_final$aliquota_real, probs = seq(0,1,0.1))
vetor_cores=c("red","green","blue", "blue", "pink", "gray", "orange", "cyan", "chocolate","black")
for (i in 2:length(vetor_percentis)){
  tiert0_inf=vetor_percentis[1]
  tiert1_inf=vetor_percentis[i]
  
  merge_data_final_temp=merge_data_final[merge_data_final$aliquota_real>=tiert0_inf,]
  merge_data_final_temp=merge_data_final_temp[merge_data_final_temp$aliquota_real<tiert1_inf,]
  
  
  
  inf_preco_lan_medio_1995=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1995]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1995])))
  inf_preco_lan_medio_1996=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1996]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1996])))
  inf_preco_lan_medio_1997=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1997]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1997])))
  inf_preco_lan_medio_1998=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1998]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1998])))
  inf_preco_lan_medio_1999=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1999]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1999])))
  inf_preco_lan_medio_2000=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2000]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2000])))
  inf_preco_lan_medio_2001=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2001]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2001])))
  inf_preco_lan_medio_2002=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2002]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2002])))
  inf_preco_lan_medio_2003=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2003]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2003])))
  inf_preco_lan_medio_2004=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2004]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2004])))
  inf_preco_lan_medio_2005=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2005]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2005])))
  inf_preco_lan_medio_2006=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2006]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2006])))
  inf_preco_lan_medio_2007=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2007]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2007])))
  inf_preco_lan_medio_2008=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2008]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2008])))
  
  
  tiert0_sup=vetor_percentis[length(vetor_percentis)-i+1]
  tiert1_sup=vetor_percentis[length(vetor_percentis)]
  
  merge_data_final_temp=merge_data_final[merge_data_final$aliquota_real>=tiert0_sup,]
  merge_data_final_temp=merge_data_final_temp[merge_data_final_temp$aliquota_real<tiert1_sup,]
  
  
  sup_preco_lan_medio_1995=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1995]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1995])))
  sup_preco_lan_medio_1996=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1996]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1996])))
  sup_preco_lan_medio_1997=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1997]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1997])))
  sup_preco_lan_medio_1998=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1998]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1998])))
  sup_preco_lan_medio_1999=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1999]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1999])))
  sup_preco_lan_medio_2000=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2000]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2000])))
  sup_preco_lan_medio_2001=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2001]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2001])))
  sup_preco_lan_medio_2002=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2002]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2002])))
  sup_preco_lan_medio_2003=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2003]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2003])))
  sup_preco_lan_medio_2004=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2004]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2004])))
  sup_preco_lan_medio_2005=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2005]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2005])))
  sup_preco_lan_medio_2006=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2006]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2006])))
  sup_preco_lan_medio_2007=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2007]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2007])))
  sup_preco_lan_medio_2008=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2008]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2008])))
  
  
  inf_vetor_precos_lan_tempo=c(inf_preco_lan_medio_2000,inf_preco_lan_medio_2001,inf_preco_lan_medio_2002)
  sup_vetor_precos_lan_tempo=c(sup_preco_lan_medio_2000,sup_preco_lan_medio_2001,sup_preco_lan_medio_2002)
  anos=c(2000,2001,2002)
  
  ###Sem corrigir inflação
  plot(y=inf_vetor_precos_lan_tempo,x=anos,pch=21,main = "Média geométrica valor de lançamento por ano",ylim = c(100000,210000),type = 'l')
  abline(v=2001,col="red")
  lines(y=inf_vetor_precos_lan_tempo,x=anos,col="red")
  lines(y=sup_vetor_precos_lan_tempo,x=anos,col="blue")
  legend(2001.5,120000,legend = c(paste("<",format(round(tiert1_inf, 2), nsmall = 2)),paste(">",format(round(tiert0_sup, 2), nsmall = 2))),col = c("red","blue"),cex = .7,fill  = c("red","blue"))
  
  
}


#ln()

#load("~/ws_final.RData")
#merge_data_final=merge_data_final[merge_data_final$PC_TT_UN>quantile(merge_data_final$PC_TT_UN,probs = seq(0, 1, 0.10))[2],]
#merge_data_final=merge_data_final[merge_data_final$PC_TT_UN<=quantile(merge_data_final$PC_TT_UN,probs = seq(0, 1, 0.10))[10],]

#vetor_percentis=quantile(merge_data_final$aliquota_real, probs = seq(0,1,0.1))
vetor_cores=c("red","green","blue", "blue", "pink", "gray", "orange", "cyan", "chocolate","black")
for (i in 2:length(vetor_percentis)){
  tiert0_inf=vetor_percentis[1]
  tiert1_inf=vetor_percentis[i]
  
  merge_data_final_temp=merge_data_final[merge_data_final$aliquota_real>=tiert0_inf,]
  merge_data_final_temp=merge_data_final_temp[merge_data_final_temp$aliquota_real<tiert1_inf,]
  
  
  
  inf_preco_lan_medio_1995=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1995]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1995])))
  inf_preco_lan_medio_1996=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1996]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1996])))
  inf_preco_lan_medio_1997=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1997]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1997])))
  inf_preco_lan_medio_1998=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1998]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1998])))
  inf_preco_lan_medio_1999=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1999]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1999])))
  inf_preco_lan_medio_2000=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2000]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2000])))
  inf_preco_lan_medio_2001=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2001]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2001])))
  inf_preco_lan_medio_2002=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2002]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2002])))
  inf_preco_lan_medio_2003=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2003]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2003])))
  inf_preco_lan_medio_2004=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2004]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2004])))
  inf_preco_lan_medio_2005=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2005]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2005])))
  inf_preco_lan_medio_2006=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2006]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2006])))
  inf_preco_lan_medio_2007=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2007]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2007])))
  inf_preco_lan_medio_2008=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2008]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2008])))
  
  
  tiert0_sup=vetor_percentis[length(vetor_percentis)-i+1]
  tiert1_sup=vetor_percentis[length(vetor_percentis)]
  
  merge_data_final_temp=merge_data_final[merge_data_final$aliquota_real>=tiert0_sup,]
  merge_data_final_temp=merge_data_final_temp[merge_data_final_temp$aliquota_real<tiert1_sup,]
  
  
  sup_preco_lan_medio_1995=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1995]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1995])))
  sup_preco_lan_medio_1996=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1996]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1996])))
  sup_preco_lan_medio_1997=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1997]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1997])))
  sup_preco_lan_medio_1998=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1998]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1998])))
  sup_preco_lan_medio_1999=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1999]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==1999])))
  sup_preco_lan_medio_2000=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2000]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2000])))
  sup_preco_lan_medio_2001=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2001]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2001])))
  sup_preco_lan_medio_2002=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2002]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2002])))
  sup_preco_lan_medio_2003=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2003]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2003])))
  sup_preco_lan_medio_2004=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2004]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2004])))
  sup_preco_lan_medio_2005=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2005]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2005])))
  sup_preco_lan_medio_2006=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2006]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2006])))
  sup_preco_lan_medio_2007=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2007]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2007])))
  sup_preco_lan_medio_2008=prod(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2008]^(1/length(merge_data_final_temp$PC_TT_UN[merge_data_final_temp$ANO.DO.EXERCICIO==2008])))
  
  
  inf_vetor_precos_lan_tempo=c(log(inf_preco_lan_medio_1995),log(inf_preco_lan_medio_1996),log(inf_preco_lan_medio_1997),log(inf_preco_lan_medio_1998),log(inf_preco_lan_medio_1998),
                               log(inf_preco_lan_medio_2000),log(inf_preco_lan_medio_2001),log(inf_preco_lan_medio_2002),
                               log(inf_preco_lan_medio_2003),log(inf_preco_lan_medio_2004),log(inf_preco_lan_medio_2005),log(inf_preco_lan_medio_2006),
                               log(inf_preco_lan_medio_2007),log(inf_preco_lan_medio_2008))
  sup_vetor_precos_lan_tempo=c(log(sup_preco_lan_medio_1995),log(sup_preco_lan_medio_1996),log(sup_preco_lan_medio_1997),log(sup_preco_lan_medio_1998),log(sup_preco_lan_medio_1998),
                               log(sup_preco_lan_medio_2000),log(sup_preco_lan_medio_2001),log(sup_preco_lan_medio_2002),
                               log(sup_preco_lan_medio_2003),log(sup_preco_lan_medio_2004),log(sup_preco_lan_medio_2005),log(sup_preco_lan_medio_2006),
                               log(sup_preco_lan_medio_2007),log(sup_preco_lan_medio_2008))
  anos=c(1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)
  
  ###Sem corrigir inflação
  plot(y=inf_vetor_precos_lan_tempo,x=anos,pch=21,main = "Log da média geométrica valor de lançamento por ano",ylim = c(11,13),col="white")
  abline(v=2001,col="red")
  lines(y=inf_vetor_precos_lan_tempo,x=anos,col="red")
  lines(y=sup_vetor_precos_lan_tempo,x=anos,col="blue")
  legend(2004,11.5,legend = c(paste("<",format(round(tiert1_inf, 2), nsmall = 2)),paste(">",format(round(tiert0_sup, 2), nsmall = 2))),col = c("red","blue"),cex = 1,fill  = c("red","blue"))
  
}

