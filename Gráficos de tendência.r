######################################################################################
#######considerando tier de aliquota real para agrupar observações da base############
######################################################################################
#construindo tier  da aliquota real
merge_data_final=read.csv2("~/base_final.csv")

#vetor_percentis=quantile(merge_data_final$aliquota_real, probs = seq(0,1,0.33))
#Define limites para agrupamentos das observações por alíquota real
vetor_percentis=c(0,0.9,1.10,max(merge_data_final$Aliquota.Real.Pos.Tratamento))
vetor_cores=c("red","black","dimgray", "blue", "darkgoldenrod4", "gray", "orange", "cyan", "chocolate","black")

#GRAFICO 1: Média geométrica valor de lançamento por ano
for (i in 2:length(vetor_percentis)){
  tiert0=vetor_percentis[i-1]
  tiert1=vetor_percentis[i]
  
  merge_data_final_temp=merge_data_final[merge_data_final$Aliquota.Real.Pos.Tratamento>=tiert0,]
  merge_data_final_temp=merge_data_final_temp[merge_data_final_temp$Aliquota.Real.Pos.Tratamento<tiert1,]
  
  
  preco_lan_medio_1995=prod(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==1995]^(1/length(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==1995])))
  preco_lan_medio_1996=prod(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==1996]^(1/length(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==1996])))
  preco_lan_medio_1997=prod(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==1997]^(1/length(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==1997])))
  preco_lan_medio_1998=prod(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==1998]^(1/length(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==1998])))
  preco_lan_medio_1999=prod(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==1999]^(1/length(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==1999])))
  preco_lan_medio_2000=prod(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2000]^(1/length(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2000])))
  preco_lan_medio_2001=prod(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2001]^(1/length(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2001])))
  preco_lan_medio_2002=prod(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2002]^(1/length(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2002])))
  preco_lan_medio_2003=prod(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2003]^(1/length(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2003])))
  preco_lan_medio_2004=prod(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2004]^(1/length(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2004])))
  preco_lan_medio_2005=prod(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2005]^(1/length(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2005])))
  preco_lan_medio_2006=prod(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2006]^(1/length(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2006])))
  preco_lan_medio_2007=prod(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2007]^(1/length(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2007])))
  preco_lan_medio_2008=prod(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2008]^(1/length(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2008])))
  
  
  vetor_precos_lan_tempo=c(preco_lan_medio_1995,preco_lan_medio_1996,preco_lan_medio_1997,preco_lan_medio_1998,preco_lan_medio_1998,
                           preco_lan_medio_2000,preco_lan_medio_2001,preco_lan_medio_2002,
                           preco_lan_medio_2003,preco_lan_medio_2004,preco_lan_medio_2005,preco_lan_medio_2006,
                           preco_lan_medio_2007,preco_lan_medio_2008)
  anos=c(1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)
  
  ###Sem corrigir inflação
  if (i==2){ 
    plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Média geométrica valor de lançamento por ano",ylim = c(60000,260000),type = 'l')
    abline(v=2001,col="red")
  }
  lines(y=vetor_precos_lan_tempo,x=anos,col=vetor_cores[i-1])
  #legend(1997,1900000,legend = c("x: Limite superior","+: Limite inferior"))
  
  
}

legend(1996,230000,legend = format(round(vetor_percentis[1:length(vetor_percentis)-1], 2), nsmall = 2),col = vetor_cores,cex = .7,fill  = vetor_cores[1:length(vetor_percentis)-1])

#GRAFICO 2: Log da média geométrica valor de lançamento por ano (2x2)

for (i in 2:length(vetor_percentis)){
  tiert0=vetor_percentis[i-1]
  tiert1=vetor_percentis[i]
  
  merge_data_final_temp=merge_data_final[merge_data_final$Aliquota.Real.Pos.Tratamento>=tiert0,]
  merge_data_final_temp=merge_data_final_temp[merge_data_final_temp$Aliquota.Real.Pos.Tratamento<tiert1,]
  
  
  preco_lan_medio_2000=prod(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2000]^(1/length(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2000])))
  preco_lan_medio_2001=prod(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2001]^(1/length(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2001])))
  preco_lan_medio_2002=prod(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2002]^(1/length(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2002])))
  
  
  vetor_precos_lan_tempo=c(
    log(preco_lan_medio_2000),log(preco_lan_medio_2001),log(preco_lan_medio_2002))
  anos=c(2000,2001,2002)
  
  ###Sem corrigir inflação
  if (i==2){ 
    plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Log da média geométrica valor de lançamento por ano",ylim = c(11.0,13),type = 'l')
    abline(v=2001,col="red")
  }
  lines(y=vetor_precos_lan_tempo,x=anos,col=vetor_cores[i-1])
  #legend(1997,1900000,legend = c("x: Limite superior","+: Limite inferior"))
  
  
}
legend(2001.5,11.4,legend = format(round(vetor_percentis[1:length(vetor_percentis)-1], 2), nsmall = 2),col = vetor_cores,cex = .7,fill  = vetor_cores[1:length(vetor_percentis)-1])




#GRAFICO 3: Log da média geométrica valor de lançamento por ano 
for (i in 2:length(vetor_percentis)){
  tiert0=vetor_percentis[i-1]
  tiert1=vetor_percentis[i]
  merge_data_final_temp=merge_data_final[merge_data_final$Aliquota.Real.Pos.Tratamento>=tiert0,]
  merge_data_final_temp=merge_data_final_temp[merge_data_final_temp$Aliquota.Real.Pos.Tratamento<tiert1,]
  
  
  preco_lan_medio_1995=prod(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==1995]^(1/length(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==1995])))
  preco_lan_medio_1996=prod(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==1996]^(1/length(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==1996])))
  preco_lan_medio_1997=prod(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==1997]^(1/length(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==1997])))
  preco_lan_medio_1998=prod(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==1998]^(1/length(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==1998])))
  preco_lan_medio_1999=prod(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==1999]^(1/length(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==1999])))
  preco_lan_medio_2000=prod(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2000]^(1/length(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2000])))
  preco_lan_medio_2001=prod(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2001]^(1/length(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2001])))
  preco_lan_medio_2002=prod(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2002]^(1/length(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2002])))
  preco_lan_medio_2003=prod(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2003]^(1/length(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2003])))
  preco_lan_medio_2004=prod(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2004]^(1/length(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2004])))
  preco_lan_medio_2005=prod(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2005]^(1/length(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2005])))
  preco_lan_medio_2006=prod(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2006]^(1/length(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2006])))
  preco_lan_medio_2007=prod(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2007]^(1/length(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2007])))
  preco_lan_medio_2008=prod(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2008]^(1/length(merge_data_final_temp$Preco.de.Lancamento.do.Imovel[merge_data_final_temp$Ano.de.Lancamento==2008])))
  
  
  vetor_precos_lan_tempo=c(log(preco_lan_medio_1995),log(preco_lan_medio_1996),log(preco_lan_medio_1997),log(preco_lan_medio_1998),log(preco_lan_medio_1999),
                           log(preco_lan_medio_2000),log(preco_lan_medio_2001),
                           log(preco_lan_medio_2002),log(preco_lan_medio_2003),log(preco_lan_medio_2004),
                           log(preco_lan_medio_2005),log(preco_lan_medio_2006),log(preco_lan_medio_2007),
                           log(preco_lan_medio_2008))
  anos=c(1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)
  
  ###Sem corrigir inflação
  if (i==2){ 
    plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Log da média geométrica do valor de lançamento por ano",ylim = c(11.30,13),type = 'l',ylab = "Preço de lançamento")
    abline(v=2001,col="red")
  }
  lines(y=vetor_precos_lan_tempo,x=anos,col=vetor_cores[i-1])
  #legend(1997,1900000,legend = c("x: Limite superior","+: Limite inferior"))
  
  
}
legend(1996,13,legend = format(round(vetor_percentis[2:length(vetor_percentis)], 2), nsmall = 2),col = vetor_cores,cex = .7,fill  = vetor_cores[1:length(vetor_percentis)-1])
