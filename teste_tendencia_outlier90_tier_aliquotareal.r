#########################################################################
#######considerando tier de aliquota real observações da base############
#########################################################################
#construindo tier  da aliquota real
load("~/ws_final.RData")
#merge_data_final=merge_data_final[merge_data_final$PC_TT_UN>quantile(merge_data_final$PC_TT_UN,probs = seq(0, 1, 0.10))[2],]
merge_data_final=merge_data_final[merge_data_final$PC_TT_UN<=quantile(merge_data_final$PC_TT_UN,probs = seq(0, 1, 0.10))[10],]

vetor_percentis=quantile(merge_data_final$aliquota_real, probs = seq(0,1,0.1))
vetor_cores=c("red","green","yellow", "blue", "pink", "gray", "orange", "cyan", "chocolate","black")
for (i in 2:length(vetor_percentis)){
  tiert0=vetor_percentis[i-1]
  tiert1=vetor_percentis[i]
  
  preco_lan_medio_1995=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==1995]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==1995])))
  preco_lan_medio_1996=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==1996]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==1996])))
  preco_lan_medio_1997=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==1997]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==1997])))
  preco_lan_medio_1998=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==1998]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==1998])))
  preco_lan_medio_1999=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==1999]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==1999])))
  preco_lan_medio_2000=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2000]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2000])))
  preco_lan_medio_2001=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2001]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2001])))
  preco_lan_medio_2002=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2002]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2002])))
  preco_lan_medio_2003=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2003]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2003])))
  preco_lan_medio_2004=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2004]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2004])))
  preco_lan_medio_2005=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2005]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2005])))
  preco_lan_medio_2006=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2006]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2006])))
  preco_lan_medio_2007=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2007]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2007])))
  preco_lan_medio_2008=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2008]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2008])))
  
  
  vetor_precos_lan_tempo=c(preco_lan_medio_1995,preco_lan_medio_1996,preco_lan_medio_1997,preco_lan_medio_1998,preco_lan_medio_1998,
                           preco_lan_medio_2000,preco_lan_medio_2001,preco_lan_medio_2002,
                           preco_lan_medio_2003,preco_lan_medio_2004,preco_lan_medio_2005,preco_lan_medio_2006,
                           preco_lan_medio_2007,preco_lan_medio_2008)
  anos=c(1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)
  
  ###Sem corrigir inflação
  if (i==2){ 
    plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Média geométrica valor de lançamento por ano",ylim = c(100000,210000))
    abline(v=2001,col="red")
    }
  lines(y=vetor_precos_lan_tempo,x=anos,col=vetor_cores[i-1])
#legend(1997,1900000,legend = c("x: Limite superior","+: Limite inferior"))

  
}

legend(2006,150000,legend = format(round(vetor_percentis[1:length(vetor_percentis)-1], 2), nsmall = 2),col = vetor_cores,cex = .7,fill  = vetor_cores[1:length(vetor_percentis)-1])


#2x2
load("~/ws_final.RData")
#merge_data_final=merge_data_final[merge_data_final$PC_TT_UN>quantile(merge_data_final$PC_TT_UN,probs = seq(0, 1, 0.10))[2],]
merge_data_final=merge_data_final[merge_data_final$PC_TT_UN<=quantile(merge_data_final$PC_TT_UN,probs = seq(0, 1, 0.10))[10],]

vetor_percentis=quantile(merge_data_final$aliquota_real, probs = seq(0,1,0.1))
vetor_cores=c("red","green","yellow", "blue", "pink", "gray", "orange", "cyan", "chocolate","black")
for (i in 2:length(vetor_percentis)){
  tiert0=vetor_percentis[i-1]
  tiert1=vetor_percentis[i]
  
   preco_lan_medio_2000=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2000]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2000])))
  preco_lan_medio_2001=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2001]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2001])))
  preco_lan_medio_2002=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2002]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2002])))

  
  vetor_precos_lan_tempo=c(
                           preco_lan_medio_2000,preco_lan_medio_2001,preco_lan_medio_2002)
  anos=c(2000,2001,2002)
  
  ###Sem corrigir inflação
  if (i==2){ 
    plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Média geométrica valor de lançamento por ano",ylim = c(100000,170000))
    abline(v=2001,col="red")
  }
  lines(y=vetor_precos_lan_tempo,x=anos,col=vetor_cores[i-1])
  #legend(1997,1900000,legend = c("x: Limite superior","+: Limite inferior"))
  
  
}
legend(2001.5,130000,legend = format(round(vetor_percentis[1:length(vetor_percentis)-1], 2), nsmall = 2),col = vetor_cores,cex = .7,fill  = vetor_cores[1:length(vetor_percentis)-1])



#ln()
load("~/ws_final.RData")

merge_data_final=merge_data_final[merge_data_final$PC_TT_UN<=quantile(merge_data_final$PC_TT_UN,probs = seq(0, 1, 0.10))[10],]

vetor_percentis=quantile(merge_data_final$aliquota_real, probs = seq(0,1,0.1))
vetor_cores=c("red","green","yellow", "blue", "pink", "gray", "orange", "cyan", "chocolate","black","red","green","yellow", "blue", "pink", "gray", "orange", "cyan", "chocolate","black")


for (i in 2:length(vetor_percentis)){
  tiert0=vetor_percentis[i-1]
  tiert1=vetor_percentis[i]
  
  preco_lan_medio_1995=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==1995]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==1995])))
  preco_lan_medio_1996=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==1996]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==1996])))
  preco_lan_medio_1997=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==1997]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==1997])))
  preco_lan_medio_1998=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==1998]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==1998])))
  preco_lan_medio_1999=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==1999]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==1999])))
  preco_lan_medio_2000=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2000]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2000])))
  preco_lan_medio_2001=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2001]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2001])))
  preco_lan_medio_2002=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2002]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2002])))
  preco_lan_medio_2003=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2003]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2003])))
  preco_lan_medio_2004=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2004]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2004])))
  preco_lan_medio_2005=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2005]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2005])))
  preco_lan_medio_2006=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2006]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2006])))
  preco_lan_medio_2007=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2007]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2007])))
  preco_lan_medio_2008=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2008]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2008])))
  
  
  vetor_precos_lan_tempo=c(log(preco_lan_medio_1995),log(preco_lan_medio_1996),log(preco_lan_medio_1997),log(preco_lan_medio_1998),log(preco_lan_medio_1999),
                           log(preco_lan_medio_2000),log(preco_lan_medio_2001),
                           log(preco_lan_medio_2002),log(preco_lan_medio_2003),log(preco_lan_medio_2004),
                           log(preco_lan_medio_2005),log(preco_lan_medio_2006),log(preco_lan_medio_2007),
                           log(preco_lan_medio_2008))
  anos=c(1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)
  
  ###Sem corrigir inflação
  if (i==2){ 
    plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Média geométrica valor de lançamento por ano",ylim = c(11.5,13))
    abline(v=2001,col="red")
  }
  lines(y=vetor_precos_lan_tempo,x=anos,col=vetor_cores[i-1])
  #legend(1997,1900000,legend = c("x: Limite superior","+: Limite inferior"))
  
  
}


#corrigi inflacao
#plot inflaçao
ipca=c(0,0.0165,0.1074,0.1735,0.2635,0.4218,0.5540,0.6721,0.7672,0.8227,0.9040,1.016)
valor=1+1*ipca
anos=c(1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)
plot(y=valor,x=anos,type = 'l')

load("~/ws_final.RData")
merge_data_final=merge_data_final[merge_data_final$PC_TT_UN<=quantile(merge_data_final$PC_TT_UN,probs = seq(0, 1, 0.10))[10],]

vetor_percentis=quantile(merge_data_final$aliquota_real, probs = seq(0,1,0.1))
vetor_cores=c("red","green","yellow", "blue", "pink", "gray", "orange", "cyan", "chocolate","black","red","green","yellow", "blue", "pink", "gray", "orange", "cyan", "chocolate","black")

for (i in 2:length(vetor_percentis)){
  tiert0=vetor_percentis[i-1]
  tiert1=vetor_percentis[i]
  
  preco_lan_medio_1995=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==1995]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==1995])))
  preco_lan_medio_1996=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==1996]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==1996])))
  preco_lan_medio_1997=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==1997]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==1997])))
  preco_lan_medio_1998=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==1998]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==1998])))
  preco_lan_medio_1999=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==1999]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==1999])))
  preco_lan_medio_2000=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2000]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2000])))
  preco_lan_medio_2001=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2001]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2001])))
  preco_lan_medio_2002=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2002]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2002])))
  preco_lan_medio_2003=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2003]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2003])))
  preco_lan_medio_2004=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2004]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2004])))
  preco_lan_medio_2005=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2005]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2005])))
  preco_lan_medio_2006=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2006]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2006])))
  preco_lan_medio_2007=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2007]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2007])))
  preco_lan_medio_2008=prod(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2008]^(1/length(merge_data_final$PC_TT_UN[merge_data_final$ANO.DO.EXERCICIO[merge_data_final$aliquota_real[merge_data_final$aliquota_real<tiert1]>=tiert0]==2008])))
  

  
  preco_lan_medio_1997=preco_lan_medio_1997-preco_lan_medio_1997*0
  preco_lan_medio_1998=preco_lan_medio_1998-preco_lan_medio_1997*0.0165
  preco_lan_medio_1999=preco_lan_medio_1999-preco_lan_medio_1997*0.1074
  preco_lan_medio_2000=preco_lan_medio_2000-preco_lan_medio_1997*0.1735
  preco_lan_medio_2001=preco_lan_medio_2001-preco_lan_medio_1997*0.2635
  preco_lan_medio_2002=preco_lan_medio_2002-preco_lan_medio_1997*0.4218
  preco_lan_medio_2003=preco_lan_medio_2003-preco_lan_medio_1997*0.5540
  preco_lan_medio_2004=preco_lan_medio_2004-preco_lan_medio_1997*0.6721
  preco_lan_medio_2005=preco_lan_medio_2005-preco_lan_medio_1997*0.7672
  preco_lan_medio_2006=preco_lan_medio_2006-preco_lan_medio_1997*0.8227
  preco_lan_medio_2007=preco_lan_medio_2007-preco_lan_medio_1997*0.9040
  preco_lan_medio_2008=preco_lan_medio_2008-preco_lan_medio_1997*1.016
  
  
  #até 2004
  vetor_precos_lan_tempo=c(preco_lan_medio_1997,preco_lan_medio_1998,preco_lan_medio_1998,
                           preco_lan_medio_2000,preco_lan_medio_2001,preco_lan_medio_2002,
                           preco_lan_medio_2003,preco_lan_medio_2004,preco_lan_medio_2005,preco_lan_medio_2006
                           ,preco_lan_medio_2007,preco_lan_medio_2008)
  anos=c(1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008)
  
  ###Sem corrigir inflação
  if (i==2){ 
    plot(y=vetor_precos_lan_tempo,x=anos,pch=21,main = "Média geométrica valor de lançamento por ano",ylim = c(0,200000))
    abline(v=2001,col="red")
  }
  lines(y=vetor_precos_lan_tempo,x=anos,col=vetor_cores[i-1])
  #legend(1997,1900000,legend = c("x: Limite superior","+: Limite inferior"))
  
  
}

