spatialpostsampling <- function(data){
  data <- data %>% 
    group_by(level2) %>% 
    mutate(n=n())
  
  data$pik <- data$n/nrow(data)
  data$sample <- Slpm2(data$pik, cbind(data$lon, data$lat), Distanza_Euclidea)
  
  std.error <- function(x)  sd(x)/sqrt(sum(!is.na(x)))
  price_ps <- mean(data$price_kg[data$sample==1])
  se_price_ps  <- std.error(data$price_kg[data$sample==1])
  n_price_ps <- nrow(data[data$sample==1, ])       
  sd_price_ps <- se_price_ps * sqrt(n_price_ps)
  price_mu <- mean(data$price_kg)
  se_price_mu <- std.error(data$price_kg)
  n_price_mu <- nrow(data)
  sd_price_mu <- se_price_mu * sqrt(n_price_mu)
  
  n <- n_price_mu
  m <- n_price_ps
  #m <- round(sum(n)/sum(m)*m)
  N <- sum(n)

  cri <- 1 - sum((m-n)^2) / (sum(n)^2 + 2*N*min(n) + N^2)
  cat("Price mean ", price_mu, " - Price ps ", price_ps, 'Spatial Post-sampling CRI:', cri, "\n")
  return(list(price_ps = price_ps, price_mu = price_mu, se_price_ps = se_price_ps, sd_price_ps = sd_price_ps,  n_price_ps = n_price_ps, se_price_mu = se_price_mu, sd_price_mu = sd_price_mu, n_price_mu = n_price_mu, cri = cri ))
}


# Selezione di campioni well-spread in uno spazio topologico
# Il metodo impiegato ? proposto da Grafstr?m et al. 2012, indicato come "lpm2"
# La funzione restituisce un campione bilanciato nello spazio
#
# ARGOMENTI DELLA FUNZIONE
# pik ? il vettore di dimensione N, dove pik[i]? la probabilit? di inclusione per l'unit? i
# X ? un vettore (o matrice) di dimensione N x q, di variabili ausiliarie (standardizzate)
# funzdist ? una funzione che prende in input i valori X[i,] e X[j,] (coordinate)
# e restituisce la distanza tra i e j
#
# INDICAZIONE DELL'USO DELLA FUNZIONE
# S = Slpm2(pik,X,funzdist);
# S ? un indicatore di inclusione
# S[i] = 0 se l'unit? i non ? inclusa nel campione 
# S[i] = 1 se l'unit? i ? inclusa nel campione


Slpm2 <- function(pik,X,funzdist){
N = length(pik);
unfinished  = 1:N;
while(length(unfinished) > 0){
i = unfinished[floor(runif(1)*length(unfinished))+1];
rand = runif(1);		
if(length(unfinished) > 1){
mindist = Inf;
nearest = c();
others = unfinished[unfinished!=i];
for(j in others){
dist = funzdist(X[j,],X[i,]);
if(dist == mindist){
nearest = c(nearest,j);
}
if(dist < mindist){
mindist = dist;
nearest = c(j);
}
}
j = nearest[floor(runif(1)*length(nearest))+1];				
if(pik[i]+pik[j]<1){
if( rand < pik[i]/(pik[i]+pik[j]) ){
pik[i] = pik[i]+pik[j];
pik[j] = 0;		
}else{
pik[j] = pik[i]+pik[j];
pik[i] = 0;					
}
}else{
if( rand < (1-pik[j])/(2-pik[i]-pik[j])){
pik[j] = pik[i]+pik[j]-1;		
pik[i] = 1;
}else{
pik[i] = pik[i]+pik[j]-1;
pik[j] = 1;					
}
}
if(pik[i]==0|pik[i]==1){unfinished = unfinished[unfinished!=i];}
if(pik[j]==0|pik[j]==1){unfinished = unfinished[unfinished!=j];}	
}else{
if( rand < pik[i] ){
pik[i] = 1;
}else{
pik[i] = 0;
}
unfinished = unfinished[unfinished!=i]; 
}
}  
pik;
}

# Funzione di distanza utilizzata (Euclidea, si pu? scegliere eventualmente un'altra)
Distanza_Euclidea  = function(Xi,Xj){
ret=sqrt(sum((Xi-Xj)^2));
}
##
##



###############################
###############################
###############################

