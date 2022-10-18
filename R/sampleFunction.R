# Selezione di campioni well-spread in uno spazio topologico
# Il metodo impiegato è proposto da Grafström et al. 2012, indicato come "lpm2"
# La funzione restituisce un campione bilanciato nello spazio
#
# ARGOMENTI DELLA FUNZIONE
# pik è il vettore di dimensione N, dove pik[i]è la probabilità di inclusione per l'unità i
# X è un vettore (o matrice) di dimensione N x q, di variabili ausiliarie (standardizzate)
# funzdist è una funzione che prende in input i valori X[i,] e X[j,] (coordinate)
# e restituisce la distanza tra i e j
#
# INDICAZIONE DELL'USO DELLA FUNZIONE
# S = Slpm2(pik,X,funzdist);
# S è un indicatore di inclusione
# S[i] = 0 se l'unità i non è inclusa nel campione 
# S[i] = 1 se l'unità i è inclusa nel campione


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

# Funzione di distanza utilizzata (Euclidea, si può scegliere eventualmente un'altra)
Distanza_Euclidea  = function(Xi,Xj){
ret=sqrt(sum((Xi-Xj)^2));
}
##
##



###############################
###############################
###############################

