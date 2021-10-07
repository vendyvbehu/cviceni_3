# Problem s vracenim minci

VratitMince <- function(M, mince){
  # M je to, co mam vratit, mince jsou vektorem minci, ktere jsou pouzite.
  # Pr. VratitMince(423, c(50,20,10,5,2,1))
  
  pocet_minci <- matrix(0,1,length(mince))
  zbytek <- M
  
  # Vracim od nejvetsich (napr od 50).
  for (i in 1:length(mince)) {
  pocet <- floor(zbytek/mince[i]) #pocet 50 korunovych minci
  zbytek <- zbytek %% mince[i] # zbyva vratit
  pocet_minci[i]  <- pocet
  }
  return(pocet_minci)
}

# Nejcokoladovejsi cesta

CokoladaRek <- function(M,r,s){
  # M reprezentuje labyrint, r je radek a s sloupec (Start pozice mysky)
  # Priklad matrix(c(3,0,0,0,1,4,0,0,5,3,0,0,1,2,6,7), nrow = 4, ncol = 4, byrow = TRUE)
  
  # Rekurzivni zpusob reseni
  if (r == nrow(M)){
    return(M[r,s])
  }
  else{
    C <- M[r,s]
    Cdolu <- CokoladaRek(M,r+1,s)
    Csikmo <- CokoladaRek(M,r+1,s+1)
    return(max(Cdolu,Csikmo)+C)
  }
}
  
  # Iterativni zpusob reseni
CokoladaIt <- function(M){
  # M reprezentuje labyrint, r a s startovni pozice mysky
  s <- dim(M)
  k1 <- seq(from=s[1]-1, to=1, by=-1)
  for (r in k1){
    k2 <- seq(from=r, to=1, by=-1)
    for (s in k2){
      Cdolu <- M[r+1,s]+M[r,s]
      Csikmo <- M[r+1,s+1]+M[r,s]
      M[r,s] <- max(c(Cdolu, Csikmo))
    }
  }
  return(M[1,1])
  }

# Hanojske veze
HanojskaVez <- function(n,zkoliku,nakolik){
  if (n==1){
    print(paste(unlist(c('Pøesuò disk z kolíku ', as.character(zkoliku), ' na kolík ', as.character(nakolik), '.')), collapse = ''))
  }
  else{
    volnyKolik <- 6 - zkoliku - nakolik
    HanojskaVez(n-1,zkoliku,volnyKolik)
    print(paste(unlist(c('Pøesuò disk z kolíku ', as.character(zkoliku), ' na kolík ', as.character(nakolik), '.')), collapse = ''))
    HanojskaVez(n-1,volnyKolik,nakolik)
  }
}