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

Cokolada <- function(M,r,s){
  # M reprezentuje labyrint, r je radek a s sloupec
  # Priklad matrix(c(3,0,0,0,1,4,0,0,5,3,0,0,1,2,6,7), nrow = 4, ncol = 4, byrow = TRUE)
  
  # Rekurzivni zpusob
  if (r == nrow(M)){
    return(M[r,s])
  }
  else{
    C <- M[r,s]
    Cdolu <- Cokolada(M,r+1,s)
    Csikmo <- Cokolada(M,r+1,s+1)
    return(max(Cdolu,Csikmo)+C)
  }
}

# Hanojske veze