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

