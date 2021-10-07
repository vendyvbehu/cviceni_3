VratitMince <- function(M){
  # M je to, co mam vratit
  # z50 <- M %% 50 #zbytek po deleni 50
  # p50 <- (M - z50)/50 #pocet 50 minci
  mince <- c(50,20,10,5,2,1)
  pocet_minci <- matrix(0,1,6)
  zbytek <- M
  for (i in 1:length(mince)) {
  pocet <- floor(zbytek/mince[i]) #pocet 50 korunovych minci
  zbytek <- zbytek %% mince[i] # zbyva vratit
  pocet_minci[i]  <- pocet
  }
  return(pocet_minci)
}

# mince 50, 20, 10, 5, 2, 1