varpor <- function(añoant, añoact) {
        añoant <- sum(añoant)
        añoact <- sum(añoact)
        diferencia <- añoact-añoant
        difporcent <- diferencia*100/añoant
        texto <- paste0("Variación %: ", difporcent,", ", "Número de casos: ", 
                        diferencia, ", ", "Tendencia: ")
        if(difporcent<0){
                print(paste0(texto, "Disminuyó"))
        } else {
                print(paste0(texto, "Aumentó"))
        } 
}

varpor(20,1)
