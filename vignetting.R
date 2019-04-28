# Análisis de viñeteo en ópticas con R 
# www.datosimagensonido.com

# Conversión de ARW (Sony) a DNG con Adobe DNG Converter
# Revelado lineal con DCRAW: dcraw -v -r 1 1 1 1 -o 0 -4 -T *.DNG
# Conversión de TIFF a PNG en Photoshop

# Leemos imágenes
library(png)
f35=readPNG("35mm.png")
f50=readPNG("50mm.png")
f85=readPNG("85mm.png")

# Combinamos los 3 canales en los 3 objetivos (1=35mm, 2=50mm, 3=85mm) -> Luminosidad
NUMOBJ=3
lum=array(0,dim=c(dim(f35[,,1]), NUMOBJ))

lum[,,1] = 0.299*f35[,,1] + 0.587*f35[,,2] + 0.114*f35[,,3]
lum[,,2] = 0.299*f50[,,1] + 0.587*f50[,,2] + 0.114*f50[,,3]
lum[,,3] = 0.299*f85[,,1] + 0.587*f85[,,2] + 0.114*f85[,,3]

# Calculamos perfil de esquina a esquina pasando por el centro de la imagen
perfil=array(0,dim=c(ncol(lum), NUMOBJ))

for (objetivo in 1:NUMOBJ) {
    for (x in 1:1024) {
      y = (681-1)/(1024-1)*(x-1)+1 # Con (x,y) recorremos la diagonal
      perfil[x, objetivo] = lum[y, x, objetivo]
    }
}

# Dibujamos gráfica
xaxis=seq(-0.5, 0.5, len=1024)
          
plot(xaxis, log2(perfil[,2]/max(perfil[,2])), type='l', col="red", main="Perfil de viñeteo",
     xlab="Diagonal", ylab="Viñeteo (EV)") # 50mm
lines(xaxis, log2(perfil[,1]/max(perfil[,1])), col="green") # 35mm
lines(xaxis, log2(perfil[,3]/max(perfil[,3])), col="blue") # 85mm

abline(h=c(0,-1,-2), col="lightgray", lty = "dotted") # rejilla horizontal
abline(v=0)
a=0.5/((1+(3/2)^2)^0.5) # Extremos izq/der y sup/inf del encuadre
abline(v=c(-0.5,-a*3/2,-a,a,a*3/2,0.5), lty=2) # Extremos izq/der y sup/inf del encuadre
legend("bottom", inset=.05, c("35mm @ f/2","50mm @ f/1,4","85mm @ f/1,8"),
  fill=c("green", "red", "blue"))


