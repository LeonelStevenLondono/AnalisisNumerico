
#-------------------
#autores:
#Andres Gil 
#Erika Gonzalez 
#Leonel Londono
#Analisis numerico

#------------------


puntosX <- c(1,2,5,6,7.5,8.1,10,13,17.6,20,23.5,24.5,25,26.5,27.5,28,29,30)
puntosY <- c(3,3.7,3.9,4.5,5.7,6.69,7.12,6.7,4.45,7,6.1,5.6,5.87,5.15,4.1,4.3,4.1,3)
# CODIGO
x=c(1,2,6,9,14,17.6)     
x1=c(17.6,20,23,24.5,26.5,28.7,29.1)
x2=c(28.4,29,29.275)
y=c(3,3.7,4.5,7.12,6.7,4.45)
y1=c(4.45,7,6.5,5.6,5.87,4.8,3.71)
y2=c(5.1,4.1,3)
a=c(1, 5.39, 8.62)
a1=c(7.9,8.1,8.62)
a2=c(7.9,9,11.5,13.76,16.95,20.22,23.5,26.83,28.45,29.1)
ay=c(3,2.5,3.16)
ay1=c(2.03,2.6,3.16)
ay2=c(2.03,1.9,2.15,1.85,1.85,1.8,1.55,1.54,2.35,3.71)
plot(x, y, xlim=c(1,30), ylim=c(2, 20), xlab = "X", ylab = "Y", main = "Interpolacion Perro")
lines(spline(x, y, n = 20), col = 2)
par(new = TRUE)
plot(x1, y1, xlim=c(1,30), ylim=c(2, 20), xlab = "X", ylab = "Y")
lines(spline(x1, y1, n = 20), col = 2)
par(new = TRUE)
plot(a, ay, xlim=c(1,30), ylim=c(2, 20), xlab = "X", ylab = "Y")
lines(spline(a, ay, n = 20), col = 2)
par(new = TRUE)
plot(a1, ay1, xlim=c(1,30), ylim=c(2, 20), xlab = "X", ylab = "Y")
lines(spline(a1, ay1, n = 20), col = 2)
par(new = TRUE)
plot(a2, ay2, xlim=c(1,30), ylim=c(2, 20), xlab = "X", ylab = "Y")
lines(spline(a2, ay2, n = 20), col = 2, xlab = "X", ylab = "Y")



#--------------------------------