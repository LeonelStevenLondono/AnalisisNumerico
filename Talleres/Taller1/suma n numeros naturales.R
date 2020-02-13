sumaNaturales <- function(b)
{
  vecto <- c()
 for( i in b)
 {
   suma = 0
   
   for(d in 0:i^2)
   {
     suma = suma + d^2
   }
  c(vecto,suma)
 }
  points(b, vecto, col = "red")
  lines(b, vecto, col = "blue")

  plot(b,vecto)
  
  
}
b<-c(10,20,30,40,50,60,100,200,300)

sumaNaturales(b)