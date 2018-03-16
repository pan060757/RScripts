getratio <- function(X0)
{
   ratio <- c()
   for(i in 1:length(X0))
   {
     r<-(X0[i]-X0[i-1])/X0[i-1]
     ratio <- c(ratio,r)
   }
   return (ratio)
}