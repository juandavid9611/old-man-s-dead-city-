#Randy Darrell  Gustavo Mendez Adrian  
f_ <-function(x0,d0,d1,delta){
  f<-function(t){
    r<-x0
    k<-t
    r<-r+k*d0
    k<-k*t
    r<-r+k*(3*delta-2*d0-d1)
    k<-k*t
    r<-r+k*(d0+d1-2*delta)
  }
  return(f)
}

S_ <-function(x,k){
  s<-c()
  i=0
  L=length(x)
  while(i<L+1){
    x0_=(x[i%%L]+x[(i+1)%%L])/2
    d0_=k*(x[(i+1)%%L]-x[i%%L])
    d1_=k*(x[(i+2)%%L]-x[(i+1)%%L])
    del_=(x[(i+2)%%L]-x[i%%L])/2
    f=f_(x0_,d0_,d1_,del_)
    j=0
    while(j<1){
      r=f(j)
      s=append(s,r)
      j=j+0.1
    }
    i=i+1
  }
  
  return(s)
}

plotS <-function(x,y,k,x1,y1){
  
  x_<-S_(x,k)
  y_<-S_(y,k)
  z<-rep(1,32)
  plot( x_, y_, type="l", col="red",asp=1 )
  par(new=TRUE)
  plot( x1, y1, type="p", col="green",asp=1 )
  }

#interpolacion
##Puntos 

x=c(381,188,54,18,32,58,144,199,199,226,224,227,257,288,319,357,370,403,437,442,469,529,553,582,546,569,658,679,697,626,589,520,381,188,54)                                                                                                       
y=c(855,792,572,457,436,436,519,584,583,570,322,126,113,120,417,414,90,75,99,426,427,124,122,146,449,460,247,245,266,540,786,858,855,792,572)     
y=y*-1

plotS(x,y,1,x,y)