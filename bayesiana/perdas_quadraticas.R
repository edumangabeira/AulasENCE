theta=seq(0,1,0.01)
L1=function(theta,a){((theta-a)^2)/(theta*(1-theta))}
par(mfrow=c(2,3))
plot(theta,L1(theta,0.5),type="l",main="Perda exemplo (a=0.5)",ylim=c(0,60))
plot(theta,L1(theta,0.25),type="l",main="Perda exemplo (a=0.25)",ylim=c(0,60))
plot(theta,L1(theta,0.75),type="l",main="Perda exemplo (a=0.75)",ylim=c(0,60))


#com perda quadrÃ¡tica
L2=function(theta,a){(theta-a)^2}
plot(theta,L2(theta,0.5),type="l",main="Perda QuadrÃ¡tica (a=0.5)",ylim=c(0,1))
plot(theta,L2(theta,0.25),type="l",main="Perda QuadrÃ¡tica (a=0.25)",ylim=c(0,1))
plot(theta,L2(theta,0.75),type="l",main="Perda QuadrÃ¡tica (a=0.75)",ylim=c(0,1))
