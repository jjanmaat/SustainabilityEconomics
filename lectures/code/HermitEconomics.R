#
# Figures for hermit economics slides
#
# Hermit needs 200 - 250 kg of grain per year, and 150 - 180 kg of fish
#
# Grain production, input labour
grain <- function(l,a=1,b=0.5) {
  1000*(l/50)^b*a^(1-b)
}
#
# Fish production
fish <- function(e,b=0.5) {
  10*(e/0.5)^b
}
#
# Time available
tVec <- 0:365
#
# get yeilds
gYield <- grain(tVec)
fYield <- fish(tVec)
#
# Plot them
datPl <- data.frame(t=c(tVec,tVec),Ylab=c(rep("grain",length(tVec)),
                                          rep("fish",length(tVec))),
                    Y=c(gYield,fYield))
library(ggplot2)
pl <- ggplot(datPl,aes(x=t,y=Y)) +
  geom_line(linewidth=1.5) +
  labs(y="Yield (kg)", x="Labour/Effort (days") +
  scale_x_continuous(expand=expansion(mult=c(0,0.02))) +
  scale_y_continuous(expand=expansion(mult=c(0,0.02))) +
  facet_grid(Ylab ~ ., scales="free_y")
print(pl)
ggsave(file="HermitProdFun.png",plot=pl,width=6,height=4)
#
# Add lines to use for total product
tVal <- 180
datPl2 <- data.frame(t=rep(c(tVal,tVal,0)),Ylab=c(rep("grain",3),rep("fish",3)),
                     Y=c(0,rep(grain(tVal),2),0,rep(fish(tVal),2)))
plTP <- pl + geom_path(aes(x=t,y=Y),data=datPl2,linewidth=1,linetype="dashed") +
  facet_grid(Ylab ~ ., scales="free_y")
print(plTP)
ggsave(file="HermitProdFunTP.png",plot=plTP,width=6,height=4)
#
# Add lines to use for marginal product
tVal <- 180
tDel <- 10
datPl3 <- data.frame(t=rep(c(tVal-tDel,tVal-tDel,0)),Ylab=c(rep("grain",3),rep("fish",3)),
                     Y=c(0,rep(grain(tVal-tDel),2),0,rep(fish(tVal-tDel),2)))
datPl4 <- data.frame(t=rep(c(tVal+tDel,tVal+tDel,0)),Ylab=c(rep("grain",3),rep("fish",3)),
                     Y=c(0,rep(grain(tVal+tDel),2),0,rep(fish(tVal+tDel),2)))
plMP <- pl + 
  geom_path(aes(x=t,y=Y),data=datPl3,linewidth=1,linetype="dashed") +
  geom_path(aes(x=t,y=Y),data=datPl4,linewidth=1,linetype="dashed") +
  facet_grid(Ylab ~ ., scales="free_y")
print(plMP)
ggsave(file="HermitProdFunMP.png",plot=plMP,width=6,height=4)
#
# Now put them together in a production possibilities frontier
datPl <- data.frame(grain=gYield,fish=rev(fYield))
pl <- ggplot(datPl,aes(x=grain,y=fish)) +
  geom_line(linewidth=1.5) +
  scale_x_continuous(expand=expansion(mult=c(0,0.02))) +
  scale_y_continuous(expand=expansion(mult=c(0,0.02))) +
  labs(x="Grain (kg)",y="Fish (kg)")
print(pl)
ggsave(file="HermitPPF.png",plot=pl,width=4,height=4)
