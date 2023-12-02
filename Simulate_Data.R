

library(forecast)

Sample4 <-read.csv("Sample4Most.csv", header = TRUE)[,-1]
Sample4_wide1 <- Sample4$freq %>%
  matrix(nrow = 64, ncol = nrow(Sample4)/64) %>%
  as.data.frame() %>%
  ts(frequency = 12)
colnames(Sample4_wide1) <- unique(Sample4$id)


###########################################NoiseFH
set.seed(234)
sim <- matrix(NA, ncol = ncol(Sample4_wide1), nrow = nrow(Sample4_wide1))
for(i in 1:ncol(Sample4_wide1)){
  sim[,i] <- simulate(auto.arima(Sample4_wide1[,i]), nsim=64, future = FALSE) 
}

sim.scale <- apply(sim,2,function(x) ((x-mean(x))/sd(x)))
sim.mean <- as.vector(apply(sim, 2, function(x) mean(x)))
sim.sd <- as.vector(apply(sim, 2, function(x) sd(x)))

set.seed(123)
noise001 <- matrix(NA, ncol = ncol(Sample4_wide1), nrow = nrow(Sample4_wide1))
for(i in 1:ncol(Sample4_wide1)){
  noise001[,i] <- arima.sim(model = list(order = c(0, 0, 0)), n =64, sd = 0.01)
}

set.seed(123)
noise01 <- matrix(NA, ncol = ncol(Sample4_wide1), nrow = nrow(Sample4_wide1))
for(i in 1:ncol(Sample4_wide1)){
  noise01[,i] <- arima.sim(model = list(order = c(0, 0, 0)), n =64, sd = 0.1)
}

set.seed(123)
noise05 <- matrix(NA, ncol = ncol(Sample4_wide1), nrow = nrow(Sample4_wide1))
for(i in 1:ncol(Sample4_wide1)){
  noise05[,i] <- arima.sim(model = list(order = c(0, 0, 0)), n =64, sd = 0.5)
}

set.seed(123)
noise1 <- matrix(NA, ncol = ncol(Sample4_wide1), nrow = nrow(Sample4_wide1))
for(i in 1:ncol(Sample4_wide1)){
  noise1[,i] <- arima.sim(model = list(order = c(0, 0, 0)), n =64, sd = 1)
}

sim001.scale <- sim.scale + noise001
sim001 <- sweep(sim001.scale, 2, sim.sd, FUN="*")
sim001 <- sweep(sim001, 2, sim.mean, FUN="+")
colnames(sim001) <- colnames(Sample4_wide1)

sim01.scale <- sim.scale + noise01
sim01 <- sweep(sim01.scale, 2, sim.sd, FUN="*")
sim01 <- sweep(sim01, 2, sim.mean, FUN="+")
colnames(sim01) <- colnames(Sample4_wide1)

sim05.scale <- sim.scale + noise05
sim05 <- sweep(sim05.scale, 2, sim.sd, FUN="*")
sim05 <- sweep(sim05, 2, sim.mean, FUN="+")
colnames(sim05) <- colnames(Sample4_wide1)

sim1.scale <- sim.scale + noise1
sim1 <- sweep(sim1.scale, 2, sim.sd, FUN="*")
sim1 <- sweep(sim1, 2, sim.mean, FUN="+")
colnames(sim1) <- colnames(Sample4_wide1)

actual.sim.noise.FH <- rbind(cbind(reshape2::melt(sim001), Sim ='sig0.01'), cbind(reshape2::melt(sim01), Sim = 'sig0.1'), 
                             cbind(reshape2::melt(sim05), Sim = 'sig0.5'), cbind(reshape2::melt(sim1), Sim = 'sig1'))

write_csv(actual.sim.noise.FH, 'actual.sim.noise.FH.3Most.n.csv')
