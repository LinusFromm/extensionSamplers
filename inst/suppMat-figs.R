#### Data ####

samplesHighway4UniformFMB <- read.csv("4-highway_uniform_FMB.csv")
samplesHighway4Uniform1MB <- read.csv("4-highway_uniform_1MB.csv")
samplesHighway4UniformHR <- read.csv("4-highway_uniform_HR.csv")

samplesHighway7UniformFMB <- read.csv("7-highway_uniform_FMB.csv")
samplesHighway7Uniform2MB <- read.csv("7-highway_uniform_2MB.csv")
samplesHighway7UniformHR <- read.csv("7-highway_uniform_HR.csv")

samplesHighway7PoissonFMB <- read.csv("7-highway_poisson_FMB.csv")
samplesHighway7Poisson2MB <- read.csv("7-highway_poisson_2MB.csv")
samplesHighway7PoissonHR <- read.csv("7-highway_poisson_HR.csv")

samples3x4x6PoissonFMB <- read.csv("3x4x6_poisson_FMB.csv")
samples3x4x6Poisson1MB <- read.csv("3x4x6_poisson_1MB.csv")
samples3x4x6PoissonHR <- read.csv("3x4x6_poisson_HR.csv")

samples3x4x6UniformFMB <- read.csv("3x4x6_uniform_FMB.csv")
samples3x4x6Uniform1MB <- read.csv("3x4x6_uniform_1MB.csv")
samples3x4x6UniformHR <- read.csv("3x4x6_uniform_HR.csv")

samples3x4x12Poisson1MB <- read.csv("3x4x12_poisson_1MB.csv")
samples3x4x12PoissonHR <- read.csv("3x4x12_poisson_HR.csv")

samples3x4x20Uniform1MB <- read.csv("3x4x20_uniform_1MB.csv")
samples3x4x20UniformHR <- read.csv("3x4x20_uniform_HR.csv")

#### Traceplots ####

par(mfrow = c(1,3))
extensionSamplers::traceplot(samplesHighway4UniformFMB, 2, FALSE)
extensionSamplers::traceplot(samplesHighway4Uniform1MB, 2, TRUE)
extensionSamplers::traceplot(samplesHighway4UniformHR, 2, TRUE)

par(mfrow = c(1,3))
extensionSamplers::traceplot(samplesHighway7UniformFMB, 3, FALSE)
extensionSamplers::traceplot(samplesHighway7Uniform2MB, 3, TRUE)
extensionSamplers::traceplot(samplesHighway7UniformHR, 3, TRUE)

par(mfrow = c(1,3))
extensionSamplers::traceplot(samplesHighway7PoissonFMB, 3, FALSE)
extensionSamplers::traceplot(samplesHighway7Poisson2MB, 3, TRUE)
extensionSamplers::traceplot(samplesHighway7PoissonHR, 3, TRUE)

par(mfrow = c(1,3))
extensionSamplers::traceplot(samples3x4x6PoissonFMB, 59, FALSE)
extensionSamplers::traceplot(samples3x4x6Poisson1MB, 59, TRUE)
extensionSamplers::traceplot(samples3x4x6PoissonHR, 59, TRUE)

par(mfrow = c(1,2))
extensionSamplers::traceplot(samples3x4x12Poisson1MB, 59, TRUE)
extensionSamplers::traceplot(samples3x4x12PoissonHR, 59, TRUE)

par(mfrow = c(1,2))
extensionSamplers::traceplot(samples3x4x20Uniform1MB, 11, TRUE)
extensionSamplers::traceplot(samples3x4x20UniformHR, 11, TRUE)


#### Delta ####

deltaHighway4Uniform1MB <- sum(complete.cases(extensionSamplers::onlyPositive(samplesHighway4Uniform1MB)))/nrow(samplesHighway4Uniform1MB)
deltaHighway4UniformHR <- sum(complete.cases(extensionSamplers::onlyPositive(samplesHighway4UniformHR)))/nrow(samplesHighway4UniformHR)

deltaHighway7Uniform2MB <- sum(complete.cases(extensionSamplers::onlyPositive(samplesHighway7Uniform2MB)))/nrow(samplesHighway7Uniform2MB)
deltaHighway7UniformHR <- sum(complete.cases(extensionSamplers::onlyPositive(samplesHighway7UniformHR)))/nrow(samplesHighway7UniformHR)

deltaHighway7Poisson2MB <- sum(complete.cases(extensionSamplers::onlyPositive(samplesHighway7Poisson2MB)))/nrow(samplesHighway7Poisson2MB)
deltaHighway7PoissonHR <- sum(complete.cases(extensionSamplers::onlyPositive(samplesHighway7PoissonHR)))/nrow(samplesHighway7PoissonHR)

delta3x4x6Uniform1MB <- sum(complete.cases(extensionSamplers::onlyPositive(samples3x4x6Uniform1MB)))/nrow(samples3x4x6Uniform1MB)
delta3x4x6UniformHR <- sum(complete.cases(extensionSamplers::onlyPositive(samples3x4x6UniformHR)))/nrow(samples3x4x6UniformHR)

delta3x4x6Poisson1MB <- sum(complete.cases(extensionSamplers::onlyPositive(samples3x4x6Poisson1MB)))/nrow(samples3x4x6Poisson1MB)
delta3x4x6PoissonHR <- sum(complete.cases(extensionSamplers::onlyPositive(samples3x4x6PoissonHR)))/nrow(samples3x4x6PoissonHR)

delta3x4x12Poisson1MB <- sum(complete.cases(extensionSamplers::onlyPositive(samples3x4x12Poisson1MB)))/nrow(samples3x4x12Poisson1MB)
delta3x4x12PoissonHR <- sum(complete.cases(extensionSamplers::onlyPositive(samples3x4x12PoissonHR)))/nrow(samples3x4x12PoissonHR)

delta3x4x20Uniform1MB <- sum(complete.cases(extensionSamplers::onlyPositive(samples3x4x20Uniform1MB)))/nrow(samples3x4x20Uniform1MB)
delta3x4x20UniformHR <- sum(complete.cases(extensionSamplers::onlyPositive(samples3x4x20UniformHR)))/nrow(samples3x4x20UniformHR)

#### ESS ####

essHighway4UniformFMB <- posterior::summarise_draws(extensionSamplers::to_draws_df(samplesHighway4UniformFMB), "ess_bulk")$ess_bulk
essHighway4Uniform1MB <- posterior::summarise_draws(extensionSamplers::to_draws_df(samplesHighway4Uniform1MB), "ess_bulk")$ess_bulk*deltaHighway4Uniform1MB
essHighway4UniformHR <- posterior::summarise_draws(extensionSamplers::to_draws_df(samplesHighway4UniformHR), "ess_bulk")$ess_bulk*deltaHighway4UniformHR

essHighway7UniformFMB <- posterior::summarise_draws(extensionSamplers::to_draws_df(samplesHighway7UniformFMB), "ess_bulk")$ess_bulk
essHighway7Uniform2MB <- posterior::summarise_draws(extensionSamplers::to_draws_df(samplesHighway7Uniform2MB), "ess_bulk")$ess_bulk*deltaHighway7Uniform2MB
essHighway7UniformHR <- posterior::summarise_draws(extensionSamplers::to_draws_df(samplesHighway7UniformHR), "ess_bulk")$ess_bulk*deltaHighway7UniformHR

essHighway7PoissonFMB <- posterior::summarise_draws(extensionSamplers::to_draws_df(samplesHighway7PoissonFMB), "ess_bulk")$ess_bulk
essHighway7Poisson2MB <- posterior::summarise_draws(extensionSamplers::to_draws_df(samplesHighway7Poisson2MB), "ess_bulk")$ess_bulk*deltaHighway7Poisson2MB
essHighway7PoissonHR <- posterior::summarise_draws(extensionSamplers::to_draws_df(samplesHighway7PoissonHR), "ess_bulk")$ess_bulk*deltaHighway7PoissonHR

ess3x4x6UniformFMB <- posterior::summarise_draws(extensionSamplers::to_draws_df(samples3x4x6UniformFMB), "ess_bulk")$ess_bulk
ess3x4x6Uniform1MB <- posterior::summarise_draws(extensionSamplers::to_draws_df(samples3x4x6Uniform1MB), "ess_bulk")$ess_bulk*delta3x4x6Uniform1MB
ess3x4x6UniformHR <- posterior::summarise_draws(extensionSamplers::to_draws_df(samples3x4x6UniformHR), "ess_bulk")$ess_bulk*delta3x4x6UniformHR

ess3x4x6PoissonFMB <- posterior::summarise_draws(extensionSamplers::to_draws_df(samples3x4x6PoissonFMB), "ess_bulk")$ess_bulk
ess3x4x6Poisson1MB <- posterior::summarise_draws(extensionSamplers::to_draws_df(samples3x4x6Poisson1MB), "ess_bulk")$ess_bulk*delta3x4x6Poisson1MB
ess3x4x6PoissonHR <- posterior::summarise_draws(extensionSamplers::to_draws_df(samples3x4x6PoissonHR), "ess_bulk")$ess_bulk*delta3x4x6PoissonHR

ess3x4x12Poisson1MB <- posterior::summarise_draws(extensionSamplers::to_draws_df(samples3x4x12Poisson1MB), "ess_bulk")$ess_bulk*delta3x4x12Poisson1MB
ess3x4x12PoissonHR <- posterior::summarise_draws(extensionSamplers::to_draws_df(samples3x4x12PoissonHR), "ess_bulk")$ess_bulk*delta3x4x12PoissonHR

ess3x4x20Uniform1MB <- posterior::summarise_draws(extensionSamplers::to_draws_df(samples3x4x20Uniform1MB), "ess_bulk")$ess_bulk*delta3x4x20Uniform1MB
ess3x4x20UniformHR <- posterior::summarise_draws(extensionSamplers::to_draws_df(samples3x4x20UniformHR), "ess_bulk")$ess_bulk*delta3x4x20UniformHR


#### ESS plots ####

# 4-Highway (Uniform)

pdf("../figs/supMatFig1.pdf", width = 8, height = 5)
par(mfrow = c(1,1))
barplot(log10(rbind(essHighway4UniformFMB,
                    essHighway4Uniform1MB,
                    essHighway4UniformHR)), 
        beside = TRUE,
        legend.text = c("FMB", "1MB", "HR"),
        args.legend = list(bg = "NA", x = "topright", horiz = TRUE, cex = 0.7),
        ylab = expression(log[10](ESS)),
        ylim = c(0, 5),
        xlab = "Variable",
        names.arg = parse(text = paste0("x[", 1:10, "]")))
box()
dev.off()

# 7-Highway (Uniform)
pdf("../figs/supMatFig3.pdf", width = 8, height = 5)
par(mfrow = c(1,1))
boxplot(log10(cbind(essHighway7UniformFMB,
                    essHighway7Uniform2MB,
                    essHighway7UniformHR)),
        xlab = "Sampler",
        names = c("FMB", "1MB", "HR"),
        ylab = expression(log[10](ESS)),
        outcol = "#00000000")
points(rep(1, length(essHighway7UniformFMB))+rnorm(length(essHighway7UniformFMB), 0, 0.025), 
       log10(essHighway7UniformFMB), 
       pch = 20)
points(rep(2, length(essHighway7Uniform2MB))+rnorm(length(essHighway7Uniform2MB), 0, 0.025), 
       log10(essHighway7Uniform2MB), 
       pch = 20)
points(rep(3, length(essHighway7UniformHR))+rnorm(length(essHighway7UniformHR), 0, 0.025), 
       log10(essHighway7UniformHR), 
       pch = 20)
dev.off()

# 7-Highway (Poisson)
pdf("../figs/supMatFig4.pdf", width = 8, height = 5)
par(mfrow = c(1,1))
boxplot(log10(cbind(essHighway7PoissonFMB,
                    essHighway7Poisson2MB,
                    essHighway7PoissonHR)),
        xlab = "Sampler",
        names = c("FMB", "1MB", "HR"),
        ylab = expression(log[10](ESS)),
        outcol = "#00000000")
points(rep(1, length(essHighway7PoissonFMB))+rnorm(length(essHighway7PoissonFMB), 0, 0.025), 
       log10(essHighway7PoissonFMB), 
       pch = 20)
points(rep(2, length(essHighway7Poisson2MB))+rnorm(length(essHighway7Poisson2MB), 0, 0.025), 
       log10(essHighway7Poisson2MB), 
       pch = 20)
points(rep(3, length(essHighway7PoissonHR))+rnorm(length(essHighway7PoissonHR), 0, 0.025), 
       log10(essHighway7PoissonHR), 
       pch = 20)
dev.off()

# 3x4x6 (Uniform)

pdf("../figs/supMatFig5.pdf", width = 8, height = 5)
par(mfrow = c(1,1))
boxplot(log10(cbind(ess3x4x6UniformFMB,
                    ess3x4x6Uniform1MB,
                    ess3x4x6UniformHR)),
        xlab = "Sampler",
        names = c("FMB", "1MB", "HR"),
        ylab = expression(log[10](ESS)),
        outcol = "#00000000")
points(rep(1, length(ess3x4x6UniformFMB))+rnorm(length(ess3x4x6UniformFMB), 0, 0.025), 
       log10(ess3x4x6UniformFMB), 
       pch = 20)
points(rep(2, length(ess3x4x6Uniform1MB))+rnorm(length(ess3x4x6Uniform1MB), 0, 0.025), 
       log10(ess3x4x6Uniform1MB), 
       pch = 20)
points(rep(3, length(ess3x4x6UniformHR))+rnorm(length(ess3x4x6UniformHR), 0, 0.025), 
       log10(ess3x4x6UniformHR), 
       pch = 20)
dev.off()

# 3x4x6 (Poisson)

pdf("../figs/supMatFig6.pdf", width = 8, height = 5)
par(mfrow = c(1,1))
boxplot(log10(cbind(ess3x4x6PoissonFMB,
                    ess3x4x6Poisson1MB,
                    ess3x4x6PoissonHR)),
        xlab = "Sampler",
        names = c("FMB", "1MB", "HR"),
        ylab = expression(log[10](ESS)),
        outcol = "#00000000")
points(rep(1, length(ess3x4x6PoissonFMB))+rnorm(length(ess3x4x6PoissonFMB), 0, 0.025), 
       log10(ess3x4x6PoissonFMB), 
       pch = 20)
points(rep(2, length(ess3x4x6Poisson1MB))+rnorm(length(ess3x4x6Poisson1MB), 0, 0.025), 
       log10(ess3x4x6Poisson1MB), 
       pch = 20)
points(rep(3, length(ess3x4x6PoissonHR))+rnorm(length(ess3x4x6PoissonHR), 0, 0.025), 
       log10(ess3x4x6PoissonHR), 
       pch = 20)
dev.off()

# 3x4x12 (Poisson)

pdf("../figs/supMatFig7.pdf", width = 6, height = 5)
par(mfrow = c(1,1))
boxplot(log10(cbind(ess3x4x12Poisson1MB,
                    ess3x4x12PoissonHR)),
        xlab = "Sampler",
        names = c("1MB", "HR"),
        ylab = expression(log[10](ESS)),
        outcol = "#00000000")
points(rep(1, length(ess3x4x12Poisson1MB))+rnorm(length(ess3x4x12Poisson1MB), 0, 0.025), 
       log10(ess3x4x12Poisson1MB), 
       pch = 20)
points(rep(2, length(ess3x4x12PoissonHR))+rnorm(length(ess3x4x12PoissonHR), 0, 0.025), 
       log10(ess3x4x12PoissonHR), 
       pch = 20)
dev.off()

# 3x4x20 (Uniform)
pdf("../figs/supMatFig8.pdf", width = 6, height = 5)
par(mfrow = c(1,1))
boxplot(log10(cbind(ess3x4x20Uniform1MB,
                    ess3x4x20UniformHR)),
        xlab = "Sampler",
        names = c("1MB", "HR"),
        ylab = expression(log[10](ESS)),
        outcol = "#00000000")
points(rep(1, length(ess3x4x20Uniform1MB))+rnorm(length(ess3x4x20Uniform1MB), 0, 0.025), 
       log10(ess3x4x20Uniform1MB), 
       pch = 20)
points(rep(2, length(ess3x4x20UniformHR))+rnorm(length(ess3x4x20UniformHR), 0, 0.025), 
       log10(ess3x4x20UniformHR), 
       pch = 20)
dev.off()
