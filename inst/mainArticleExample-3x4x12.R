#### Setup ####

A = extensionSamplers::contingencyTable3x4x12$A
x = extensionSamplers::contingencyTable3x4x12$x
y = A%*%x
B = extensionSamplers::contingencyTable3x4x12$B


#### 1MB Sampler ####

system.time({
  set.seed(2027)
  samples_burnin <- extensionSamplers::sampler(A = A,
                                y = y,
                                B = B,
                                Model = "uniform",
                                extension = "p",
                                p = 1,
                                w = 5,
                                loggamma = 0,
                                nChains = 4,
                                nSample = 1e+07,
                                nBurnin = 1e+06,
                                thinning = 100,
                                includeBurnin = TRUE)
})

samples <- samples_burnin[c((1e+04+1):(1e+04+1e+05),
                            (2e+04+1+1e+05):(2e+04+2e+05),
                            (3e+04+1+2e+05):(3e+04+3e+05),
                            (4e+04+1+3e+05):(4e+04+4e+05)),]

samples[,ncol(A)+2] <- rep(1:1e+05, 4)
samples[,ncol(A)+3] <- 1:4e+05

samples_pos <- extensionSamplers::onlyPositive(samples)

(positive_portion <- sum(complete.cases(samples_pos))/4e+05)
write.csv(samples_burnin, "samples.csv", row.names = FALSE)

sample_ess_3x4x12_uniform <- posterior::summarise_draws(extensionSamplers::to_draws_df(samples), "ess_bulk")$ess_bulk*positive_portion
#mESS_3x4x12_uniform <- extensionSamplers::computeMESS(samples, A)

#### Hyperrectangle ####

A2Idx = extensionSamplers::contingencyTable3x4x12$A2IdxHR

system.time({
  set.seed(2027)
  samples_HR_burnin <- extensionSamplers::sampler(A = A,
                                                  y = y,
                                                  A2Idx = A2Idx,
                                                  Model = "uniform",
                                                  extension = "hyperrectangle",
                                                  w = 5,
                                                  loggamma = 0,
                                                  nChains = 4,
                                                  nSample = 1e+07,
                                                  nBurnin = 1e+06,
                                                  thinning = 100,
                                                  includeBurnin = TRUE)
})

samples_HR <- samples_HR_burnin[c((1e+04+1):(1e+04+1e+05),
                            (2e+04+1+1e+05):(2e+04+2e+05),
                            (3e+04+1+2e+05):(3e+04+3e+05),
                            (4e+04+1+3e+05):(4e+04+4e+05)),]

samples_HR[,ncol(A)+2] <- rep(1:1e+05, 4)
samples_HR[,ncol(A)+3] <- 1:4e+05

samples_HR_pos <- extensionSamplers::onlyPositive(samples_HR)

(positive_portion_HR <- sum(complete.cases(samples_HR_pos))/4e+05)
write.csv(samples_HR_burnin, "samples.csv", row.names = FALSE)

sample_ess_3x4x12_uniform_HR <- posterior::summarise_draws(extensionSamplers::to_draws_df(samples_HR), "ess_bulk")$ess_bulk*positive_portion_HR

#### Plots ####

pdf("fig7-3.pdf", width = 6, height = 5)
boxplot(log(cbind(sample_ess_3x4x12_uniform,
                  sample_ess_3x4x12_uniform_HR))/log(10),
        ylab = expression(log[10](ESS)),
        outcol = scales::alpha(1, 0),
        names = c("1-Extension", "Hyperrectangle"))
points(x = rnorm(ncol(A), 1, 0.05),
       y = log(sample_ess_3x4x12_uniform)/log(10),
       pch = 20,
       col = scales::alpha(1, 0.2))

points(x = rnorm(ncol(A), 2, 0.05),
       y = log(sample_ess_3x4x12_uniform_HR)/log(10),
       pch = 20,
       col = scales::alpha(1, 0.2))
dev.off()

nBurnin = 1e+04
nSample = 1e+05
nExtra = 1e+04
i = 11

pdf(paste0("fig7-1.pdf"), width = 6, height = 5)
xRange = range(samples_burnin[c(1:(nBurnin+nExtra),
                                (nBurnin+nSample) + 1:(nBurnin+nExtra),
                                2*(nBurnin+nSample) + 1:(nBurnin+nExtra),
                                3*(nBurnin+nSample) + 1:(nBurnin+nExtra)),i])

extensionSamplers::traceplot(samples_burnin[c(1:(nBurnin+nExtra),
                                              (nBurnin+nSample) + 1:(nBurnin+nExtra),
                                              2*(nBurnin+nSample) + 1:(nBurnin+nExtra),
                                              3*(nBurnin+nSample) + 1:(nBurnin+nExtra)),], i, TRUE, 0.2, ylim = xRange)

rect(0, -100000, nBurnin, 100000, col = "#00000010", border = "#00000000")
dev.off()



pdf(paste0("fig7-2.pdf"), width = 6, height = 5)
xRange = range(samples_HR_burnin[c(100:(nBurnin+nExtra),
                                (nBurnin+nSample) + 100:(nBurnin+nExtra),
                                2*(nBurnin+nSample) + 100:(nBurnin+nExtra),
                                3*(nBurnin+nSample) + 100:(nBurnin+nExtra)),i])

extensionSamplers::traceplot(samples_HR_burnin[c(1:(nBurnin+nExtra),
                                              (nBurnin+nSample) + 1:(nBurnin+nExtra),
                                              2*(nBurnin+nSample) + 1:(nBurnin+nExtra),
                                              3*(nBurnin+nSample) + 1:(nBurnin+nExtra)),], i, TRUE, 0.2, ylim = xRange)

rect(0, -100000, nBurnin, 100000, col = "#00000010", border = "#00000000")
dev.off()

