#### Setup ####
A = extensionSamplers::contingencyTable3x4x6$A

lambda = extensionSamplers::contingencyTable3x4x6$lambda

y = extensionSamplers::contingencyTable3x4x6$y

nSample = 1e+06
nBurnin = 2e+05
thinning = 10

#### 4ti2 Basis ####

FMB = extensionSamplers::contingencyTable3x4x6$FMB

#### Hyperrectangle partition ####

A2Idx = extensionSamplers::contingencyTable3x4x6$A2IdxHR

#### 1-Markov basis (basic moves) ####

B = extensionSamplers::contingencyTable3x4x6$B

#### Poisson Samplers ####

set.seed(2027)
samplesPoissonFMB <- extensionSamplers::sampler(A = A,
                                                y = y,
                                                B = FMB,
                                                Model = "poisson",
                                                lambda = lambda,
                                                extension = "none",
                                                nChains = 4,
                                                nSample = nSample,
                                                nBurnin = nBurnin,
                                                thinning = thinning,
                                                includeBurnin = FALSE)

write.csv(samplesPoissonFMB, "3x4x6_poisson_FMB.csv", row.names = FALSE)

set.seed(2027)
samplesPoisson1MB <- extensionSamplers::sampler(A = A,
                                                y = y,
                                                B = B,
                                                Model = "poisson",
                                                lambda = lambda,
                                                extension = "p",
                                                p = 1,
                                                loggamma = 0,
                                                w = 7,
                                                nChains = 4,
                                                nSample = nSample,
                                                nBurnin = nBurnin,
                                                thinning = thinning,
                                                includeBurnin = FALSE)

write.csv(samplesPoisson1MB, "3x4x6_poisson_1MB.csv", row.names = FALSE)

positiveProportion1MB <- sum(complete.cases(extensionSamplers::onlyPositive(samplesPoisson1MB)))/(4*(nSample/thinning))

set.seed(2027)
samplesPoissonHR <- extensionSamplers::sampler(A = A,
                                               y = y,
                                               Model = "poisson",
                                               lambda = lambda,
                                               extension = "hyperrectangle",
                                               A2Idx = A2Idx,
                                               loggamma = 0,
                                               w = 7,
                                               nChains = 4,
                                               nBurnin = nBurnin,
                                               nSample = nSample,
                                               thinning = thinning,
                                               includeBurnin = FALSE)

write.csv(samplesPoissonHR, "3x4x6_poisson_HR.csv", row.names = FALSE)
positiveProportionHR <- sum(complete.cases(extensionSamplers::onlyPositive(samplesPoissonHR)))/(4*(nSample/thinning))

#### Uniform Samplers ####

set.seed(2027)
samplesUniformFMB <- extensionSamplers::sampler(A = A,
                                                y = y,
                                                B = FMB,
                                                Model = "uniform",
                                                extension = "none",
                                                nChains = 4,
                                                nSample = nSample,
                                                nBurnin = nBurnin,
                                                thinning = thinning,
                                                includeBurnin = FALSE)

write.csv(samplesUniformFMB, "3x4x6_uniform_FMB.csv", row.names = FALSE)

set.seed(2027)
samplesUniform1MB <- extensionSamplers::sampler(A = A,
                                                y = y,
                                                B = B,
                                                Model = "uniform",
                                                extension = "p",
                                                p = 1,
                                                loggamma = 0,
                                                w = 4,
                                                nChains = 4,
                                                nSample = nSample,
                                                nBurnin = nBurnin,
                                                thinning = thinning,
                                                includeBurnin = FALSE)

write.csv(samplesUniform1MB, "3x4x6_uniform_1MB.csv", row.names = FALSE)

positiveProportion1MB <- sum(complete.cases(extensionSamplers::onlyPositive(samplesUniform1MB)))/(4*(nSample/thinning))

set.seed(2027)
samplesUniformHR <- extensionSamplers::sampler(A = A,
                                               y = y,
                                               Model = "uniform",
                                               extension = "hyperrectangle",
                                               A2Idx = A2Idx,
                                               loggamma = 0,
                                               w = 4,
                                               nChains = 4,
                                               nBurnin = nBurnin,
                                               nSample = nSample,
                                               thinning = thinning,
                                               includeBurnin = FALSE)

write.csv(samplesUniformHR, "3x4x6_uniform_HR.csv", row.names = FALSE)
positiveProportionHR <- sum(complete.cases(extensionSamplers::onlyPositive(samplesUniformHR)))/(4*(nSample/thinning))
