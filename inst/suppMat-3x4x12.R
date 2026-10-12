#### Setup ####
A = extensionSamplers::contingencyTable3x4x12$A

lambda = extensionSamplers::contingencyTable3x4x12$lambda

y = extensionSamplers::contingencyTable3x4x12$y

nSample = 1e+07
nBurnin = 2e+06
thinning = 100

#### Hyperrectangle partition ####

A2Idx = extensionSamplers::contingencyTable3x4x12$A2IdxHR

#### 1-Markov basis (basic moves) ####

B = extensionSamplers::contingencyTable3x4x12$B

#### Poisson Samplers ####

set.seed(2027)
samplesPoisson1MB <- extensionSamplers::sampler(A = A,
                                                y = y,
                                                B = B,
                                                Model = "poisson",
                                                lambda = lambda,
                                                extension = "p",
                                                p = 1,
                                                loggamma = 0,
                                                w = 8,
                                                nChains = 4,
                                                nSample = nSample,
                                                nBurnin = nBurnin,
                                                thinning = thinning,
                                                includeBurnin = FALSE)

write.csv(samplesPoisson1MB, "3x4x12_poisson_1MB.csv", row.names = FALSE)

positiveProportion1MB <- sum(complete.cases(extensionSamplers::onlyPositive(samplesPoisson1MB)))/(4*(nSample/thinning))

set.seed(2027)
samplesPoissonHR <- extensionSamplers::sampler(A = A,
                                               y = y,
                                               Model = "poisson",
                                               lambda = lambda,
                                               extension = "hyperrectangle",
                                               A2Idx = A2Idx,
                                               loggamma = 0,
                                               w = 4,
                                               nChains = 4,
                                               nBurnin = nBurnin,
                                               nSample = nSample,
                                               thinning = thinning,
                                               includeBurnin = FALSE)

write.csv(samplesPoissonHR, "3x4x12_poisson_HR.csv", row.names = FALSE)
positiveProportionHR <- sum(complete.cases(extensionSamplers::onlyPositive(samplesPoissonHR)))/(4*(nSample/thinning))