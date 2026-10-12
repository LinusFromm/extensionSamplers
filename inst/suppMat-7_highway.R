#### Setup ####
A = extensionSamplers::highway7$A

lambda = extensionSamplers::highway7$lambda

y = extensionSamplers::highway7$y

nSample = 1e+06
nBurnin = 2e+05
thinning = 10

#### 4ti2 Basis ####

FMB = extensionSamplers::highway7$FMB

#### Lexicographic Basis ####

A2Idx = extensionSamplers::highway7$A2IdxLB
LB = extensionSamplers::highway7$LB

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

write.csv(samplesPoissonFMB, "7-highway_poisson_FMB.csv", row.names = FALSE)

set.seed(2027)
samplesPoisson2MB <- extensionSamplers::sampler(A = A,
                                                y = y,
                                                B = LB,
                                                Model = "poisson",
                                                lambda = lambda,
                                                extension = "p",
                                                p = ifelse(1:28 %in% A2Idx, 0, 2),
                                                loggamma = 0,
                                                w = 1.3,
                                                nChains = 4,
                                                nSample = nSample,
                                                nBurnin = nBurnin,
                                                thinning = thinning,
                                                includeBurnin = FALSE)

write.csv(samplesPoisson2MB, "7-highway_poisson_2MB.csv", row.names = FALSE)

positiveProportion2MB <- sum(complete.cases(extensionSamplers::onlyPositive(samplesPoisson2MB)))/(4*(nSample/thinning))

set.seed(2027)
samplesPoissonHR <- extensionSamplers::sampler(A = A,
                                               y = y,
                                               Model = "poisson",
                                               lambda = lambda,
                                               extension = "hyperrectangle",
                                               A2Idx = A2Idx,
                                               loggamma = 0,
                                               w = 1.4,
                                               nChains = 4,
                                               nBurnin = nBurnin,
                                               nSample = nSample,
                                               thinning = thinning,
                                               includeBurnin = FALSE)

write.csv(samplesPoissonHR, "7-highway_poisson_HR.csv", row.names = FALSE)
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

write.csv(samplesUniformFMB, "7-highway_uniform_FMB.csv", row.names = FALSE)

set.seed(2027)
samplesUniform2MB <- extensionSamplers::sampler(A = A,
                                                y = y,
                                                B = LB,
                                                Model = "uniform",
                                                extension = "p",
                                                p = ifelse(1:28 %in% A2Idx, 0, 2),
                                                loggamma = 0,
                                                w = 0,
                                                nChains = 4,
                                                nSample = nSample,
                                                nBurnin = nBurnin,
                                                thinning = thinning,
                                                includeBurnin = FALSE)

write.csv(samplesUniform2MB, "7-highway_uniform_2MB.csv", row.names = FALSE)

positiveProportion2MB <- sum(complete.cases(extensionSamplers::onlyPositive(samplesUniform2MB)))/(4*(nSample/thinning))

set.seed(2027)
samplesUniformHR <- extensionSamplers::sampler(A = A,
                                               y = y,
                                               Model = "uniform",
                                               extension = "hyperrectangle",
                                               A2Idx = A2Idx,
                                               loggamma = 0,
                                               w = 0.15,
                                               nChains = 4,
                                               nBurnin = nBurnin,
                                               nSample = nSample,
                                               thinning = thinning,
                                               includeBurnin = FALSE)

write.csv(samplesUniformHR, "7-highway_uniform_HR.csv", row.names = FALSE)
positiveProportionHR <- sum(complete.cases(extensionSamplers::onlyPositive(samplesUniformHR)))/(4*(nSample/thinning))
