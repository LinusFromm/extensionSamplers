#### Setup ####
A = extensionSamplers::contingencyTable3x4x20$A

x = extensionSamplers::contingencyTable3x4x20$x
y = A%*%x

nSample = 1e+07
nBurnin = 2e+06
thinning = 100

#### Hyperrectangle partition ####

A2Idx = extensionSamplers::contingencyTable3x4x20$A2IdxHR

#### 1-Markov basis (basic moves) ####

B = extensionSamplers::contingencyTable3x4x20$B

#### Uniform Samplers ####

set.seed(2027)
samplesUniform1MB <- extensionSamplers::sampler(A = A,
                                                y = y,
                                                B = B,
                                                Model = "uniform",
                                                extension = "p",
                                                p = 1,
                                                loggamma = 0,
                                                w = 6,
                                                nChains = 4,
                                                nSample = nSample,
                                                nBurnin = nBurnin,
                                                thinning = thinning,
                                                includeBurnin = FALSE)

write.csv(samplesUniform1MB, "3x4x20_uniform_1MB.csv", row.names = FALSE)

positiveProportion1MB <- sum(complete.cases(extensionSamplers::onlyPositive(samplesUniform1MB)))/(4*(nSample/thinning))

set.seed(2027)
samplesUniformHR <- extensionSamplers::sampler(A = A,
                                               y = y,
                                               Model = "uniform",
                                               extension = "hyperrectangle",
                                               A2Idx = A2Idx,
                                               loggamma = 0,
                                               w = 5.5,
                                               nChains = 4,
                                               nBurnin = nBurnin,
                                               nSample = nSample,
                                               thinning = thinning,
                                               includeBurnin = FALSE)

write.csv(samplesUniformHR, "3x4x20_uniform_HR.csv", row.names = FALSE)
positiveProportionHR <- sum(complete.cases(extensionSamplers::onlyPositive(samplesUniformHR)))/(4*(nSample/thinning))