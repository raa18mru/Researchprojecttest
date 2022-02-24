library(MGDrivE)
rm(list=ls())

####################
# Output Folder
####################
# Simple start, assigning the folder for data to be output
outFolder <- "medfly_tare_no_loop"
dir.create(path = outFolder)

####################
# Simulation Parameters
####################

# time in days
tMax <- 365

# biological parameters- time in each stage, betak (Female egg batch size of wild-type)
# muAd (Wild-type daily adult mortality (1/muAd is average wild-type lifespan))
bioParameters <- list(betaK=20.35,tEgg=2.18,tLarva=7.12,tPupa=9.4,popGrowth=1.056,muAd=0.1)

moveMat <- matrix(data = 1, nrow = 1, ncol = 1)

# Adult population size at equilibrium
adultPopEquilibrium <- 500
sitesNumber <- nrow(moveMat)

####################
# Basic Inheritance pattern
####################
# make a TARE inheritance cube - print(tarecube) to investigate  
# W: Wild-type allele # H:  # B:
# Assign sex ratio of offspring to simulate sex switching gene
newphi <- c("HHBB" = 0, "WHBB" = 0)


# establish inheritance cube, 80% female deposition cutting rate
tarecube <- cubeClvR(
  cF = 1,
  crF = 0,
  ccF = 1,
  ccrF = 0,
  cM = 1,
  crM = 0,
  ccM = 1,
  ccrM = 0,
  dW = 1,
  drW = 0,
  ddW = 1,
  ddrW = 0,
  hSuf = 1,
  eta = NULL,
  phi = newphi,
  omega = NULL,
  xiF = NULL,
  xiM = NULL,
  s = NULL
)


#
####################
# Setup releases and batch migration
####################

patchReleases <- replicate(n=sitesNumber,
                           expr={list(maleReleases=NULL,femaleReleases=NULL,
                                      eggReleases=NULL,matedFemaleReleases=NULL)},
                           simplify=FALSE)

# choose release parameters
releasesParameters <- list(releasesStart=1,
                           releasesNumber=10,
                           releasesInterval=10,
                           releaseProportion=200)

# generate release vector
malereleasesVector <- generateReleaseVector(driveCube=tarecube,
                                            releasesParameters=releasesParameters)

# femalereleasesVector <- generateReleaseVector(driveCube=cube,releasesParameters=releasesParameters)

# put releases into the proper place in the release list male only release
patchReleases[[1]]$maleReleases <- malereleasesVector
# patchReleases[[1]]$femaleReleases <- femalereleasesVector


# batch migration is disabled by setting the probability to 0
batchMigration <- basicBatchMigration(batchProbs=0,
                                      sexProbs=c(.5,.5),
                                      numPatches=sitesNumber)

####################
# Combine parameters and run!
####################
# set MGDrivE to run deterministic
setupMGDrivE(stochasticityON = FALSE, verbose = TRUE)



#
netPar <- parameterizeMGDrivE(runID=1, simTime=tMax, sampTime = 1, nPatch=sitesNumber,
                              beta=bioParameters$betaK, muAd=bioParameters$muAd,
                              popGrowth=bioParameters$popGrowth, tEgg=bioParameters$tEgg,
                              tLarva=bioParameters$tLarva, tPupa=bioParameters$tPupa,
                              AdPopEQ=adultPopEquilibrium, inheritanceCube = tarecube)

# build network prior to run
MGDrivESim <- Network$new(params=netPar,
                          driveCube=tarecube,
                          patchReleases=patchReleases,
                          migrationMale=moveMat,
                          migrationFemale=moveMat,
                          migrationBatch=batchMigration,
                          directory=outFolder,
                          verbose=TRUE)

# run simulation
MGDrivESim$oneRun(verbose = TRUE)


####################
# Post Analysis
####################
# First, split output by patch
# Second, aggregate females by their mate choice
splitOutput(readDir = outFolder, remFile = TRUE, verbose = FALSE)

# aggregate females by their mate choice
# This reduces the female file to have the same columns as the male file
aggregateFemales(readDir = outFolder, genotypes = tarecube$genotypesID,
                 remFile = TRUE, verbose = FALSE)



# plot output to see effect
plotMGDrivESingle(readDir = outFolder, totalPop = TRUE, lwd = 3.5, alpha = 1)




