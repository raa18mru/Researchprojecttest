library(MGDrivE)



####################
# Output Folder
####################
outFolder <- "medfly_RIDL"
dir.create(path = outFolder)


# movement matrix for 1 node
moveMat <- matrix(data = 1, nrow = 1, ncol = 1)
moveMat


### EXAMPLE
####################
# Simulation Parameters
####################

# time in days
tMax <- 365
# nRep <- 10


# 
bioParameters <- list(betaK=20.35,tEgg=2.18,tLarva=7.12,tPupa=9.4,popGrowth=1.056,muAd=0.1)

# 
moveMat <- matrix(data = 1, nrow = 1, ncol = 1)

# 
adultPopEquilibrium <- 500
sitesNumber <- nrow(moveMat)

####################
# Basic Inheritance pattern
####################
# make a RIDL inheritance cube - print(cube) to investigate 
cube <- cubeRIDL()

# shows that Female viability is 1 or totally fine!
cube$xiF
# Set Heterozyvgous and Homozygous states to lethal
cube$xiF <- c(1,0,0)
#
####################
# Setup releases and batch migration
####################
# 
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
malereleasesVector <- generateReleaseVector(driveCube=cube,
                                        releasesParameters=releasesParameters)

femalereleasesVector <- generateReleaseVector(driveCube=cube,
                                            releasesParameters=releasesParameters)

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
setupMGDrivE(stochasticityON = FALSE, verbose = FALSE)



#
netPar <- parameterizeMGDrivE(runID=1, simTime=tMax, sampTime = 1, nPatch=sitesNumber,
                              beta=bioParameters$betaK, muAd=bioParameters$muAd,
                              popGrowth=bioParameters$popGrowth, tEgg=bioParameters$tEgg,
                              tLarva=bioParameters$tLarva, tPupa=bioParameters$tPupa,
                              AdPopEQ=adultPopEquilibrium, inheritanceCube = cube)

# build network prior to run
MGDrivESim <- Network$new(params=netPar,
                          driveCube=cube,
                          patchReleases=patchReleases,
                          migrationMale=moveMat,
                          migrationFemale=moveMat,
                          migrationBatch=batchMigration,
                          directory=outFolder,
                          verbose=FALSE)

# run simulation
MGDrivESim$oneRun(verbose = FALSE)


####################
# Post Analysis
####################
# First, split output by patch
# Second, aggregate females by their mate choice
splitOutput(readDir = outFolder, remFile = TRUE, verbose = FALSE)

# aggregate females by their mate choice
#  This reduces the female file to have the same columns as the male file
aggregateFemales(readDir = outFolder, genotypes = cube$genotypesID,
                 remFile = TRUE, verbose = FALSE)



# plot output to see effect
plotMGDrivESingle(readDir = outFolder, totalPop = TRUE, lwd = 3.5, alpha = 1)




