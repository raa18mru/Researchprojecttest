library(MGDrivE)
rm(list=ls())

####################
# Output Folder
####################
# Simple start, assigning the folder for data to be output
outFolder <- "output/Deposition_investigation"
# dir.create(path = outFolder)
aggFolder <- "output/Deposition_investigation/aggFolder"
# dir.create(path = aggFolder)

# Clears the previous run CSVs
unlink("output/Deposition_investigation/*")
unlink("output/Deposition_investigation/aggFolder/*")

####################
# Simulation Parameters
####################

# time in days
tMax <- 425

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
# W: Wild-type allele # H: Homing  # B: Broken
# Assign sex ratio of offspring to simulate sex switching gene

female_deposition_cutting <- list(0,0.2,0.4,0.6,0.8,1)
run <- list(1,2,3,4,5,6)
newphi <- c("HHBB" = 0, "WHBB" = 0)

for (i in 1:length(female_deposition_cutting)) {
  # establish inheritance cube
  tarecube <- cubeClvR(
    cF = 1,
    crF = 0,
    ccF = 1,
    ccrF = 0,
    cM = 1,
    crM = 0,
    ccM = 1,
    ccrM = 0,
    dW = female_deposition_cutting[[i]],
    drW = 0,
    ddW = female_deposition_cutting[[i]],
    ddrW = 0,
    phi = newphi,
    omega = NULL,
    xiF = NULL,
    xiM = NULL,
    s = NULL)
  
  
  
  #
  ####################
  # Setup releases and batch migration
  ####################
  
  patchReleases <- replicate(n=sitesNumber,
                             expr={list(maleReleases=NULL,femaleReleases=NULL,
                                        eggReleases=NULL,matedFemaleReleases=NULL)},
                             simplify=FALSE)
  
  # choose release parameters
  releasesParameters <- list(releasesStart=60,
                             releasesNumber=15,
                             releasesInterval=15,
                             releaseProportion=400)
  
  
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
  netPar <- parameterizeMGDrivE(runID=run[[i]], simTime=tMax, sampTime = 1, nPatch=sitesNumber,
                                beta=bioParameters$betaK, muAd=bioParameters$muAd,
                                popGrowth=bioParameters$popGrowth, tEgg=bioParameters$tEgg,
                                tLarva=bioParameters$tLarva, tPupa=bioParameters$tPupa,
                                AdPopEQ=adultPopEquilibrium, inheritanceCube = tarecube)
  
  # prints a number every time it completes a loop
  if(i %% 1==0){print(i)}
  
  # build network prior to run
  MGDrivESim <- Network$new(params=netPar,
                            driveCube=tarecube,
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
  # split output by patch
  #  Required for plotting later
  splitOutput(readDir = outFolder, remFile = TRUE, verbose = FALSE)
  
  # aggregate females by their mate choice
  #  This reduces the female file to have the same columns as the male file
  aggregateFemales(readDir = outFolder, genotypes = tarecube$genotypesID,
                   remFile = TRUE, verbose = FALSE, writeDir=aggFolder)
  unlink("output/Deposition_investigation/*")
}

##############