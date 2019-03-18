rm(list = ls())

INSTALLED <- require(rprime)
if (!INSTALLED) install.package('rprime')
source("../getEdataValues.r")

# Definitions ------------------------------------------------------------------
extractSubjNum <- function(x)
  # Extract subject number from edata files
  sapply(strsplit(x, split = '-'), function(y) as.integer(y[2]))

convEdata <- function(subj, run, EDATA)
{

  # Echo
  message("Now processing: ")
  message("Subject: ", subj, "... ...")
  message("Run: ", run, "... ...")

  edata <- EDATA

  # Import and convert edata
  r <- to_data_frame(FrameList(read_eprime(edata)))

  #############
  # WRITE CSV #
  #############
  write.csv(r, file = file.path("edata_csv", paste(subj, '_', run, '.csv', sep = '')))

  # Get onset times
  onset <- getEdataValues(
    r
    , namePattern = "(vstimimg|detectionpic|pinyintxt|gs|wr[0-9]*tt)[0-9]*\\.OnsetTime$"
  )

#  # Get offset times
#  offset <- getEdataValues(
#    r
#    , namePattern = "((detectionpic|vstimimg[0-9]*)\\.Offset|vblank[0-9]*\\.Onset|gs\\.Finish)Time$"
#    , valRange = c(as.integer(onset) + 200, 1e6)
#  )

  # Get durations
  duration <- getEdataValues(
    r
    , namePattern = "((vstimimg|wr[0-9]*tt|gs)[0-9]*\\.Duration|pinyintxt[0-9]*\\.OnsetToOnsetTime)$"
  )

  # Stack to dataframe
  rr <- data.frame(
    subject  = as.integer(subj)
    , run    = as.integer(run)
    , stim   = r$vstim
    , code   = as.integer(r$code)
    , onset  = as.integer(onset)
#    , offset = as.integer(offset)
    , dur    = duration
  )

  rr[!is.na(rr$stim), ]
}

# ------------------------------------------------------------------------------

# Find edata for run 1&2
EDATA.1 <- dir('./edata', pattern = '.*part1.*', full.name = T)
EDATA.2 <- dir('./edata', pattern = '.*part2.*', full.name = T)

# Coerce them into just one list
OUT <- append(
  lapply(EDATA.1, function(i) convEdata(extractSubjNum(i), 1, i)),
  lapply(EDATA.2, function(i) convEdata(extractSubjNum(i), 2, i))
)

# Convert list to dataframe
OUT.df <- do.call(rbind, OUT)

#############
# WRITE CSV #
#############
write.csv(OUT.df[!is.na(OUT.df$code), ], file = 'OUT.df.csv')


