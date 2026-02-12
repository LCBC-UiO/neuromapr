library(vcr)

vcr_configure(
  dir = "../fixtures/vcr",
  record = "once"
)
