test_that("sig_visualise runs without error", {
  path_test_decomp <- system.file("testfiles/tcga_test_decomp.csv", package = "sigvis")
  path_test_sig <- system.file("testfiles/test_sbs2.csv", package = "sigvis")

  df_test_decomp <- read.csv(path_test_decomp)
  df_test_sig <- read.csv(path_test_sig)

  signatures <- sigstash::sig_load('COSMIC_v3.3.1_SBS_GRCh38')
  df_test_model <- sigstats::sig_combine(signatures, model = c('SBS2' = 0.5, 'SBS13' = 0.5))

  expect_error(suppressMessages(sig_visualise(df_test_decomp, class = "decomposition")),regexp = NA)

  expect_error(suppressMessages(sig_visualise(df_test_sig, class = "signature")),regexp = NA)

  expect_error(suppressMessages(sig_visualise(df_test_model, class = "model")),regexp = NA)
})
