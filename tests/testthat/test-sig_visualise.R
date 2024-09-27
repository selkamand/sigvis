test_that("sig_visualise runs without error", {
  path_test_catalogues <- system.file("testfiles/tcga_test_catalogues.csv", package = "sigvis")
  path_test_sig <- system.file("testfiles/test_sbs2.csv", package = "sigvis")

  df_test_catalogues <- read.csv(path_test_catalogues)
  df_test_sig <- read.csv(path_test_sig)

  signatures <- sigstash::sig_load('COSMIC_v3.3.1_SBS_GRCh38')
  df_test_model <- sigstats::sig_combine(signatures, model = c('SBS2' = 0.5, 'SBS13' = 0.5))

  expect_error(suppressMessages(sig_visualise(df_test_catalogues, class = "catalogue")),regexp = NA)

  expect_error(suppressMessages(sig_visualise(df_test_sig, class = "signature")),regexp = NA)

  expect_error(suppressMessages(sig_visualise(df_test_model, class = "model")),regexp = NA)
})



test_that("sig_visualise auto-colouring works with signatures", {
  df_catalogue <- data.frame(
    type = rep(c("0", "1", "2", "3-4", "5-8", "9+"), c(3L, 5L, 10L, 10L, 10L, 10L)),
    channel = c(
      "0:homdel:>1Mb", "0:homdel:0-100kb", "0:homdel:100kb-1Mb", "1:LOH:>40Mb",
      "1:LOH:0-100kb", "1:LOH:100kb-1Mb", "1:LOH:10Mb-40Mb", "1:LOH:1Mb-10Mb",
      "2:het:>40Mb", "2:het:0-100kb", "2:het:100kb-1Mb", "2:het:10Mb-40Mb",
      "2:het:1Mb-10Mb", "2:LOH:>40Mb", "2:LOH:0-100kb", "2:LOH:100kb-1Mb",
      "2:LOH:10Mb-40Mb", "2:LOH:1Mb-10Mb", "3-4:het:>40Mb", "3-4:het:0-100kb",
      "3-4:het:100kb-1Mb", "3-4:het:10Mb-40Mb", "3-4:het:1Mb-10Mb", "3-4:LOH:>40Mb",
      "3-4:LOH:0-100kb", "3-4:LOH:100kb-1Mb", "3-4:LOH:10Mb-40Mb",
      "3-4:LOH:1Mb-10Mb", "5-8:het:>40Mb", "5-8:het:0-100kb", "5-8:het:100kb-1Mb",
      "5-8:het:10Mb-40Mb", "5-8:het:1Mb-10Mb", "5-8:LOH:>40Mb", "5-8:LOH:0-100kb",
      "5-8:LOH:100kb-1Mb", "5-8:LOH:10Mb-40Mb", "5-8:LOH:1Mb-10Mb", "9+:het:>40Mb",
      "9+:het:0-100kb", "9+:het:100kb-1Mb", "9+:het:10Mb-40Mb", "9+:het:1Mb-10Mb",
      "9+:LOH:>40Mb", "9+:LOH:0-100kb", "9+:LOH:100kb-1Mb", "9+:LOH:10Mb-40Mb",
      "9+:LOH:1Mb-10Mb"
    ),
    fraction = c(
      0.00128135693268892, 0.000953990025032259, 0.00188757614064117,
      2.32982332194122e-05, 0.00135952739494608, 0.00317780167677141,
      0.00227993423276864, 0.00762969426467821, 2.96638292452979e-07,
      0.000351008982856614, 0.000340433982403718, 0.000319537295431936,
      0.000452628561737111, 0.0153252148477545, 0.000678326969750336,
      0.00381218548775498, 0.0448115054113704, 0.0397708733493721,
      0.0716785180378961, 0.000301724284591399, 0.00353531236474442,
      0.0997310031643665, 0.0843363061030153, 0.00408431252574583,
      0.00156242376835983, 0.0119904785651645, 0.0225892611730991,
      0.0399668231635854, 0.00808885541308866, 0.000512041186964217,
      0.00706909867621894, 0.0266641105955516, 0.054923000819516,
      0.00184802533978704, 0.000821285385280166, 0.00351898687947765,
      0.00250753172580081, 0.00813461055447359, 0.000384527777718085,
      0.000203009105584771, 0.000623288757674539, 0.000398593207716688,
      0.00074748092098747, 0.000130630475100971, 0.000144904110399013,
      0.000143541309630001, 8.53200973575284e-05, 0.000157253403241923
    )
  )

  sm(expect_no_message(sig_visualise(df_catalogue), message = "No exact .* match"))
})


test_that("sig_visualise auto-colouring/sorting works on all sigvis collections", {

  # Get all datasets
  datasets <- sigstash::sig_available()[["dataset"]]

  # Visualise each dataset
  for (dataset in datasets){
    collection <- sigstash::sig_load(dataset = dataset)
    signature <- collection[[1]]
    sm(expect_no_message(sig_visualise(signature), message = "No exact .* match"))
  }

})
