test_that("sig visualise dotplot works", {
  data <- example_dotplot_data()
  sig_visualise_dotplot(data)

  # Runs without errors
  expect_no_error(
    print(sig_visualise_dotplot(
      data,
      col_fill = "sample_info",
      palette_fill = c("sample_of_interest" = "red", "cancer_type" = "blue", "other" = "grey"),
      sort_by = "frequency_fill"
  )))

  expect_s3_class(
    sig_visualise_dotplot(
      data,
      col_fill = "sample_info",
      palette_fill = c("sample_of_interest" = "red", "cancer_type" = "blue", "other" = "grey"),
      sort_by = "frequency_fill"
    ),
    class = "gg"
  )
})
