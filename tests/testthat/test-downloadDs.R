test_that("modal save function works", {

  bucket_id <- "testuser"
  user_name <- "test"

  library(hgchmagic)
  element <- hgchmagic::hgch_bar_Cat(tibble(a = c("a","b")))
  name <- "dsmodules test"
  slug <- dspins::create_slug(name)
  type <- "dsviz"
  html_link <- paste0("https://",bucket_id,".dskt.ch/",user_name,"/", slug, "/", slug,".html")

  urls <- modalFunction_saveFile(element = element, type = type, user_name = user_name, bucket_id = bucket_id,
                                 name = name)

  expect_equal(urls$link, paste0("https://datasketch.co/",user_name,"/", slug))
  expect_equal(urls$permalink, html_link)
  expect_equal(urls$iframe_embed, paste0("<iframe src=\"",html_link,"\" frameborder=0 width=\"100%\" height=\"400px\"></iframe>"))

  expect_error(modalFunction_saveFile(type = type, user_name = user_name), "Need element to save.")
  expect_error(modalFunction_saveFile(element = element, user_name = user_name), "Need type to save file.")
  expect_error(modalFunction_saveFile(element = element, type = type), "Need user_name or org_name to save file.")
  expect_error(modalFunction_saveFile(element = element, type = "blah", user_name = user_name), "Element must be of type 'fringe', 'dsviz', or 'drop'.")

})
