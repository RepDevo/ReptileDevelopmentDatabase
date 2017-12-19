
# Run tests to check the data base for structural integrity, errors etc.
database <- read.csv("./Database.csv")

# Check columns to make sure the correct levels have been entered.
test_that("Sex column contains incorrect levels", {
	levels <- sort(c("mixed", "females", "males"))
	expect_true(setequal(levels, sort(unique(database$sex))))
})


test_that("Trait category column contains incorrect levels", {
	levels <- sort(c("Morphology", "Sex", "Physiology", "Survival", "Incubation", "Physiology", "Performance"))
	expect_true(setequal(levels, sort(unique(database$trait_cat))))
})

# Check that the correct years and paper range have been entered
test_that("Years go beyond the last search date", {
	version_year <- 2016
	expect_true(version_year == range(database$pub_year)[2])
})


test_that("Years go beyond the last search date", {
	version_papers <- 1662
	expect_true(version_papers == range(database$data_no)[2])
})


# Check the structural integrity of the database

# Note that this will break as soon as rows change order...example entering new, older data. 
test_that("Check structural integrity of temperatures...", {
	expect_equal(database$T[1], 28, info = "Row 1 checked")
	expect_equal(database$T[30], 31, info = "Row 30 checked")
	expect_equal(database$T[113], 17.9, info = "Row 113 checked")
	expect_equal(database$T[2317], 24, info = "Row 2317 checked")
	expect_equal(database$T[5954], 27, info = "Row 5954 checked")
	expect_equal(database$T[7206], 30, info = "Row 7206 checked")
	expect_equal(database$T[7448], 34.5, info = "Row 7448 checked")
	expect_equal(database$T[7982], 27, info = "Row 7982 checked")
})