
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


test_that("Error type column contains incorrect levels", {
	levels <- sort(c("SE", "CI", "P", "NA", "SD"))
	expect_true(setequal(levels, sort(unique(database$error_type))))
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
	expect_equal(database$T[8006], 28, info = "Row 8006 checked")
	expect_equal(database$T[8138], 26.5, info = "Row 8138 checked")
	expect_equal(database$T[8138], 26.5, info = "Row 8138 checked")
	expect_equal(database$T[8419], 24, info = "Row 8419 checked")
	expect_equal(database$T[8497], 32, info = "Row 8497 checked")
	expect_equal(database$T[8777], 25.5, info = "Row 8777 checked")
	expect_equal(database$T[9091], 33, info = "Row 9091 checked")
	expect_equal(database$T[9390], 33, info = "Row 9390 checked")
	expect_equal(database$T[9705], 30, info = "Row 9705 checked")
	expect_equal(database$T[9757], 29.5, info = "Row 9757 checked")
})