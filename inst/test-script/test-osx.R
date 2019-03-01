R.version <- sprintf("%s.%s", R.version$major, R.version$minor)
LESSON_PREFIX <- Sys.getenv("LESSON_PREFIX")
TEST_COURSE <- Sys.getenv("TEST_COURSE")
R_LIBS <- c(tempfile(), normalizePath(file.path("~", R.version)))
lapply(R_LIBS, dir.create, recursive = TRUE, showWarnings = FALSE)
.libPaths(new = R_LIBS)
Sys.setenv("R_LIBS" = paste(R_LIBS, collapse = .Platform$file.sep))

utils::install.packages("remotes", repos = "http://cran.csie.ntu.edu.tw", lib = R_LIBS[1])
utils::install.packages("pvm", repos = NULL, type = 'source', lib = R_LIBS[1])
pvm::metamran.update()
pvm::import.packages(sprintf("https://raw.githubusercontent.com/wush978/pvm-list/master/dsr-%s.yml", package_version(R.version)), lib = R_LIBS[2], repos = c(CRAN='http://cran.csie.ntu.edu.tw'))
utils::install.packages("swirl", repos = NULL, type = 'source', lib = R_LIBS[1])

R.date <- pvm::R.release.dates[R.version]

R_USER_LIBS <- normalizePath(file.path("~", "R-lib", TEST_COURSE, LESSON_PREFIX, R.version, R.date))
dir.create(R_USER_LIBS, showWarnings = FALSE, recursive = TRUE)
repos <- c(CRAN = sprintf("https://cran.microsoft.com/snapshot/%s", R.date + 7))
if (!suppressWarnings(require(subprocess))) install.packages("subprocess", repos = repos, lib = R_LIBS[2])
if (!suppressWarnings(require(magrittr))) install.packages("magrittr", repos = repos, lib = R_LIBS[2])
utils::install.packages("swirlify", repos = NULL, type = 'source', lib = R_LIBS[1])

Sys.setenv("SWIRL_DEV"="TRUE")
.libPaths(new = c(R_USER_LIBS, .libPaths()))
swirlify::test_lesson_by_agent(file.path("course", TEST_COURSE), LESSON_PREFIX, repos)
