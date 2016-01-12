context("test the context of swirl_courses")

swirl_courses_repo <- "https://github.com/wush978/swirl_courses"

test_that("checkout the swirl_courses", {
  git2r::clone(swirl_courses_repo, local_path = "./swirl_courses")
  yaml_list <- dir("swirl_courses", pattern = "lesson.yaml", full.names = TRUE, recursive = TRUE)
  for(yaml_path in yaml_list) {
    set_lesson(yaml_path, open_lesson = FALSE, silent = TRUE)
    test_lesson()
  }
})