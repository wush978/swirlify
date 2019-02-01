#' Run tests on a lesson
#'
#' Run basic tests on all questions in the current lesson.
#'
#' @export
#' @examples
#' \dontrun{
#' # Set a lesson interactively
#' set_lesson()
#' 
#' # Run tests on that lesson
#' test_lesson()
#' }
test_lesson <- function(){
  lesson_file_check()
  test_lesson_by_name()
}

#' Run tests on a course
#'
#' Run basic tests on all questions in the lessons listed in the \code{MANIFEST}.
#' See \code{\link{add_to_manifest}} for information about the \code{MANIFEST}
#' file.
#'
#' @export
#' @examples
#' \dontrun{
#' # To test a course, set any lesson in that course as the current lesson
#' set_lesson()
#' 
#' # Run tests on every lesson in that course listed in the MANIFEST
#' test_course()
#' }
test_course <- function(){
  lesson_file_check()
  
  if(!any(file.exists(file.path(getOption("swirlify_course_dir_path"), c("LICENSE.txt", "LICENSE", "LICENCE.txt", "LICENCE"))))){
    message("It seems this course does not contian a LICENSE.txt file.\nYou can easily add a license with add_license().\n")
  }
  
  manifest_path <- file.path(getOption("swirlify_course_dir_path"), "MANIFEST")
  if(!file.exists(manifest_path)){
    stop("It seems there's no MANIFEST file for this course.\nPlease add one using add_to_manifest().")
  }
  yaml_list <- file.path(getOption("swirlify_course_dir_path"), 
                         readLines(manifest_path), "lesson.yaml")
  for(lesson in yaml_list){
    # Check to see if `lesson.yaml` or just `lesson` exists
    # If neither exists warn the user and go on to the next lesson
    if(!file.exists(lesson)){
      lesson <- sub(".yaml$", "", lesson)
      if(!file.exists(lesson)){
        message("Could not find expected lesson file:\n", lesson, ".yaml")
        next()
      }
    }
    
    set_lesson(path_to_yaml = lesson, open_lesson = FALSE, silent = TRUE)
    test_lesson_by_name()
  }
}

# Test all cmd and mult questions of any lesson of current course.
#' @importFrom yaml yaml.load_file
test_lesson_by_name <- function(){
  message(paste("##### Begin testing:", getOption("swirlify_lesson_name"), "#####"))
#   .e <- environment(swirl:::any_of_exprs)
#   attach(.e)
#   on.exit(detach(.e))
#   e <- new.env()
  
  course_dir_path <- getOption("swirlify_course_dir_path")
  lesson_dir_path <- getOption("swirlify_lesson_dir_path")
  les <- yaml.load_file(getOption("swirlify_lesson_file_path"))
  
#   for (R_file in c("customTests.R", "initLesson.R")){
#     R_file_path <- file.path(lesson_dir_path, R_file)
#     if(file.exists(R_file_path)) source(R_file_path,local = e)
#   }
  
  qn <- 0 # question number
  scripts_warned_already <- FALSE
  for (question in les){
    if(question$Class == "meta"){
      for(i in c("Course", "Lesson", "Author", "Type", "Version")){
        if(is.null(question[[i]])) message("Please provide a value for the ",
                                                  i, " key in the meta question.")
      }
    } else if(question$Class == "cmd_question"){
      for(i in c("Output", "CorrectAnswer", "AnswerTests", "Hint")){
        if(is.null(question[[i]])) message("Please provide a value for the ",
                                                  i, " key in question ", qn, ".")
      }
#       ne$val <- eval(parse(text=question$CorrectAnswer), envir = ne)
#       ne$expr <- parse(text = question$CorrectAnswer)[[1]]
#       if(!eval(parse(text=question$AnswerTests), envir = e)){
#         message("CorrectAnswer/AnswerTests mismatch in question ", qn,".")
#       }
    } else if(question$Class == "figure"){
      for(i in c("Output", "Figure", "FigureType")){
        if(is.null(question[[i]])) message("Please provide a value for the ",
                                                  i, " key in question ", qn, ".")
      }
      
      if(!is.null(question$Figure)){
        fig <- file.path(getOption("swirlify_lesson_dir_path"), question$Figure)
        if(!file.exists(fig)){
          message("Could not find figure-creating script '", question$Figure,
                  "'")
        }
      }
    } else if(question$Class == "text"){
      for(i in c("Output")){
        if(is.null(question[[i]])) message("Please provide a value for the ",
                                                  i, " key in question ", qn, ".")
      }
    } else if(question$Class == "mult_question"){
      for(i in c("Output", "AnswerChoices", "CorrectAnswer", "AnswerTests", "Hint")){
        if(is.null(question[[i]])) message("Please provide a value for the ",
                                                  i, " key in question ", qn, ".")
      }
#       e$val <- as.character(question$CorrectAnswer)
#       if(!eval(parse(text = question$AnswerTests), envir = e)){
#         message("CorrectAnswer/AnswerTests mismatch in question ", qn,".")
#       }
    } else if(question$Class == "exact_question"){
      for(i in c("Output", "CorrectAnswer", "AnswerTests", "Hint")){
        if(is.null(question[[i]])) message("Please provide a value for the ",
                                                  i, " key in question ", qn, ".")
      }
    } else if(question$Class == "script"){
      for(i in c("Output", "Script", "AnswerTests", "Hint")){
        if(is.null(question[[i]])) message("Please provide a value for the ",
                                                  i, " key in question ", qn, ".")
      }
      
      # Check for the existense of a scripts directory for this lesson
      scripts_path <- file.path(getOption("swirlify_lesson_dir_path"), "scripts")
      if(!file.exists(scripts_path) && !scripts_warned_already){
        message("Please create a directory called 'scripts' in the lesson
                directory for ", getOption("swirlify_lesson_name"), ".")
        scripts_warned_already <- TRUE
      }
      
      if(!is.null(question$Script) && !scripts_warned_already){
        script <- question$Script
        
        # Check for the existence of the script.R file.
        if(!file.exists(file.path(scripts_path, script))){
          message("Could not find script '", script, "' for lesson '", 
                  getOption("swirlify_lesson_name"), "' question number ",
                  qn, ".")
        }
        
        # Check for the existence of the script-correct.R file.
        script_correct <- paste0(sub(".R$", "", script), "-correct.R")
        if(!file.exists(file.path(scripts_path, script_correct))){
          message("Could not find script '", script_correct, "' for lesson '", 
                  getOption("swirlify_lesson_name"), "' question number ",
                  qn, ".")
        }
      }
    } else if(question$Class == "text_question"){
      for(i in c("Output", "CorrectAnswer", "AnswerTests", "Hint")){
        if(is.null(question[[i]])) message("Please provide a value for the ",
                                                  i, " key in question ", qn, ".")
      }
    } else if(question$Class == "video"){
      for(i in c("Output", "VideoLink")){
        if(is.null(question[[i]])) message("Please provide a value for the ",
                                                  i, " key in question ", qn, ".")
      }
    } else {
      message("Question ", qn, ": a question of class '", question$Class,
              "' is not officially supported by swirl.")
    }
    
    qn <- qn + 1
  }
  
  message("##### End testing: ", getOption("swirlify_lesson_name"), " #####\n")
}

#'@importFrom magrittr %>%
#'@importFrom magrittr extract
#'@importFrom magrittr extract2
#'@export
test_lesson_by_agent <- function(course.dir, lesson.name, repos = getOption("repos"), answer.yaml = NULL) {
  .env <- Sys.getenv()
  .env[["R_LIBS"]] <- paste(.libPaths(), collapse = ":")
  for(category in c(
    "LC_COLLATE", "LC_CTYPE",
    "LC_MONETARY", "LC_NUMERIC", "LC_TIME", "LC_MESSAGES",
    "LC_PAPER", "LC_MEASUREMENT")
  ) {
    .env[[category]] <- Sys.getlocale(category = category)
  }
  .env[["SWIRL_DEV"]] <- "TRUE"
  if (file.exists(file.path(R.home("bin"), "R"))) {
    p <- subprocess::spawn_process(
      file.path(R.home("bin"), "R"),
      c("--no-save", "--no-readline", "--quiet", "--interactive"),
      .env,
      workdir = getwd()
    )
  } else {
    p <- subprocess::spawn_process(
      file.path(R.home("bin"), "R.exe"),
      c("--no-save", "--ess"),
      .env,
      workdir = getwd()
    )
  }
  p.buf <- new.env()
  p.buf$output <- list()
  p.buf$show <- 0
  
  get_stdout <- function() {
    lapply(p.buf$output, "[[", "stdout") %>%
      unlist()
  }
  
  get_stderr <- function() {
    lapply(p.buf$output, "[[", "stderr") %>%
      unlist()
  }
  
  get_buf_size <- function() {
    length(p.buf$output)
  }
  
  search_output <- function(checker, is.output = TRUE) {
    lapply(p.buf$output, "[[", ifelse(is.output, "stdout", "stderr")) %>%
      sapply(checker) %>%
      which()
  }
  
  get_stderr <- function() {
    lapply(p.buf$output, "[[", "stderr") %>%
      unlist()
  }
  
  show <- function() {
    if (p.buf$show < length(p.buf$output)) {
      p.buf$show <- p.buf$show + 1
      cat(sprintf("(%d)---stdout---\n", p.buf$show))
      cat(p.buf$output[[p.buf$show]]$stdout, sep = "\n")
      cat(sprintf("(%d)---stderr---\n", p.buf$show))
      cat(p.buf$output[[p.buf$show]]$stderr, sep = "\n")
    }
  }
  
  read <- function() {
    p.buf$output[[length(p.buf$output) + 1]] <- subprocess::process_read(p)
    show()
  }
  
  wait_until <- function(checker, is.stdout = TRUE, check.last = TRUE, current.index = get_buf_size()) {
    if (check.last && current.index > 0) {
      if (is.stdout) {
        if (checker(p.buf$output[[current.index]]$stdout)) return(invisible(NULL))
      } else {
        if (checker(p.buf$output[[current.index]]$stderr)) return(invisible(NULL))
      }
    }
    retry <- 0
    colddown <- 0.1
    start.index <- current.index + 1
    while(TRUE) {
      Sys.sleep(colddown)
      read()
      end.index <- get_buf_size()
      for(i in start.index:end.index) {
        if (is.stdout) {
          if (checker(p.buf$output[[i]]$stdout)) return(invisible(NULL))
        } else {
          if (checker(p.buf$output[[i]]$stderr)) return(invisible(NULL))
        }
      }
      start.index <- end.index + 1
      retry <- retry + 1
      if (retry %% 5 == 0) enter_process("\n")
      if (retry > 600) stop(sprintf("wait_until timeout"))
      colddown <- min(colddown + 0.1, 1)
    }
  }
  
  search_selection_from_p.buf <- function(index, is.stdout, ans) {
    target.name <- ifelse(is.stdout, "stdout", "stderr")
    current.seq <- seq(index, by = -1, length.out = 1)
    while(TRUE) {
      . <- try({
        p.buf$output[current.seq] %>%
          lapply(function(.) {
            .[[target.name]]
          }) %>%
          do.call(what = c) %>%
          search_selection(ans)
      }, silent = TRUE)
      if (class(.) != "try-error") {
        return(.)
      } else {
        if (attr(., "condition")$message %>% as.integer() == 0) {
          current.seq <- seq(index, by = -1, length.out = length(current.seq) + 1)
          if (min(current.seq) == 0) break
        } else {
          break
        }
      }
    }
    stop(sprintf("Failed to `search_select(..., '%s')`", ans))
  }
  
  search_selection <- function(txt, ans) {
    for(char in c("\\", "(", ")", "^", "[", "]", "{", "}", ".", "$", "*", "+")) {
      ans <- gsub(char, sprintf("\\%s", char), ans, fixed = TRUE)
    }
    . <- regexec(sprintf("^\\s*(\\d+): %s$", ans), txt) %>%
      regmatches(x = txt) %>%
      Filter(f = function(.) length(.) == 2)
    if(length(.) != 1) {
      stop(paste(length(.)))
    }
    .[[1]][2]
  }
  
  enter_process <- function(cmd, breakline = TRUE) {
    if (breakline) {
      if (substring(cmd, nchar(cmd), nchar(cmd)) != "\n") {
        cmd <- sprintf("%s\n", cmd)
      }
    }
    subprocess::process_write(p, cmd)
    cat(sprintf("(process_write)> %s\n", cmd))
    Sys.sleep(0.1)
    read()
    show()
    invisible(NULL)
  }
  
  get_character <- function(expr.txt) {
    cmd <- sprintf("cat(sprintf('output:%%s:\\n', %s))", expr.txt)
    enter_process(cmd, breakline = TRUE)
    wait_until(function(.) any(grepl("output:", ., fixed = TRUE)))
    . <- lapply(p.buf$output, "[[", "stdout") %>%
      grep(pattern = "output:", fixed = TRUE) %>%
      max()
    . <- p.buf$output[[.]]$stdout
    m <- regexec("output:(.*):$", .)
    regmatches(., m) %>%
      Filter(f = function(.) length(.) == 2) %>%
      extract2(1) %>%
      extract(2)
  }
  
  enter_swirl <- function() {
    enter_process(sprintf("options(repos=c(CRAN='%s'))\n", repos))
    enter_process("options(editor = function(...){}, browser = function(...){})\n")
    enter_process(". <- as.environment('package:utils')")
    enter_process("unlockBinding('View', .)")
    enter_process("assign('View', function(x, title) NULL, envir = .)")
    enter_process("lockBinding('View', .)")
    enter_process("library(swirl)")
    enter_process("swirl::delete_progress('wush')\n")
    enter_process("swirl::uninstall_all_courses(force = TRUE)")
    enter_process(sprintf("swirl::install_course_directory(path = '%s')", course.dir))
    enter_process('dir.create(file.path(system.file("", package = "swirl"), "user_data", "wush"), recursive = TRUE)')
    enter_process("assign('wush', '333', swirl:::.swirl_classroom_auth_cache)")
    enter_process("swirl()\n")
    enter_process("3\n")
    enter_process("wush\n")
    enter_process("\n")
  }
  
  enter_course <- function(name) {
    wait_until(function(.) any(grepl("帶我去 swirl 課程庫！", ., fixed = TRUE)), check.last = TRUE)
    # . <- search_output(function(.) any(grepl("帶我去 swirl 課程庫！", ., fixed = TRUE))) %>%
    #   max()
    # search_selection(p.buf$output[[.]]$stdout, basename(course.dir)) %>%
    #   enter_process(breakline = TRUE)
    search_output(function(.) any(grepl("帶我去 swirl 課程庫！", ., fixed = TRUE))) %>%
      max() %>%
      search_selection_from_p.buf(TRUE, basename(course.dir)) %>%
      enter_process(breakline =)
    # . <- search_output(function(.) any(grepl(name, ., fixed = TRUE))) %>%
    #   max()
    # search_selection(p.buf$output[[.]]$stdout, name) %>%
    #   enter_process(breakline = TRUE)
    . <- search_output(function(.) any(grepl(name, ., fixed = TRUE))) %>%
      max() %>%
      search_selection_from_p.buf(TRUE, name) %>%
      enter_process(breakline = TRUE)
    src <- file.path(course.dir, name, "lesson.yaml") %>%
      yaml::yaml.load_file()
    ans <- yaml::yaml.load_file(answer.yaml)
    wait_until(function(.) any(grepl("Your status has beed updated to tracking server", ., fixed = TRUE)), is.stdout = FALSE, check.last = TRUE)
    for(i in 2:length(src)) {
      if (src[[i]]$Class == "text") {
        enter_process("\n")
      } else if (src[[i]]$Class == "cmd_question") {
        wait_until(function(.) any(grepl("> ", ., fixed = TRUE)), check.last = TRUE)
        if (!is.null(src[[i]]$CorrectAnswer)) {
          enter_process(src[[i]]$CorrectAnswer, TRUE)
        } else {
          enter_process(ans[[i]]$CorrectAnswer, TRUE)
        }
      } else if (src[[i]]$Class == "script") {
        wait_until(function(.) any(grepl("> ", ., fixed = TRUE)), check.last = TRUE)
        script_temp_path <- get_character("swirl:::.get_e()$script_temp_path")
        correct_script_temp_path <- get_character("swirl:::.get_e()$correct_script_temp_path")
        if (file.exists(correct_script_temp_path)) {
          file.copy(from = correct_script_temp_path, to = script_temp_path, overwrite = TRUE)
        } else {
          cat(ans[[i]]$CorrectAnswer, sep = "\n", file = script_temp_path, append = FALSE)
        }
        #      enter_process("cat(sprintf('output:%s:', swirl:::.get_e()$script_temp_path))\n")
        enter_process("submit()\n")
      } else if (src[[i]]$Class == "mult_question") {
        wait_until(function(.) any(grepl("Selection:", ., fixed = TRUE)), check.last = TRUE)
        ans <- src[[i]]$CorrectAnswer %>% as.character()
        # . <- search_output(function(.) any(grepl("Selection:", ., fixed = TRUE))) %>%
        #   max()
        # . <- search_selection(p.buf$output[[.]]$stdout, ans)
        . <- search_output(function(.) any(grepl("Selection:", ., fixed = TRUE))) %>%
          max() %>%
          search_selection_from_p.buf(TRUE, ans) %>%
          enter_process(breakline = TRUE)
      } else {
        browser()
        stop()
      }
    }
  }
  
  # Execusion
  tryCatch({
    wait_until(function(.) any(grepl("> ", ., fixed = TRUE)))
    enter_swirl()
    enter_course(lesson.name)
  }, finally = {
    subprocess::process_terminate(p)
  })
}
