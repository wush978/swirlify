#'@title Test a YAML Lesson File
#'
#'This function tests whether the \code{CorrectAnswer} matches 
#'the \code{AnswerTests}.
#'
#'It only checks \code{cmd_question} and \code{mult_question} now.
#'
#'@param lesson_dir path. The location of lesson.
#'@export
test_lesson <- function(lesson_dir = NULL){
  if (is.null(lesson_dir)) {
    lesson_dir <- dirname(lesson_file_check(getOption("swirlify_lesson_file_path")))
  }
  print(paste("####Begin testing", lesson_dir))
  .e <- environment(swirl:::any_of_exprs)
  attach(.e)
  on.exit(detach(.e))
  e = new.env()

  # Since the initLesson might change working directory, load lesson yaml first before that happens.
  lesson = yaml.load_file(paste0(lesson_dir,"/lesson.yaml"))
  
  
  for(R_file in c("/customTests.R", "/initLesson.R")){
    R_file_path = paste0(lesson_dir, R_file)
    if(file.exists(R_file_path)) source(R_file_path,local = e)
  }
  
  for(question in lesson){
    if(!is.null(question$CorrectAnswer) && question$Class=="cmd_question"){
      print(paste(">", question$CorrectAnswer))
      suppressWarnings({
        eval(parse(text=question$CorrectAnswer), envir = e)
        e$expr <- parse(text = question$CorrectAnswer)[[1]]
        stopifnot(eval(parse(text=question$AnswerTests), envir = e))
      })
    } 
  }
  
  print(paste("-----Testing", lesson_dir, "Done"))
}