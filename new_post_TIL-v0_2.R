
# Use R to generate a Quarto blogpost
# ==== Especially for posting my TIL in my blog only! ===
# The {cli} and {fs} packages make life easy!
# Tom Mock
# reference : https://themockup.blog/posts/2022-11-08-use-r-to-generate-a-quarto-blogpost/

# ver 0.2

# 수정사항
# layout-ncol=2 위치 변경 
  # - 전체에 한꺼번에 적용 -> iterated phrase function에 적용
  # 전체에 한꺼번에 적용시 중간에 section title, author가 들어가면 순서가 틀어지는 현상 발생.

new_post_TIL <- function(
    categories,
    numbers_of_images = 1,
    file = "index.qmd", 
    date = Sys.Date(), 
    draft = FALSE,
    open_file = TRUE
){
  # generate the slug as draft, prefix with _ which prevents
  # quarto from rendering/recognizing the folder
  if(draft){
    slug <- glue::glue("TIL/_{date}")
    cli::cli_alert_info("Appending a '_' to folder name to avoid render while a draft, remove '_' when finished.")
  } else {
    slug <- glue::glue("TIL/{date}")
  }
  
  # create and alert about TIL/date directory
  fs::dir_create(
    path = slug
  )
  cli::cli_alert_success("Folder created at {.file {slug}}")
  
  # create and alert about TIL/date/images directory
  fs::dir_create(
    path = slug + "/images"
  )
  cli::cli_alert_success("Folder created at {.file {slug}/images}")
  
  # wrap description at 77 characters
  # description <- stringr::str_wrap(description, width = 77) |> 
  #   stringr::str_replace_all("[\n]", "\n  ")
  
  # start generating file
  new_post_file <- glue::glue("{slug}/{file}")
  
  # title 
  title = paste0("TIL on ",toupper(substr(weekdays(Sys.Date()),1,3))) # 요일 앞에 3개 글자만 대문자로 출력  
  
  # build yaml core
  new_post_core <- c(
    "---",
    glue::glue('title: "{title}"'),
    glue::glue("date: {date}"),
    glue::glue("categories: [{categories}]"),
    glue::glue("page-navigation: true"),
    glue::glue("lightbox: "), # 탭을 넣어야 하는데 잘 될지 확인
    glue::glue("  match: auto"),  
    glue::glue("  desc-position: right"),
    glue::glue("language: "),
    glue::glue("  ko-KR:"),  
    glue::glue("   title-block-modified: \"수정\""),  
    glue::glue("toc: true")  
  )
  
  # add draft if draft
  if(draft){
    new_post_text <- c(
      new_post_core,
      "draft: true",
      "---\n"
    )
  } else {
    new_post_text <- c(
      new_post_core,
      "---\n"
    )
  }
  # ===== profile ko =====
  profile_ko_text_upper = c(
    "::: {.content-visible when-profile=\"ko\"}\n\n",
    "### section title\n\n",
    "- section author\n\n"
  )
  
  # define iterated_phrase function
  iterated_phrase <- function(numbers_of_images) {
    phrases <- c("::: {layout-ncol=2}\n\n", 
                 "![](./images/Scan.jpeg){group=\"my-gallery\"}\n\n", 
                 ":::{.callout-note}\n:::\n",
                 ":::\n") # layout-ncol
    
    if (numbers_of_images > 1) {
      additional_phrases <- sapply(1:(numbers_of_images - 1), function(i) {
        c("::: {layout-ncol=2}\n\n",
          glue::glue("![](./images/Scan {i}.jpeg){{group=\"my-gallery\"}}\n\n"), 
          ":::{.callout-note}\n:::\n",
          ":::\n")# layout-ncol
      })
      
      phrases <- c(phrases, unlist(additional_phrases))
    }
    
    return(phrases)
  }
  
  # using iterated_phrase function
  iterated_phrase_ko = iterated_phrase(numbers_of_images)
  
  profile_ko_text_end = c(
    ":::\n\n"  # profile = ko
  )
  # ===== profile ko END =====
  # ===== profile en =====
  
  profile_en_text_upper = c(
    "::: {.content-visible when-profile=\"en\"}\n\n",
    "### section title\n\n",
    "- section author\n\n"
  )
  
  # using iterated_phrase function
  iterated_phrase_en = iterated_phrase(numbers_of_images)
  
  profile_en_text_end = c(
    ":::"  # profile = en
  )  
  # ===== profile en END =====
  
  
  new_post_text = c(
    new_post_text,
    profile_ko_text_upper, 
    iterated_phrase_ko,
    profile_ko_text_end,
    profile_en_text_upper, 
    iterated_phrase_en,
    profile_en_text_end
  )
  
  # finalize new post text
  new_post_text <- paste0(
    new_post_text,
    collapse = "\n"
  )
  
  # create file and alert
  fs::file_create(new_post_file)
  cli::cli_alert_success("File created at {.file {new_post_file}}")
  
  # print new post information
  cat(new_post_text)
  
  if(yesno::yesno2("Are you ready to write that to disk?")){
    writeLines(
      text = new_post_text,
      con = new_post_file
    )
    
    rstudioapi::documentOpen(new_post_file, line = length(new_post_text))
  } 
  
}