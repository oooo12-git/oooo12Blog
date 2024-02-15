
# Use R to generate a Quarto blogpost
# ==== Especially for posting my TIL in my blog only! ===
# The {cli} and {fs} packages make life easy!
# Tom Mock
# reference : https://themockup.blog/posts/2022-11-08-use-r-to-generate-a-quarto-blogpost/

# ====== ver 1.1.1 ======

library(stringr)

TIL_profile_image_callout = function(n,cate){
  # ==== profile ko ====
  ## profile_ko_text_upper minimal
  ### ***NOT - include SECTION title & author***
  
  n2 = n - 1
  profile_ko_text_upper_m = c(
    "::: {.content-visible when-profile=\"ko\"}\n\n"
  )    
  
  # ==== image + callout ====
  phrases <- c("::: {layout-ncol=2}\n\n", 
               glue::glue("![{cate}](./images/Scan {n2}.jpeg){{group=\"my-gallery\"}}\n\n"), 
               ":::{.callout-note}\n",
               glue::glue("{cate}"),
               ":::\n", # callout
               ":::\n", # layout-ncol
               ":::\n\n") # profile
  
  ## ==== profile en ====
  ## profile_en_text_upper minimal
  ### ***NOT - include SECTION title & author***
  profile_en_text_upper_m = c(
    "::: {.content-visible when-profile=\"en\"}\n\n")
  
  ### === image + callout ===
  #### phrases2 - 합칠때 넣어야 함.
  result = c(profile_ko_text_upper_m, phrases, 
             profile_en_text_upper_m, phrases)
  return(result)
}

new_post_TIL<- function(
    subject,
    date = Sys.Date(), 
    # categories,
    # numbers_of_images = 1,
    file = "index.qmd", 
    draft = FALSE,
    open_file = TRUE
){
  # generate the slug as draft, prefix with _ which prevents
  # quarto from rendering/recognizing the folder
  if(draft){
    slug <- glue::glue("TIL/_{date}_{subject}")
    cli::cli_alert_info("Appending a '_' to folder name to avoid render while a draft, remove '_' when finished.")
  } else {
    slug <- glue::glue("TIL/{date}_{subject}")
  }

  # start generating file
  new_post_file <- glue::glue("{slug}/{file}")
  
  # title 
  title = paste0("TIL on ",toupper(substr(weekdays(Sys.Date()),1,3))," about ",subject) # 요일 앞에 3개 글자만 대문자로 출력  
  
  # command line prompt iteration
  
  # ===== 1st image(Scan.jpeg) ===== 
  cate1 = readline(prompt = "Categorize 1st image: ")
  cate1_split = str_split(cate1,",")[[1]]
  
  ## build yaml core(category - X)
  new_post_core <- c(
    "---",
    glue::glue('title: "{title}"'),
    glue::glue("date: {date}"),
    glue::glue("page-navigation: true"),
    glue::glue("lightbox: "),
    glue::glue("  match: auto"),  
    glue::glue("  desc-position: right"),
    glue::glue("language: "),
    glue::glue("  ko-KR:"),  
    glue::glue("   title-block-modified: \"수정\""),  
    glue::glue("toc: true")  
  )
  
  ### add draft if draft
  if(draft){
    new_post_text <- c(
      new_post_core,
      "draft: true"
    )
  } else {
    new_post_text <- c(
      new_post_core
    )
  }
  
  ## profile ko
  ### ***include SECTION title & author***
  
  profile_ko_text_upper = c(
    "::: {.content-visible when-profile=\"ko\"}\n\n",
    "### section title\n\n",
    "- section author\n\n"
  )  
  ### === image + callout ===
  phrases1 <- c("::: {layout-ncol=2}\n\n", 
                glue::glue("![{cate1}](./images/Scan.jpeg){group=\"my-gallery\"}\n\n"), 
               ":::{.callout-note}\n",
               glue::glue("{cate1}"),
               ":::\n", # callout
               ":::\n", # layout-ncol
               ":::\n\n") # profile end
  
  ## profile en
  
  profile_en_text_upper = c(
    "::: {.content-visible when-profile=\"en\"}\n\n",
    "### section title\n\n",
    "- section author\n\n"
  )  
  ### === image ===
  #### phrases1 - 합칠때 넣어야 함.
  
  ## !! yes no !!
  if(yesno::yesno2("Do you have a 2nd image?")){
    
    cate2 = readline(prompt = "Categorize 2nd image: ")
    cate2_split = str_split(cate2,",")[[1]]
    
    # 2nd image(Scan 1.jpeg) 
    
    result2 = TIL_profile_image_callout(2,cate2)
    
  # !! yes no !!
  
  if(yesno::yesno2("Do you have a 3rd image?")){
    
    cate3 = readline(prompt = "Categorize 3rd image: ")
    cate3_split = str_split(cate3,",")[[1]]
    
    # 3rd image(Scan 2.jpeg)
    
    result3 = TIL_profile_image_callout(3,cate3)
    
    if(yesno::yesno2("Do you have a 4th image?")){
      
      cate4 = readline(prompt = "Categorize 4th image: ")
      cate4_split = str_split(cate4,",")[[1]]
      
      # 4th image(Scan 3.jpeg)
      
      result4 = TIL_profile_image_callout(4,cate4)
      
      if(yesno::yesno2("Do you have a 5th image?")){
        
        cate5 = readline(prompt = "Categorize 5th image: ")
        cate5_split = str_split(cate5,",")[[1]]
        
        # 5th image(Scan 4.jpeg)
        
        result5 = TIL_profile_image_callout(5,cate5)
        
        yesno::yesno2("Are you ready to write that to disk?")
        
        # output 5 images
        # make up categories
        categories = str_c(c(cate1_split,cate2_split,cate3_split,cate4_split,cate5_split), 
                           collapse = ",") # cate1, cate2 ... 다 합치기
        make_up_categories = c(glue::glue("categories: [{categories}]"),
                               "---\n")
        new_post_text = c(new_post_text, make_up_categories)
        
        # combine (when I have 3 images)
        new_post_text = c(
          new_post_text,
          profile_ko_text_upper, 
          phrases1,
          profile_en_text_upper,
          phrases1, 
          result2,
          result3,
          result4,
          result5
        )
        
        # finalize new post text
        new_post_text <- paste0(
          new_post_text,
          collapse = "\n"
        )
        
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
        
        
        # create file and alert
        fs::file_create(new_post_file)
        cli::cli_alert_success("File created at {.file {new_post_file}}")
        
        writeLines(
          text = new_post_text,
          con = new_post_file
        )
        
        rstudioapi::documentOpen(new_post_file, line = length(new_post_text))
      } else{
        if(yesno::yesno2("Are you ready to write that to disk?")){
          
          # output 4 images
          # make up categories
          categories = str_c(c(cate1_split,cate2_split,cate3_split,cate4_split), 
                             collapse = ",") # cate1, cate2 ... 다 합치기
          make_up_categories = c(glue::glue("categories: [{categories}]"),
                                 "---\n")
          new_post_text = c(new_post_text, make_up_categories)
          
          # combine (when I have 4 images)
          new_post_text = c(
            new_post_text,
            profile_ko_text_upper, 
            phrases1,
            profile_en_text_upper,
            phrases1, 
            result2,
            result3,
            result4
          )
          
          # finalize new post text
          new_post_text <- paste0(
            new_post_text,
            collapse = "\n"
          )
          
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
          
          
          # create file and alert
          fs::file_create(new_post_file)
          cli::cli_alert_success("File created at {.file {new_post_file}}")
          
          writeLines(
            text = new_post_text,
            con = new_post_file
          )
          
          rstudioapi::documentOpen(new_post_file, line = length(new_post_text))
      }
      }
    } else {
      if(yesno::yesno2("Are you ready to write that to disk?")){
        
        # output 3 images
        # make up categories
        categories = str_c(c(cate1_split,cate2_split,cate3_split), collapse = ",") # cate1, cate2 ... 다 합치기
        make_up_categories = c(glue::glue("categories: [{categories}]"),
                               "---\n")
        new_post_text = c(new_post_text, make_up_categories)
        
        # combine (when I have 3 images)
        new_post_text = c(
          new_post_text,
          profile_ko_text_upper, 
          phrases1,
          profile_en_text_upper,
          phrases1, 
          result2,
          result3
        )
        
        # finalize new post text
        new_post_text <- paste0(
          new_post_text,
          collapse = "\n"
        )
        
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
        
        
        # create file and alert
        fs::file_create(new_post_file)
        cli::cli_alert_success("File created at {.file {new_post_file}}")
        
        writeLines(
          text = new_post_text,
          con = new_post_file
        )
        
        rstudioapi::documentOpen(new_post_file, line = length(new_post_text))
      }
    }
  }
  else{
    if(yesno::yesno2("Are you ready to write that to disk?")){
      
      # output 2 images
    # make up categories
    categories = str_c(c(cate1_split,cate2_split), collapse = ",") # cate1, cate2 ... 다 합치기
    make_up_categories = c(glue::glue("categories: [{categories}]"),
                           "---\n")
    new_post_text = c(new_post_text, make_up_categories)
    
    # combine (when I have 2 images)
    new_post_text = c(
      new_post_text,
      profile_ko_text_upper, 
      phrases1,
      profile_en_text_upper,
      phrases1, 
      result2
    )
    
    # finalize new post text
    new_post_text <- paste0(
      new_post_text,
      collapse = "\n"
    )
    
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
    
    
    # create file and alert
    fs::file_create(new_post_file)
    cli::cli_alert_success("File created at {.file {new_post_file}}")
    
    writeLines(
      text = new_post_text,
      con = new_post_file
    )
    
    rstudioapi::documentOpen(new_post_file, line = length(new_post_text))
    }
  }
  } else {
    if(yesno::yesno2("Are you ready to write that to disk?")){
      # output 1 image
      
      # make up categories
      categories = str_c(c(cate1_split), collapse = ",") # cate1, cate2 ... 다 합치기
      make_up_categories = c(glue::glue("categories: [{categories}]"),
                             "---\n")
      new_post_text = c(new_post_text, make_up_categories)
      
      # combine (when I have 1 images)
      new_post_text = c(
        new_post_text,
        profile_ko_text_upper, 
        phrases1,
        profile_en_text_upper,
        phrases1
      )
      
      # finalize new post text
      new_post_text <- paste0(
        new_post_text,
        collapse = "\n"
      )
      
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
      
      
      # create file and alert
      fs::file_create(new_post_file)
      cli::cli_alert_success("File created at {.file {new_post_file}}")
      
      writeLines(
        text = new_post_text,
        con = new_post_file
      )
      
      rstudioapi::documentOpen(new_post_file, line = length(new_post_text))
    }
  }
}