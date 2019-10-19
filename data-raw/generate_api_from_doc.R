
library(tidyverse)
library(httr)
library(rvest)

base_api <- "https://devdata.osisoft.com/piwebapi/help"
auth <- authenticate(Sys.getenv("R_PI_PUBLIC_USER"), Sys.getenv("R_PI_PUBLIC_PASSWORD"))

full_url <- function(url, from = base_api) {
  from_dir <- str_replace(from, "/[^/]*$", "/")
  from_server <- str_extract(from, "https?://[^/]+")
  case_when(
    str_detect(url, "^/") ~ paste0(from_server, url),
    str_detect(url, "^http") ~ url,
    TRUE ~ paste0(from_dir, url)
  )
}

# full_url(c("/api", "../fishing", "http://google.com"))

page_links <- function(doc, base_url = base_api) {
  doc %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    full_url(from = base_url) %>%
    unique()
}

# page_links(read_html(base_api))

docs <- new.env(parent = emptyenv())

get_doc <- function(url) {
  if (!(url %in% names(docs))) {
    message(url)
    docs[[url]] <- read_html(url)
  }
  docs[[url]]
}

crawl_doc <- function(url) {
  urls <- page_links(get_doc(url), base_url = url)
  urls <- setdiff(urls, names(docs))
  if (any(str_detect(urls, "#"))) {
    browser()
  }
  # urls <- str_subset(urls, "#", negate = TRUE)
  for (new_url in urls) {
    Sys.sleep(0.1)
    crawl_doc(new_url)
  }
  invisible()
}

# crawl_doc(base_api)

# ---- extract functions -------

page_url <- "https://devdata.osisoft.com/piwebapi/help/controllers/analysis/actions/get"

url_to_function <- function(page_url, fun_name = "XXXXX") {
  page <- get_doc(page_url)

  title <- page %>%
    html_node("#content") %>%
    html_node("p") %>%
    html_text() %>%
    str_remove("\\.$")

  endpoint <- page %>%
    html_node("#content") %>%
    html_node("h2") %>%
    html_text() %>%
    str_trim() %>%
    str_replace(".*?(GET|POST|DELETE|UPDATE)\\s+", "\\1 ")

  http_method <- str_remove(endpoint, "\\s.*")
  fun <- str_remove(endpoint, ".*?\\s")

  params_lis <- page %>%
    html_node("#content") %>%
    html_node("ul") %>%
    html_nodes("li")

  params <- tibble(
    param = map_chr(params_lis, ~html_node(., "h4") %>% html_text()),
    description = map_chr(params_lis, ~html_node(., "p") %>% html_text()) %>%
      str_replace_all("\\s+", " ") %>%
      str_wrap()
  ) %>%
    filter(param != "webIdType", param != "selectedFields")

  fun_params <- str_extract_all(fun, "\\{.*?\\}") %>%
    .[[1]] %>%
    str_remove_all("\\{|\\}")

  query_string_params <- setdiff(params$param, fun_params)

  wrap_roxygen <- function(x) {
    str_replace_all(x, "\n", "\n#'  ")
  }

  fun_text <- if (length(fun_params) > 0) glue::glue("glue::glue(\"{fun}\")") else glue::glue("\"{fun}\"")

  param_section <- glue::glue("#' @param {params$param} {wrap_roxygen(params$description)}") %>%
    paste0(collapse = "\n")
  glue::glue(
    "
    #' {title}
    #'
    #' @inheritParams pi_get
    {param_section}
    #' @param ... Passed to [pi_get()]
    #'
    #' @references
    #' {page_url}
    #'
    #' @examples
    #' con <- pi_connect_public()
    #' pi_{fun_name}(con)
    #'
    pi_{fun_name} <- function({paste0(c('.con', params$param), collapse = ', ')}, ...) {{
      {if ('webId' %in% params$param) 'webId <- pi_web_id(webId)' else ''}
      pi_get(
        .con,
        {fun_text},
        {paste(query_string_params, '=', query_string_params, collapse = ', ')},
        ...
      )
    }}
    "
  )
}

# -------- make functions + doc! -------------

url_to_function("https://devdata.osisoft.com/piwebapi/help/controllers/assetserver/actions/list", "assetserver_list")
