# utility_funcitons -------------------------------------------------------

import_rda_file <-
  function(file =NULL) {

    if (file %>% is_null()) {
      stop("Please enter a file path")
    }

    env <- new.env()
    nm <- load(file, env)[1]
    env[[nm]] %>%
      dplyr::as_data_frame()
  }


curl_url <-
  function(url = "https://github.com/abresler/FRED_Dictionaries/blob/master/data/fred_series_data.rda?raw=true"){
    con <-
      url %>%
      curl::curl()

    data <-
      con %>%
      import_rda_file()
    close(con)
    return(data)
  }

get_data_df <- function() {
  dplyr::data_frame(
    nameFile = c('all', 'names', 'items'),
    urlFile = c(
      "https://github.com/abresler/nazi_data/blob/master/data/nazis_all.rda?raw=true",
      "https://github.com/abresler/nazi_data/blob/master/data/nazis_names.rda?raw=true",
      "https://github.com/abresler/nazi_data/blob/master/data/nazi_multiple_items.rda?raw=true"
    )
  )
}

#' import nazi data
#'
#' @param table_name
#' @import httr curl tidyr rvest dplyr stringr lubridate xml2 purrr stringi readr
#' @return
#' @export
#' @examples
import_nazi_data <-
  function(table_name = 'all') {
    df_tables <-
      get_data_df()

    if (!table_name %in% df_tables$nameFile) {
      stop("Tables can only be all, names, or items")
    }
    table_name <-
      table_name %>%
      str_to_lower()
    url <- df_tables %>%
      filter(nameFile == table_name) %>%
      .$urlFile
    curl_url(url = url)
  }

get_class_df <-
  function(data) {
    class_data <-
      data %>%
      map(class)

    class_df <-
      1:length(class_data) %>%
      map_df(function(x) {
        data_frame(nameColumn = names(data)[[x]],
                   typeColumn = class_data[[x]] %>% .[length(.)])
      })

    return(class_df)
  }

#' tidy counts
#'
#' @param data
#' @param column_keys
#' @param bind_to_original_df
#'
#' @return
#' @export
#'
#' @examples
tidy_counts <-
  function(data,
           column_keys = c('nameActualPerson', 'urlBiography'),
           bind_to_original_df = FALSE) {
    class_df <-
      data %>%
      get_class_df()
    columns_matching <-
      names(data)[!names(data) %>% substr(nchar(.), nchar(.)) %>% readr::parse_number() %>% is.na() %>% suppressWarnings()] %>%
      suppressWarnings() %>%
      suppressMessages() %>%
      suppressWarnings()


    data <-
      data %>%
      mutate(idRow = 1:n()) %>%
      select(idRow, everything())

    df <-
      data_frame()

    if (columns_matching %>% length() > 0) {
      match <-
        columns_matching %>% str_replace_all('[0-9]', '') %>% unique() %>% paste0(collapse = '|')

      df_match <-
        data %>%
        select(idRow, one_of(column_keys), dplyr::matches(match))

      has_dates <-
        names(df_match) %>% str_count("date") %>% sum() > 0

      if (has_dates) {
        df_match <-
          df_match %>%
          mutate_at(df_match %>% select(dplyr::matches("^date")) %>% names(),
                    funs(. %>% as.character()))
      }

      key_cols <-
        df_match %>%
        select(c(idRow, one_of(column_keys))) %>%
        names()

      valuecol <-
        'value'

      gathercols <-
        df_match %>%
        select(-c(idRow, one_of(column_keys))) %>%
        names()

      df_match <-
        df_match %>%
        gather_('item', 'value', gathercols, na.rm = T) %>%
        mutate(
          countItem = item %>% readr::parse_number(),
          countItem = ifelse(countItem %>% is.na(), 0, countItem) + 1,
          item = item %>% str_replace_all('[0-9]', '')
        ) %>%
        suppressWarnings() %>%
        suppressMessages()
      df <-
        df %>%
        bind_rows(df_match)

    }
    has_data <-
      df %>% nrow() > 0

    if (!has_data) {
      return(data %>% select(-dplyr::matches("idRow")))
    }
    if (has_data) {
      df <-
        df %>%
        arrange(idRow) %>%
        distinct()

      col_order <-
        c(df %>% select(-c(item, value)) %>% names(), df$item)

      df <-
        df %>%
        spread(item, value) %>%
        select(one_of(col_order)) %>%
        suppressWarnings()

      has_dates <-
        data %>% select(dplyr::matches("^date")) %>% ncol() > 0

      if (has_dates) {
        df <-
          df %>%
          mutate_at(df %>% select(dplyr::matches("^date[A-Z]")) %>% names(),
                    funs(. %>% lubridate::ymd())) %>%
          mutate_at(df %>% select(dplyr::matches("^datetime[A-Z]")) %>% names(),
                    funs(. %>% lubridate::ymd_hm()))
      }

      if (bind_to_original_df) {
        ignore_cols <-
          columns_matching %>% paste0(collapse = '|')

        data <-
          data %>%
          select(-dplyr::matches(ignore_cols)) %>%
          left_join(df %>%
                      nest(-idRow, .key = 'dataMultipleItems')) %>%
          suppressMessages()
        return(data)
      }
      if (!bind_to_original_df) {
        return(df)
      }
    }

  }



# scraper -----------------------------------------------------------------
get_page_count <- function() {
  page <-
    "http://pamiec.pl/pa/form/60,Zaloga-SS-KL-Auschwitz.html?page=1" %>%
    xml2::read_html()

  page_count <-
    page %>%
    html_nodes('#meni_strony a') %>%
    html_text() %>%
    readr::parse_number() %>%
    max(na.rm = TRUE) %>%
    suppressMessages() %>%
    suppressWarnings()

  page <-
    list('http://pamiec.pl/pa/form/60,Zaloga-SS-KL-Auschwitz.html?page=',page_count) %>%
    purrr::reduce(paste0) %>%
    read_html()

  has_final_page <-
    page %>%
    html_nodes('h3') %>%
    html_text() %>%
    str_trim() %>% length() > 0

  if (!has_final_page) {
    page_count <-
      page_count - 1
  }
  page_count
}

get_name_df <-
  function() {
    data_frame(nameItem = c("Date of Birth", "Place of Birth", "Nationality", "Education Background",
                            "Occupation", "NSDAP Affiliation", "Allgemeine SS Affiliation",
                            "SS Membership Number", "Professional, Voluntary Military Service in SS Units",
                            "Service in SS-Verfügungstruppen, Totenkopfverbände, Waffen-SS",
                            "Affiliation to other organisations", "Military service till 1920",
                            "Military service in other countries", "Military service in Reichswehr, Wehrmacht",
                            "Awarded decorations, distinctions, medals", "urlDocumentDetails"),
               nameActual  = c("dateBirth", "locationBirth", "countryNationality", "descriptionEducation",
                               "descriptionOccupation", "descriptionNSDAPAffiliation", "descriptionAllgemeineSSAffiliation",
                               "idSS", "typeServiceSS",
                               "descriptionServiceWaffenSS",
                               "descritionAffiliationOtherOrganizations", "descriptionServicePrior1920",
                               "descriptionServiceOtherCountries", "descriptionServiceWehrmacht",
                               "descriptionMedals", "urlDocumentDetails"
               )
    )
  }

get_all_page_urls <-
  function() {
    page_count <-
      get_page_count()

    urls <-
      list('http://pamiec.pl/pa/form/60,Zaloga-SS-KL-Auschwitz.html?page=',1:page_count) %>%
      purrr::reduce(paste0)
    return(urls)
  }

parse_search_page <-
  function(urls, return_message = TRUE) {
    df <-
      data_frame()
    success <- function(res) {
      if (return_message) {
        list("Parsing: ", res$url) %>% purrr::reduce(paste0) %>% message()
      }

      page <-
        res$url %>%
        read_html()

      person <-
        page %>%
        html_nodes('h3') %>%
        html_text() %>%
        str_trim()
      url_person <-
        page %>%
        html_nodes('.strz a') %>%
        html_attr('href') %>%
        paste0('http://pamiec.pl',.)

      images <- page %>%
        html_nodes('.fotoSS') %>%
        html_attr('src') %>%
        paste0('http://pamiec.pl',.)

      data <-
        data_frame(namePerson = person,
                   urlImage = images,
                   urlBiography = url_person)
      data <-
        data %>%
        mutate(hasImage = urlImage %>% str_detect(".jpg"),
               urlImage = ifelse(hasImage, urlImage, NA)) %>%
        tidyr::separate(namePerson, sep = ' alias ', into = c('namePerson', 'aliasPerson')) %>%
        suppressWarnings()

      df <<-
        df %>%
        bind_rows(data)
    }
    failure <- function(msg){
      cat(sprintf("Fail: %s (%s)\n", res$url, msg))
    }
    urls %>%
      walk(function(x){
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()
    df
  }

parse_nazi_bios <-
  function(urls, return_message = TRUE) {
    df <-
      data_frame()
    success <- function(res) {
      if (return_message) {
        list("Parsing: ", res$url) %>% purrr::reduce(paste0) %>% message()
      }

      page <-
        res$content %>%
        read_html()

      name_actual <-
        page %>%
        html_nodes('.naglowek h2') %>%
        html_text()

      has_alias <- name_actual %>% str_detect(pattern = 'alias ')

      if (has_alias) {
        name_parts <-
          name_actual %>% str_split(' alias ') %>% flatten_chr()
        name_alias <-
          name_parts[[2]]
        name_actual <-
          name_parts[[1]]
      } else {
        name_alias <-
          NA
      }

      actual_name <-
        name_actual %>%
        map_chr(function(x) {
          count_spaces <-
            x %>% str_count('\\ ')

          if (count_spaces == 0) {
            return(x)
          }

          if (count_spaces == 1) {
            name_parts <-
              x %>% str_split('\\ ') %>% flatten_chr()

            actual_name <- list(name_parts[2], name_parts[1]) %>%
              purrr::reduce(paste)
            return(actual_name)
          }
          name_parts <-
            x %>% str_split('\\ ') %>% flatten_chr()
          name_last <-
            name_parts[[1]]

          name_first <-
            name_parts[2:(name_parts %>% length())] %>%
            purrr::reduce(paste)
          list(name_first, name_last) %>%
            purrr::reduce(paste)
        })

      values <-
        page %>%
        html_nodes('strong') %>%
        html_text()

      items <-
        page %>%
        html_nodes('.label') %>%
        html_text()

      items <-
        items[!items %>% str_detect("Judicial Documents")]

      values <-
        1:length(values) %>%
        map_chr(function(x) {
          value <-
            values[[x]]
          if (value %>% str_count(' ') == 0) {
            return(value)
          }
          if (value == "No data") {
            value <-
              NA
          }
          has_alias <- value %>% str_detect(' alias ')

          if (has_alias) {
            value <- value %>% str_split(' alias ') %>%
              flatten_chr() %>%
              unique() %>%
              paste0(collapse = '; ')
            return(value)
          }

          has_dash <-
            value %>% str_count(' / ') > 0
          if (has_dash) {
            value <-
              value %>% str_split(' / ') %>%
              flatten_chr() %>% .[[2]]
          }

          bracket_count <-
            value %>% str_count('\\[')

          if (bracket_count == 1) {
            value <-
              value %>% str_split('\\[') %>%
              flatten_chr() %>%
              str_trim() %>%
              .[[1]]
            value <-
              value %>% str_split('\\/') %>%
              flatten_chr() %>%
              paste0(collapse = '; ')
            return(value)
          }

          if (bracket_count > 1) {
            bracket_values <-
              value %>%
              str_split('\\;') %>%
              flatten_chr() %>%
              str_trim()
            value <-
              1:length(bracket_values) %>%
              map_chr(function(x) {
                bracket_value <-
                  bracket_values[[x]] %>%
                  str_split('\\[') %>%
                  flatten_chr()
                skip <-
                  bracket_value %>% grep('\\]', .)
                if (skip %>% length() == 0) {
                  skip <-
                    0
                }
                all_values <-
                  bracket_value %>%
                  str_split('\\]') %>%
                  flatten_chr() %>%
                  str_replace_all('\\:', '') %>%
                  str_trim()
                all_values <-
                  all_values[!all_values == '']
                all_values[-skip[[1]]] %>% paste0(collapse = ':') %>%
                  str_replace_all('\\ -:', ' - ')
              }) %>%
              paste0(collapse = '; ')

            return(value)
          }

          if (value == "No data") {
            return(NA)
          }

          value
        })

      items <-
        1:length(items) %>%
        map_chr(function(x) {
          item <-
            items[[x]]
          if (item %>% str_count('/') == 0) {
            return(item)
          }

          item <-
            item %>% str_split(' / ') %>%
            flatten_chr() %>% .[[3]]
          item
        }) %>%
        str_replace_all('\\:', '')

      has_documents <-
        page %>%
        html_nodes('.okno a') %>%
        html_attr('href') %>%
        length() > 0

      if (has_documents) {
        url_documents <-
          page %>%
          html_nodes('.okno a') %>%
          html_attr('href') %>%
          paste0('http://pamiec.pl', .)

        pdfs <-
          url_documents %>%
          map_chr(function(x) {
            page_pdf <-
              x %>%
              read_html()

            page_pdf %>%
              html_nodes('.form_text a') %>%
              html_attr('href') %>%
              paste0('http://pamiec.pl', .)
          })

        items <-
          c(items, rep('urlDocumentDetails', length(pdfs)))
        values <-
          c(values, pdfs)
      }

      data <-
        data_frame(
          nameActualPerson = actual_name,
          nameAlias = name_alias,
          nameItem = items,
          value = values,
          urlBiography = res$url
        )

      df_names <-
        get_name_df()

      df_actual_names <-
        data$nameItem %>%
        map_df(function(x) {
          actual_name <- df_names %>%
            filter(nameItem %>% str_detect(x)) %>%
            .$nameActual

          data_frame(nameItem = x,
                     nameActual = actual_name)
        })

      data <-
        data %>%
        left_join(df_actual_names) %>%
        select(-matches("nameAlias")) %>%
        select(nameActualPerson, nameActual, value, urlBiography) %>%
        suppressMessages() %>% suppressWarnings()
      has_no_rows <-
        data %>%
        filter(!value %>% is.na()) %>%
        nrow() == 0
      if (has_no_rows) {
        data <-
          data %>%
          select(nameActualPerson, urlBiography) %>%
          distinct()
      }

      if (!has_no_rows) {
        data <-
          data %>%
          left_join(df_actual_names) %>%
          select(-matches("nameAlias")) %>%
          select(nameActualPerson, nameActual, value, urlBiography) %>%
          filter(!value %>% is.na()) %>%
          suppressMessages()
        df_values <-
          data %>%
          select(nameActual, value)

        df_values <-
          1:nrow(df_values) %>%
          map_df(function(x) {
            name_actual <-
              df_values$nameActual[[x]]

            values <-
              df_values$value[[x]] %>%
              str_split('\\;') %>%
              flatten_chr() %>%
              str_trim()

            data_frame(nameActual = name_actual, value = values)

          })

        data <-
          df_values %>%
          distinct() %>%
          mutate(
            nameActualPerson = data$nameActualPerson %>% unique(),
            urlBiography = data$urlBiography %>% unique()
          ) %>%
          select(nameActualPerson, urlBiography, everything()) %>%
          group_by(nameActual) %>%
          mutate(countItem = 1:n() - 1) %>%
          ungroup() %>%
          mutate(nameActual = ifelse(countItem > 0, paste0(nameActual, countItem), nameActual)) %>%
          suppressMessages() %>%
          select(-countItem)

        col_order <-
          c('nameActualPerson', 'urlBiography', data$nameActual)

        data <-
          data %>%
          spread(nameActual, value) %>%
          select(one_of(col_order))

        data <-
          data %>%
          mutate_at(data %>% select(matches("date")) %>% names(),
                    funs(. %>% lubridate::dmy()))
      }

      df <<-
        df %>%
        bind_rows(data)
    }
    failure <- function(msg) {
      cat(sprintf("Fail: %s (%s)\n", res$url, msg))
    }
    urls %>%
      walk(function(x) {
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()
    df
  }

#' Get Nazi Data
#'
#' @param parse_bios
#' @param tidy_columns
#' @param return_message
#'
#' @return
#' @export
#' @import httr curl tidyr rvest dplyr stringr lubridate xml2 purrr stringi readr formattable
#' @examples
get_nazis <-
  function(parse_bios = TRUE,
           tidy_columns = FALSE,
           return_message = TRUE) {
    urls <-
      get_all_page_urls()

    df_search <-
      parse_search_page(urls = urls)

    if (parse_bios) {
      df_bios <-
        df_search$urlBiography %>%
        parse_nazi_bios(return_message = return_message)

      all_data <-
        df_bios %>%
        left_join(df_search) %>%
        suppressMessages()

      all_data <-
        all_data %>%
        mutate(
          nameActualPerson = nameActualPerson %>% str_to_upper(),
          nameLast = nameActualPerson %>% stri_extract_last_boundaries(),
          nameFirst = nameActualPerson %>% stri_extract_first_boundaries(),
          nameFirst = ifelse(nameFirst == nameLast, NA, nameFirst),
          letterLastNameFirst = nameLast %>% substr(1, 1),
          yearBirth = dateBirth %>% lubridate::year() %>% as.numeric()
        ) %>%
        select(
          yearBirth,
          nameActualPerson,
          hasImage,
          nameFirst,
          nameLast,
          aliasPerson,
          letterLastNameFirst,
          urlImage,
          everything()
        ) %>%
        select(-namePerson)

      if (tidy_columns) {
        all_data <-
          all_data %>%
          tidy_counts(bind_to_original_df = TRUE)
      }
    } else {
      all_data <-
        df_search
    }

    if (return_message) {
      list("Found ", all_data %>% nrow() %>% formattable::comma(digits = 0), ' Nazis in the Auschwitz SS database') %>%
        purrr::reduce(paste0) %>%
        message()
    }
    return(all_data)
  }
