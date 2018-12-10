#' Get Calls
#'
#' Function to get all calls
#' @param token Your Data API token
#' @param user_id Your ID, request it from technical support
#' @param date_from date_from, by default adding 00:00:00
#' @param date_till date_till, by default adding 00:00:00, so that you should do date+1
#' @param filter filters, deafult value is !=NULL https://www.comagic.ru/support/api/data-api/#_filters
#' @param fields (see \code{\link[Fields]{https://www.comagic.ru/support/api/data-api/Reports/}})
#' @export
#' @importFrom httr POST
#' @importFrom httr GET
#' @importFrom httr stop_for_status
#' @importFrom httr content
#' @importFrom httr add_headers
#' @importFrom rjson toJSON
#' @importFrom rjson fromJSON
#' @importFrom dplyr %>%
#' @importFrom plyr rbind.fill
#' @examples
#' getCallsCoMagic()

getCallsCoMagic <- function (
  token = NULL,
  user_id = NULL,
  date_from = NULL,
  date_till = NULL,
  filter = list(
    field = "campaign_name",
    operator = "!=",
    value = NULL),
  fields = list(
    "id","visitor_id","ua_client_id","start_time","finish_time","finish_reason","direction",
    "is_lost","communication_number","communication_id","communication_type",
    "total_duration","virtual_phone_number","sale_date","sale_cost",
    "search_query","search_engine","referrer_domain","referrer",
    "entrance_page","channel","tags","scenario_name",
    "site_domain_name","campaign_name","visitor_city","visitor_region",
    "visitor_country","visitor_device","contact_phone_number",
    "utm_source","utm_medium","utm_term","utm_content",
    "utm_campaign", "attributes", "source")){
  proc_start <- Sys.time()
  numdays <- as.integer(difftime(date_till,date_from))
  divnumber <- (numdays-1) %/% 90
  if (numdays %% 90 == 0) {divremainder <- 90} else {divremainder <- numdays %% 90}
  mainresult <- data.frame(stringsAsFactors = F)
  #  i=1
  ch <- 0
  for (i in 0:divnumber)
  {
    if (i == divnumber) {divtill <- 0} else {divtill <- 1}
    date_from1 <- as.Date(date_from) + i*90
    date_till1 <- as.Date(date_till) - divremainder*divtill - (divnumber-i-1)*90*divtill
    date_from2 <- paste0(date_from1, " 00:00:00")
    date_till2 <- paste0(date_till1, " 00:00:00")
    fields <- if(is.null(fields)) {list("contact_phone_number",
                                        "id",
                                        "start_time")
    }else {as.list(fields) }
    Calls <- content(POST("https://dataapi.comagic.ru/v2.0",
                          body = toJSON(
                            list(
                              jsonrpc="2.0",
                              id=user_id,
                              method="get.calls_report",
                              params= list(
                                access_token = token,
                                # user_id - user_id,
                                date_from = date_from2,
                                date_till = date_till2,
                                filter = filter,
                                fields = fields)
                            )
                          )),"parsed", "application/json")

    totalItems <- Calls$result$metadata$total_items
    page <- floor(totalItems/1000)
    result <- data.frame(stringsAsFactors = F)

    for (p in 0:page) {
      Calls <- content(POST("https://dataapi.comagic.ru/v2.0",
                            body = toJSON(
                              list(
                                jsonrpc="2.0",
                                id=user_id,
                                method="get.calls_report",
                                params= list(
                                  access_token = token,
                                  # user_id=userId,
                                  limit = 1000,
                                  offset = p*1000,
                                  date_from = date_from2,
                                  date_till = date_till2,
                                  filter = filter,
                                  fields = fields)
                              )
                            )),"parsed", "application/json")


      # gsub()


      for (iii in 1:length(Calls$result$data))
      {

        Calls$result$data[[iii]] <-  Calls$result$data[[iii]] %>% replace(.=="NULL", NA)
        if (length(Calls$result$data[[iii]]$attributes[[1]]) > 1)
        {
          for (ii in 1:length(Calls$result$data[[iii]]$attributes[[1]])) Calls$result$data[[iii]]$attributes[[1]] <-  Calls$result$data[[iii]]$attributes[[1]] %>% replace(.=="NULL", NA)
          Calls$result$data[[iii]]$attributes <- Calls$result$data[[iii]]$attributes[[1]]
        }

        if (length(Calls$result$data[[iii]]$tags[[1]]) > 1)
        {

          for (itag in 1:length(Calls$result$data[[iii]]$tags))
          {
            for (ii in 1:length(Calls$result$data[[iii]]$tags[[itag]])) Calls$result$data[[iii]]$tags[[itag]] <-  Calls$result$data[[iii]]$tags[[itag]] %>% replace(.=="NULL", NA)
            tagresult <- data.frame(stringsAsFactors = F)
            tagresult <- Calls$result$data[[iii]]
            tagresult$tags <- Calls$result$data[[iii]]$tags[[itag]]
            result <- rbind.fill(result,data.frame(tagresult,stringsAsFactors = F))

          }
        } else {
          result <- rbind.fill(result,data.frame(Calls$result$data[[iii]],stringsAsFactors = F))
        }

      }

      packageStartupMessage("Processed ",ch+length(result[[1]])," rows", appendLF = T)
      ch <- ch + length(result[[1]])
    }
    mainresult <- rbind.fill(mainresult,result)

  }
  # column_names <- unlist(lapply(c(names(rows[[1]])),
  #                                function(x) return(x)))
  # colnames(result) <- column_names
  # for (rows_i in 1:length(rows)) {
  #    result <- rbind(result, unlist(rows[[rows_i]]))
  # }
  names(mainresult) <- gsub("[..]","_",names(mainresult))
  names(mainresult) <- gsub("[.]","_",names(mainresult))

  # result$utm_campaign <- as.integer(result$utm_campaign)
  total_work_time <- round(difftime(Sys.time(), proc_start , units ="secs"),0)
  packageStartupMessage(paste0("Total time: ",total_work_time, " sec."))

  return(mainresult)

}


