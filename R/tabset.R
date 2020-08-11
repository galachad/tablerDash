#' Tabler tabset  panel
#' @details Generating tabset from tab items.
#'
#' @export
tablerTabsetPanel <- function(
  ...,
  id = NULL,
  selected = NULL,
  type = c("tabs"),
  width = 6) {
  if (!is.null(id))
    selected <- restoreInput(id = id, default = selected)
  tabs <- list(...)
  type <- match.arg(type)
  tabset <- buildTabset(tabs, paste0("nav nav-", type), NULL,
                        id, selected)
  first <- tabset$navList
  second <- tabset$content
  tabs_tags <- tags$div(class = "card-tabs", first, second)

  shiny::column(
    width = width,
    tabs_tags
  )
}

#' Tabler tabset item
#' @details Creating tabler tabset item.
#'
#' @export
tablerTabsetItem <- function (title, ..., value = title, icon = NULL, title_header = NULL) {
  divTag <-
    div(
      class = "card tab-pane show",
      title = title,
      `data-value` = value,
      `data-icon-class` = shiny:::iconClass(icon),
      div(class = "card-body", if (!is.null(title_header))
        div(class = "card-title", title_header),
        ...
      )
    )
}


buildTabset <- function (tabs, ulClass, textFilter = NULL, id = NULL, selected = NULL,
                         foundSelected = FALSE)
{
  res <- shiny:::findAndMarkSelectedTab(tabs, selected, foundSelected)
  tabs <- res$tabs
  foundSelected <- res$foundSelected
  if (!is.null(id))
    ulClass <- paste(ulClass, "shiny-tab-input")
  if (shiny:::anyNamed(tabs)) {
    nms <- names(tabs)
    nms <- nms[nzchar(nms)]
    stop("Tabs should all be unnamed arguments, but some are named: ",
         paste(nms, collapse = ", "))
  }
  tabsetId <- shiny:::p_randomInt(1000, 10000)
  tabs <- lapply(seq_len(length(tabs)), buildTabItem, tabsetId = tabsetId,
                 foundSelected = foundSelected, tabs = tabs, textFilter = textFilter)
  tabNavList <- tags$ul(class = ulClass, id = id, `data-tabsetid` = tabsetId,
                        lapply(tabs, "[[", 1))
  tabContent <- tags$div(class = "tab-content", `data-tabsetid` = tabsetId,
                         lapply(tabs, "[[", 2))
  list(navList = tabNavList, content = tabContent)
}


buildTabItem <- function (index, tabsetId, foundSelected, tabs = NULL, divTag = NULL,
                          textFilter = NULL)
{
  divTag <- if (!is.null(divTag))
    divTag
  else tabs[[index]]
  if (is.character(divTag) && !is.null(textFilter)) {
    liTag <- textFilter(divTag)
    divTag <- NULL
  }
  else {
    tabId <- paste("tab", tabsetId, index, sep = "-")
    liTag <- tags$li(class = "nav-item",tags$a(href = paste("#", tabId, sep = ""), class = "nav-link",
                                               `data-toggle` = "tab", `data-value` = divTag$attribs$`data-value`,
                                               shiny:::getIcon(iconClass = divTag$attribs$`data-icon-class`),
                                               divTag$attribs$title))
    if (shiny:::isTabSelected(divTag)) {
      liTag$attribs$class <- "active"
      divTag$attribs$class <- "card tab-pane active"
    }
    divTag$attribs$id <- tabId
    divTag$attribs$title <- NULL
  }
  return(list(liTag = liTag, divTag = divTag))
}
