
library(shiny)
library(shinydashboard)
library(data.table)
library(dplyr)
library(ggvis)
library(tidyr)

#---- load data --------------------------------------------

tmp_data <- fread("new.csv", header=TRUE, verbose=FALSE, 
                  stringsAsFactors=FALSE, encoding = 'UTF-8')
d <- tbl_df(tmp_data) %>%
    mutate(date_form = format(as.Date(date_time, format="%d.%m.%y %H:%M")), "%d.%m")

user_rank <- d %>%
    group_by(user) %>%
    summarize(sum_duration_sec = sum(duration_sec), 
              minutes = sum_duration_sec%/%60,
              sec = sum_duration_sec%%60,
              time_label = paste(minutes, ifelse(sec < 10, paste0(0, sec), sec), sep=":"))  %>%
    arrange(desc(sum_duration_sec))

tooltips_user_rank <- function(x) {
    row <- user_rank[user_rank$user == x$user, c(1,5)]
    paste0( format(row), collapse = "<br />")
}

# accumulated
dinamics <- d %>%
    group_by(date_form) %>%
    summarize(sum_duration_sec = sum(duration_sec), 
              minutes = sum_duration_sec%/%60,
              sec = sum_duration_sec%%60,
              time_label = paste(minutes, ifelse(sec < 10, paste0(0, sec), sec), sep=":"))

# for each and every user 
dinamics2 <- d %>%
    group_by(date_form, user) %>%
    summarize(sum_duration_sec = sum(duration_sec)) %>% 
    spread(user, sum_duration_sec, fill=0) %>% 
    mutate(idle = 0)

# combined
dinamics3 <- dinamics %>%
    left_join(dinamics2)

tooltips_dinamics <- function(x) {
    row <- dinamics[dinamics$date_form == x$date_form, 5]
    paste0( row, collapse = "<br />")
}

# ---- SERVER ------------------------------------------------

function(input, output) {
    
    user_rank %>%
        ggvis(y = ~user, x = ~sum_duration_sec/60, opacity := 0.7) %>%
        layer_rects(fill.hover = "red", x2 = 0, height=band()) %>%
        add_axis("y", title="") %>%
        add_axis("x", title="Total speaking time (minutes)", orient = "top",
                 properties = axis_props(title = list(fontSize = 12))) %>%
        set_options(height = 660, width = "auto") %>%
        add_tooltip(tooltips_user_rank, "hover") %>%
        hide_legend("fill") %>%
        bind_shiny("barplot")
    
    dinamics %>%
        ggvis(x=~as.factor(date_form)) %>%
        layer_ribbons(y = 0, y2 = ~sum_duration_sec/60, fill := "black", opacity := 0.3) %>%
        layer_points(y = ~sum_duration_sec/60) %>%
        layer_lines(y = ~sum_duration_sec/60) %>%
        add_axis("x", title = "", properties = axis_props(labels = list(angle = 90, align = 'top'))) %>%
        add_axis("y", title = "Speaking time (minutes)", title_offset = 55, 
                 properties = axis_props(title = list(fontSize = 12))) %>%
        set_options(height = 250, width = "auto") %>%
        #add_tooltip(tooltips_dinamics, "hover") %>%
        bind_shiny("dinamics")
    
    #output$user <- renderText({if (input$e0 == '') 'Ekaterina' else input$e0})
        
    # data <- renderDataTable({
    #     a <- if (input$e0 == '') 'Ekaterina' else input$e0
    #     d <- dinamics3[, c("date_form", a)]
    #     names(d) = c("date", "user")
    #     d
    #     })

    reactive({
        a <- if (input$e0 == '') 'idle' else input$e0
        d <- dinamics3[, c("date_form", a)]
        names(d) = c("date", "user")
        d %>%
            ggvis(x=~as.factor(date)) %>%
            layer_ribbons(y = 0, y2 = ~user/60, fill := "purple", opacity := 0.3) %>%
         layer_points(y = ~user/60) %>%
         layer_lines(y = ~user/60) %>%
         add_axis("x", title = "", properties = axis_props(labels = list(angle = 90, align = 'top'))) %>%
         add_axis("y", title = "Speaking time (minutes)", title_offset = 55,
                  properties = axis_props(title = list(fontSize = 12))) %>%
         set_options(height = 250, width = "auto")
        }) %>% bind_shiny("dinamics_personal")

}
