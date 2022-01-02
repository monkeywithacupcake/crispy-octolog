# functions to output book graphics
# come back and Roxygen this

library(tidyverse)
library(ggfittext)

#' example makeAndSaveSquarePatchwork(df, "~/Desktop")
makeAndSaveSquarePatchwork <- function(df, dirpath){
  library(patchwork)
  df <- prepareBookData(df) # this removes DNF and gets month
  genre_pal <- makeGenrePallette(unique(df$Genre))
  month_plot <- makeMonthPlot(df, genre_pal) 
  fav_plot <- makeFavPlot(df) 
  genre_plot <- makeGenrePlot(df, genre_pal)
  audience_plot <- makeByFormatPlot(df, "Audience", "Target Audience")
  format_plot <- makeByFormatPlot(df, "Format")
  nation_plot <- makeByFormatPlot(df, "Nation of Origin") 
  lpatchwork <- (month_plot + inset_element(fav_plot, left = 0.5, bottom = 0.7, right = 1, top = 1))/
    (genre_plot + audience_plot)/
    (format_plot + nation_plot) + 
    plot_layout(height = c(4, 2, 1))
  lpatchwork <- lpatchwork + plot_annotation(
    title = paste('Reading Log - 2021', nrow(df), 'Books!'),
    theme = theme(plot.title = element_text(size = 18,face = "bold")),
    subtitle = paste0('This year, I read ', 
                      nrow(df[df$Format != "Audiobook",]), 
                      ' books, totalling ', 
                      scales::comma(sum(df$Pages[df$Format != "Audiobook"])),
                      ' pages and listened to ',
                      nrow(df[df$Format == "Audiobook",]), 
                      ' audiobooks. \nSize is length in pages or minutes; Shading is format; Color is Genre'),
    caption = 'Book clubs this year: 2021 Book Riot Read Harder & NPR Politics \n Audiobooks mostly read on >=2X speed while running'
  )
  ggsave(filename = "readinglogsquare.jpg",
         plot = lpatchwork, 
         device = "jpeg",
         width = 15,
         height = 15,
         path = dirpath)
}




#' example genre_pal <- makeGenrePallette
makeGenrePallette <- function(gVector){
  library(RColorBrewer)
  distinct_genres <- n_distinct(gVector)
  data.frame(
    "genre" = gVector,
    "color" = c(brewer.pal(n = 8, name = "Dark2"),
                brewer.pal(n = distinct_genres-8, name = "Set3"))
  )
}

# come back and make this work by identifying the right variables
prepareBookData <- function(df){
  df %>%
    filter(is.na(DNF)) %>% # only keep finished
    select(-DNF) %>%
    mutate(type = ifelse(Format == "Audiobook", "Audio", "Print"),
           month = lubridate::month(`Finish Date`, label=TRUE),
           blength = ifelse(Format == "Audiobook", 
                            lubridate::hour(Length)*60 + lubridate::minute(Length), 
                            Pages)) %>%
    arrange(`Finish Date`) 
}

makeMonthPlot <- function(df, genre_pal){
  if( missing(genre_pal)){
    # we need to generate it
    genre_pal <- makeGenrePallette(unique(df$Genre))
  }
  df %>%
    mutate(stitle = sub(":.*", "", sub(" \\(.*", "", Title)),
           genre = Genre) %>%
    left_join(genre_pal) %>%
    group_by(month) %>%
    arrange(`Finish Date`) %>%
    mutate(run_l = cumsum(blength),
           start = ifelse(row_number() == 1, 0 , lag(run_l)),
           index = factor(row_number())) %>%
    ungroup()  %>%
    ggplot(aes(month, blength, fill = Genre, alpha = type, label = stitle)) + 
    geom_col(color = "#DFDFDF") +
    scale_alpha_manual(values = c(0.5,0.9)) +
    geom_fit_text(
      reflow = TRUE,
      position = "stack",
      lineheight = 0.8) +
    scale_fill_manual(values = color) +
    labs(x = "",
         y = "") + #"Length (in Pages or Minutes)") +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()
    )
}

makeFavPlot <- function(df) {
  # this makes a list that acts like a plot for patchwork
  favs <- df %>%
    mutate(stitle = sub(":.*", "", sub(" \\(.*", "", Title)),
           stars = str_count(`⭐️ Rating`,"\u2b50")) %>%
    filter(stars > 4) 
  # this one cannot be piped because we need row_number to reset!
  favs <- favs %>%
    mutate(w = paste(stitle,"by", Author),
           i = row_number()) %>%
    select(w,i) 
  
  ggplot(favs, aes(0,i, label=w)) +
    geom_text(hjust = 0, size = 4, color = "grey20") +
    scale_x_continuous(limits = c(0,1),
                       expand = expansion(mult = c(0, 0))) +
    expand_limits(x=c(0,1)) +
    theme_void() +
    labs(title = "~ My Most Enjoyed Reads ~") + 
    theme(legend.position="none")
}

makeGenrePlot <- function(df, genre_pal){
  if( missing(genre_pal)){
    # we need to generate it
    genre_pal <- makeGenrePallette(unique(df$Genre))
  }
  df %>%
    group_by(Genre, type) %>%
    summarise(length = sum(blength), .groups = "drop") %>%
    group_by(Genre) %>%
    mutate(l = sum(length),
           genre = Genre) %>%
    left_join(genre_pal) %>%
    ggplot(aes(reorder(Genre,l), length, fill = Genre, alpha = type)) + 
    geom_col() +
    scale_alpha_manual(values = c(0.5,0.9)) +
    scale_fill_manual(values = color) +
    labs(title = "by Genre",
         x = "",
         y = "") +
    theme_minimal() +
    theme(legend.position="none",
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) + 
    coord_flip()
}

makeByFormatPlot <- function(df, bycol, clean_name = by_col){
  if(missing(clean_name)){
    clean_name = bycol #bycol is a string
  }
  title <- paste("by", clean_name)
  df %>%
    group_by(.data[[bycol]], type) %>%
    summarise(length = sum(blength), .groups = "drop") %>%
    group_by(.data[[bycol]]) %>%
    mutate(l = sum(length)) %>%
    ggplot(aes(reorder(.data[[bycol]],l), length, alpha = type)) + 
    geom_col() +
    scale_alpha_manual(values = c(0.5,0.9)) +
    labs(title = title,
         x = "",
         y = "") +
    theme_minimal()+
    theme(legend.position="none",
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) + 
    coord_flip()
}

# end