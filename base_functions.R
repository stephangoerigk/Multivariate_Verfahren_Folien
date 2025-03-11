roundallnumerics = function(df, digits){
  for(j in 1:ncol(df)){
    if(is.numeric(df[,j])){
      df[,j] = round(df[,j], digits)
    }
  }
  return(df)
}

p_labeller = function(vec){
  vec = as.numeric(vec)
  for(i in 1:length(vec)){
    if(is.na(vec[i]) == F & vec[i] < .001){
      vec[i] = "<.001***"
    }
    if(is.na(vec[i]) == F & vec[i] >= .001 & vec[i] < .01){
      vec[i] = paste0(vec[i], "**")
    }
    if(is.na(vec[i]) == F & vec[i] > .01 & vec[i] < .05){
      vec[i] = paste0(vec[i], "*")
    }
  }
  return(vec)
}

flex_func_box = function(df){
  flextable(df) %>%
    flextable::font(fontname = "Times New Roman", part = "all") %>%
    flextable::fontsize(size = 8, part = "all") %>%
    flextable::bold(bold = TRUE, part = "header") %>%
    flextable::theme_box()
}

mytheme = theme_bw() + theme(panel.grid.minor = element_blank(),
                             panel.grid.major = element_blank(), panel.background = element_blank(),
                             strip.background = element_blank(), strip.text.y = element_text(),
                             legend.background = element_blank(), legend.key = element_blank(),
                             panel.border = element_rect(colour = "black", fill = NA))
theme_set(mytheme) 
