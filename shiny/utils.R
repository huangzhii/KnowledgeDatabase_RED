smartModal <- function(error=c(T,F), title = "Title", content = "Content"){
  if(error){
    showModal(modalDialog(
      title = title, footer = modalButton("OK"), easyClose = TRUE,
      div(class = "busy",
          p(content),
          style = "margin: auto; text-align: center"
      )
    ))
  }
  else{
    showModal(modalDialog(
      title = title, footer = NULL,
      div(class = "busy",
          p(content),
          img(src="https://cdn.dribbble.com/users/448879/screenshots/2644555/dna.gif"),
          style = "margin: auto; text-align: center"
      )
    ))
  }
}

createPUBMEDLink <- function(val) {
  sprintf('<a href="https://www.ncbi.nlm.nih.gov/pubmed/?term=%s" target="_blank" class="btn btn-primary">%s</a>',val,val)
}