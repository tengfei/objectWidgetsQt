# should this inherit from QDialog?
# submit button not currently necessary due to automatic updating
## need to support properties
qsetClass("ControlPanel", Qt$QWidget, function(obj, visible = list(),
                                               type, parent = NULL, title = "", decimal.extra = 3) {
  super(parent)
  setWindowTitle(title)

  if(is.list(visible)){
    if(length(visible)){
      ## nm.e <- names(visible)
      if(!all(names(obj$properties()) %in% names(visible)))
        stop("Names of list must be within the property set items")
      expd <- unlist(visible)[names(obj$properties())]
      ppt <- obj$properties()[expd]    
    }else{
      ppt <- obj$properties()
    }
  }else if(is.character(visible)){
    if(length(visible)){
      ## nm.e <- names(visible)
      if(!all(visible %in% names(obj$properties())))
        stop("Names of list must be within the property set items")
      ## expd <- rep(!)
      expd <- match(visible, names(obj$properties()))
      ppt <- obj$properties()[expd]    
    }else{
      ppt <- obj$properties()
    }
  }
  #this$submit <- Qt$QPushButton("Submit")
  ## this$reset <- Qt$QPushButton("Reset to Defaults")
  #qconnect(submit, "clicked", function() {
  #  sapply((l.col), function(i) {
  #    eval(parse(text=paste("obj$",i$getPar()," <- i$getValue()",sep="")))
  #  })
  #
  #  sapply(c(l.range,l.enum), function(i) {
  #    eval(parse(text=paste("obj$",i$getPar()," <- i$getValue()",
  #                 sep="")))
  #  })
  #})
  ## qconnect(reset, "clicked", function() {
  ##   obj$reset()
  ##   sapply(l.wid, function(i) {
  ##     i$setDefault()
  ##   })
  ## })
  blyt <- Qt$QHBoxLayout()
  blyt$insertStretch(0,1)
  ## blyt$addWidget(reset)
  #blyt$addWidget(submit)

  olyt <- Qt$QVBoxLayout()
  lyt <- Qt$QFormLayout()
  lyt$setRowWrapPolicy(Qt$QFormLayout$WrapLongRows)

  # best way to check for a particular class
  #sapply(pars$output()$value, function(i) is(i,"SingleEnum"))

  this$l.lab <- list()
  this$l.wid <- list()
  ## FIXME: no visible
  # color widgets
  sapply(names(ppt), function(i) {
    temp <- obj$field(i)
    if(ppt[i] == "Color"){
      l.wid[[i]] <<- ColorWidgetQt(obj, i)
      ## l.wid[[i]] <<- widget(Widget(temp), i)
      l.lab[[i]] <<- ParLabel(obj, i)
      lyt$addRow(l.lab[[i]], l.wid[[i]])
    }
    if(extends(ppt[i],"ColorEnum")){
      l.wid[[i]] <<- ColorEnumWidgetQt(obj, i)
      ## l.wid[[i]] <<- widget(Widget(temp), i)      
      l.lab[[i]] <<- ParLabel(obj, i)
      lyt$addRow(l.lab[[i]], l.wid[[i]])
    }
    if(ppt[i] == "character"){
      l.wid[[i]] <<- CharWidgetQt(obj, i)
      ## l.wid[[i]] <<- widget(Widget(temp), i)      
      l.lab[[i]] <<- ParLabel(obj, i)
      lyt$addRow(l.lab[[i]], l.wid[[i]])
    }
    if(ppt[i] == "logical"){
      l.wid[[i]] <<- logicalWidgetQt(obj, i)
      ## l.wid[[i]] <<- widget(Widget(temp), i) 
      l.lab[[i]] <<- ParLabel(obj, i)
      lyt$addRow(l.lab[[i]], l.wid[[i]])
    }
    if(extends(ppt[i], "NumericWithRange")){
      l.wid[[i]] <<- RangeWidgetQt(obj, i, "double", decimal.extra = decimal.extra)
      ## l.wid[[i]] <<- widget(Widget(temp), i, "double",
                            ## decimal.extra = decimal.extra)      
      l.lab[[i]] <<- ParLabel(obj, i)
      lyt$addRow(l.lab[[i]], l.wid[[i]])
    }
    if(extends(ppt[i], "IntegerWithRange")){
      l.wid[[i]] <<- RangeWidgetQt(obj, i, "integer", decimal.extra = decimal.extra)
      ## l.wid[[i]] <<- widget(Widget(temp), i, "double",
      ##                       decimal.extra = decimal.extra)      
      l.lab[[i]] <<- ParLabel(obj, i)
      lyt$addRow(l.lab[[i]], l.wid[[i]])
    }
    if(ppt[i] %in% 
       c("PositiveInteger","NonnegativeInteger","NegativeInteger",
         "NonpositiveInteger")){
      ## l.wid[[i]] <<- widget(Widget(temp), i, substr(ppt[i],1,6))
      l.wid[[i]] <<- IntWidgetQt(obj, i, substr(ppt[i],1,6))
      l.lab[[i]] <<- ParLabel(obj, i)
      lyt$addRow(l.lab[[i]], l.wid[[i]])

    }
    if(extends(ppt[i],"SingleEnum") && (!extends(ppt[i],"ColorEnum")) && (!extends(ppt[i],"GlyphEnum"))){
      ## l.wid[[i]] <<- widget(Widget(temp), i)      
      l.wid[[i]] <<- SingleEnumWidgetQt(obj, i)
      l.lab[[i]] <<- ParLabel(obj, i)
      lyt$addRow(l.lab[[i]], l.wid[[i]])
    }
    if(extends(ppt[i],"MultipleEnum")){
      ## l.wid[[i]] <<- widget(Widget(temp), i)      
      l.wid[[i]] <<- MultEnumWidgetQt(obj, i)
      l.lab[[i]] <<- ParLabel(obj, i)
      lyt$addRow(l.lab[[i]], l.wid[[i]])
    }
    if(extends(ppt[i],"GlyphEnum")){
      ## l.wid[[i]] <<- widget(Widget(temp), i)      
      l.wid[[i]] <<- GlyphEnumWidgetQt(obj, i)
      l.lab[[i]] <<- ParLabel(obj, i)
      lyt$addRow(l.lab[[i]], l.wid[[i]])
    }
      ## l.lab[[i]] <<- ParLabel(obj, i)
      ## lyt$addRow(l.lab[[i]], l.wid[[i]])
  })
  olyt$addLayout(lyt)
  olyt$addLayout(blyt)

  setLayout(olyt)
})


qsetMethod("setValue", ControlPanel, function(par, val) {
  l.wid[[par]]$setValue(val)
})
