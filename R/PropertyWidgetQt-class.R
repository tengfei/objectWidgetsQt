setRefClass("PropertySetWidgetQt",
            contains = c("Qt", "PropertySetWidget"))
ColorWQT <- qsetClass("ColorWidgetQt", Qt$QWidget, function(obj, par, parent = NULL) {
  super(parent)
  this$obj <- obj
  this$par <- par
  initColor <- obj$field(par)
  #parInfo <- obj$output()$parinfo[names(obj$output()$parinfo) == par]
  #this$parLabel <- Qt$QLabel(paste(parInfo,":",sep=""))
  #parLabel$setToolTip(
  #  obj$output()$tooltipinfo[names(obj$output()$tooltipinfo) == par])
  this$parSwatch <- Qt$QPushButton()
  parSwatch$setAutoFillBackground(TRUE)
  parSwatch$setFocusPolicy(Qt$Qt$NoFocus)
  parSwatch$setStyleSheet(paste("background-color:",initColor,sep=""))
  this$parEdit <- Qt$QLineEdit(initColor)
  this$col <- Qt$QColorDialog()

  qconnect(parSwatch, "clicked", function() {
    col$show()
  })

  qconnect(col, "accepted", function() {
    #parSwatch$setStyleSheet(paste("background-color:",
    #  col$currentColor$name(),sep=""))
    #parEdit$setText(col$currentColor$name())
    setValue(col$currentColor$name())
  })

  qconnect(parEdit, "editingFinished", function() {
    setValue(parEdit$text)
  })
  
  lyt <- Qt$QHBoxLayout()
  #lyt$addWidget(parLabel,1,Qt$Qt$AlignRight)
  lyt$addWidget(parSwatch)
  lyt$addWidget(parEdit)

  setLayout(lyt)
})

qsetMethod("getPar", ColorWidgetQt, function() {
  par
})

qsetMethod("getValue", ColorWidgetQt, function() {
  parEdit$text
})

# also updates the obj object with new color
qsetMethod("setValue", ColorWidgetQt, function(clr) {
  if(Qt$QColor$isValidColor(clr)) {
    parSwatch$setStyleSheet(paste("background-color:",clr,sep=""))
    parEdit$setText(clr)
    eval(parse(text=paste("obj$",par," <- parEdit$text",sep="")))
  } else {
    parEdit$setText("")
    parLabel$setFocus(Qt$Qt$OtherFocusReason)
    parEdit$setPlaceholderText("Error: Invalid color entered")
  }
})

qsetMethod("setDefault", ColorWidgetQt, function() {
  clr <- obj$field(par)
  parSwatch$setStyleSheet(paste("background-color:",clr,sep=""))
  parEdit$setText(clr)  
})


# widget to handle changing colors
qsetClass("ColorEnumWidgetQt", Qt$QWidget, function(obj, par, parent = NULL) {
  super(parent)
  this$obj <- obj; this$par <- par

  initColor <- obj$field(par)

  this$colors <- eval(parse(text=paste("levels(obj$",par,")",sep="")))
  
  this$dropList <- Qt$QComboBox()
  sapply(colors, function(i) {
    pmap <- Qt$QPixmap(30,20)
    pmap$fill(Qt$QColor(i))
    icon <- Qt$QIcon(pmap)
    dropList$addItem(icon,i)
  })
  dropList$setCurrentIndex(which(colors == initColor) - 1)
  dropList$setIconSize(Qt$QSize(40,20))  

  # change obj when user changes level
  qconnect(dropList, "currentIndexChanged(QString)", function(idx) {
    eval(parse(text=paste("obj$",par,
                 " <- dropList$currentText",sep="")))
  })

  lyt <- Qt$QHBoxLayout()
  lyt$addWidget(dropList)
  
  setLayout(lyt)
})

qsetMethod("getPar", ColorEnumWidgetQt, function() {
  par
})

qsetMethod("getValue", ColorEnumWidgetQt, function() {
  dropList$currentText
})

qsetMethod("setValue", ColorEnumWidgetQt, function(val) {
  if(val %in% colors) dropList$setCurrentIndex(which(colors == val) - 1)
})

qsetMethod("setDefault", ColorEnumWidgetQt, function() {
  val <- obj$field(par)
  dropList$setCurrentIndex(which(colors == val) - 1)
})



# widget to handle changing colors
qsetClass("GlyphEnumWidgetQt", Qt$QWidget, function(obj, par, parent = NULL) {
  super(parent)
  this$obj <- obj; this$par <- par

  initLvl <- obj$field(par)

  this$levels <- eval(parse(text=paste("levels(obj$",par,")",sep="")))
  
  this$dropList <- Qt$QComboBox()
  icons <- eval(parse(text=paste("icons(obj$",par,")",sep="")))
  sapply(seq_along(levels), function(i) {
    dropList$addItem(icons[[i]],levels[i])
  })
  dropList$setCurrentIndex(which(levels == initLvl) - 1)
  dropList$setIconSize(Qt$QSize(40,20))  

  # change obj when user changes level
  qconnect(dropList, "currentIndexChanged(QString)", function(idx) {
    eval(parse(text=paste("obj$",par,
                 " <- dropList$currentText",sep="")))
  })
  lyt <- Qt$QHBoxLayout()
  lyt$addWidget(dropList)
  setLayout(lyt)
})

qsetMethod("getPar", GlyphEnumWidgetQt, function() {
  par
})

qsetMethod("getValue", GlyphEnumWidgetQt, function() {
  dropList$currentText
})

qsetMethod("setValue", GlyphEnumWidgetQt, function(val) {
  if(val %in% levels) dropList$setCurrentIndex(which(levels == val) - 1)
})

qsetMethod("setDefault", GlyphEnumWidgetQt, function() {
  val <- obj$field(par)
  dropList$setCurrentIndex(which(levels == val) - 1)
})

# widget for changing numeric values (general for any numeric range, but
# for now hard-coded for a 0-1 range)
qsetClass("RangeWidgetQt", Qt$QWidget, function(obj, par, type,parent = NULL, decimal.extra = 3)
{
  super(parent)
  this$obj <- obj; this$par <- par; this$type <- type

  initVal <- obj$field(par)
  #this$minVal <- eval(parse(text=paste("obj$",par,"@min",sep="")))
  this$minVal <- initVal@min
  #this$maxVal <- eval(parse(text=paste("obj$",par,"@max",sep="")))
  this$maxVal <- initVal@max
  
  #parInfo <- obj$output()$parinfo[names(obj$output()$parinfo) == par]
  #this$parLabel <- Qt$QLabel(paste(parInfo,":",sep=""))
  #parLabel$setToolTip(
  #  obj$output()$tooltipinfo[names(obj$output()$tooltipinfo) == par])
  if(type == "double") {
    this$spin <- Qt$QDoubleSpinBox()
    spin$setDecimals(decimalplaces(minVal) + decimal.extra)
    spin$setSingleStep((maxVal - minVal)/100)
  }
  if(type == "integer"){
    this$spin <- Qt$QSpinBox()
    spin$setSingleStep(1)
  }

  spin$setRange(minVal, maxVal)
  spin$setValue(initVal)

  # slider -- only supports integers, so need to adjust values
  this$sl <- Qt$QSlider(Qt$Qt$Horizontal)

  if(type == "double") {
    sl$setRange(0,100)
    sl$setValue(100*(initVal - minVal)/(maxVal - minVal))
  }
  if(type == "integer"){
    sl$setSingleStep(1)
    sl$setRange(minVal,maxVal)
    sl$setValue(initVal)
  }


  if(type == "double"){
    qconnect(spin, "valueChanged(double)", function(val) {
      sl$setValue(as.integer(100*(val - minVal)/(maxVal - minVal)))
    })
  }
  if(type == "integer"){
    qconnect(spin, "valueChanged(int)", function(val) {
      sl$setValue(val)
    })
  }
  # update spinbox when slider changes, and update the obj
  qconnect(sl, "valueChanged", function(val) {
    if(type == "double") {
      spin$setValue(val / 100 * (maxVal - minVal) + minVal)
    } else {
      spin$setValue(val)
    }
    eval(parse(text=paste("obj$",par," <- spin$value",sep="")))
  })
  
  lyt <- Qt$QHBoxLayout()
  #lyt$addWidget(parLabel,1,Qt$Qt$AlignRight)
  lyt$addWidget(spin)
  lyt$addWidget(sl)

  setLayout(lyt)
})

qsetMethod("getPar", RangeWidgetQt, function() {
  par
})

qsetMethod("getValue", RangeWidgetQt, function() {
  spin$value
})

qsetMethod("setValue", RangeWidgetQt, function(val) {
  spin$setValue(val)
})

qsetMethod("setDefault", RangeWidgetQt, function() {
  val <- obj$field(par)
  spin$setValue(val)
})

# widget to change levels from a class extending Enum
qsetClass("SingleEnumWidgetQt", Qt$QWidget, function(obj, par, parent = NULL)
{
  super(parent)
  this$obj <- obj; this$par <- par

  initLvl <- obj$field(par)
  
  this$levels <- eval(parse(text=paste("levels(obj$",par,")",sep="")))
  #parInfo <- obj$output()$parinfo[names(obj$output()$parinfo) == par]
  #this$parLabel <- Qt$QLabel(paste(parInfo,":",sep=""))
  #parLabel$setToolTip(
  #  obj$output()$tooltipinfo[names(obj$output()$tooltipinfo) == par])

  this$dropList <- Qt$QComboBox()
  sapply(levels, dropList$addItem)
  dropList$setCurrentIndex(which(levels == initLvl) - 1)

  # change obj when user changes level
  qconnect(dropList, "currentIndexChanged(QString)", function(idx) {
    eval(parse(text=paste("obj$",par,
                 " <- dropList$currentText",sep="")))
  })

  lyt <- Qt$QHBoxLayout()
  #lyt$addWidget(parLabel,1,Qt$Qt$AlignRight)
  lyt$addWidget(dropList)

  setLayout(lyt)
})

qsetMethod("getPar", SingleEnumWidgetQt, function() {
  par
})

qsetMethod("getValue", SingleEnumWidgetQt, function() {
  dropList$currentText
})

qsetMethod("setValue", SingleEnumWidgetQt, function(val) {
  if(val %in% levels) dropList$setCurrentIndex(which(levels == val) - 1)
})

qsetMethod("setDefault", SingleEnumWidgetQt, function() {
  val <- obj$field(par)
  dropList$setCurrentIndex(which(levels == val) - 1)
})


# widget to change levels from a class extending Enum with ability to
# select multiple levels
qsetClass("MultEnumWidgetQt", Qt$QWidget, function(obj, par, parent = NULL)
{
  super(parent)
  this$obj <- obj; this$par <- par

  initVal <- obj$field(par)
  
  this$levels <- eval(parse(text=paste("levels(obj$",par,")",sep="")))
  this$currentVal <- levels %in% initVal
  #parInfo <- obj$output()$parinfo[names(obj$output()$parinfo) == par]
  #this$parLabel <- Qt$QLabel(paste(parInfo,":",sep=""))
  #parLabel$setToolTip(
  #  obj$output()$tooltipinfo[names(obj$output()$tooltipinfo) == par])
  
  lyt <- Qt$QGridLayout()

  this$bg <- Qt$QButtonGroup()
  bg$setExclusive(FALSE)

  # initiate buttons, and check those that are within the current value
  sapply(seq_along(levels), function(i) {
    button <- Qt$QCheckBox(levels[i])
    lyt$addWidget(button, floor((i-1)/4), (i-1) %% 4)
    bg$addButton(button, i)
    bg$button(i)$setChecked(currentVal[i])
  })

  # change obj when user changes level
  qconnect(bg, "buttonClicked(int)", function(id) {
    currentVal[id] <<- bg$button(id)$checked
    eval(parse(text=paste("obj$",par," <- levels[currentVal]",
                   sep="")))
  })

  #lyt <- Qt$QHBoxLayout()
  #lyt$addWidget(parLabel,1,Qt$Qt$AlignRight)
  #lyt$addWidget(dropList)

  setLayout(lyt)
})

qsetMethod("getPar", MultEnumWidgetQt, function() {
  par
})

qsetMethod("getValue", MultEnumWidgetQt, function() {
  levels[currentVal]
})

qsetMethod("setValue", MultEnumWidgetQt, function(val) {
  if(all(val %in% levels)) {
    currentVal <- levels %in% val
    sapply(seq_along(levels), function(i) {
      bg$button(i)$setChecked(currentVal[i])
    })
    eval(parse(text=paste("obj$",par," <- levels[currentVal]",sep="")))
  } else {
    stop("Error: one or more levels specified are not valid")
  }
})

qsetMethod("setDefault", MultEnumWidgetQt, function() {
  val <- obj$field(par)
  currentVal <- levels %in% val
  sapply(seq_along(levels), function(i) {
    bg$button(i)$setChecked(currentVal[i])
  })
})

qsetClass("logicalWidgetQt", Qt$QWidget, function(obj, par, parent = NULL)
{
  super(parent)
  this$obj <- obj; this$par <- par

  initVal <- obj$field(par)
  
  this$levels <- "TRUE"
  this$currentVal <- levels %in% initVal
  #parInfo <- obj$output()$parinfo[names(obj$output()$parinfo) == par]
  #this$parLabel <- Qt$QLabel(paste(parInfo,":",sep=""))
  #parLabel$setToolTip(
  #  obj$output()$tooltipinfo[names(obj$output()$tooltipinfo) == par])
  
  lyt <- Qt$QGridLayout()

  this$bg <- Qt$QButtonGroup()
  bg$setExclusive(FALSE)
  # initiate buttons, and check those that are within the current value
  sapply(seq_along(levels), function(i) {
    button <- Qt$QCheckBox(levels[i])
    lyt$addWidget(button, floor((i-1)/4), (i-1) %% 4)
    bg$addButton(button, i)
    bg$button(i)$setChecked(currentVal[i])
  })

  # change obj when user changes level
  qconnect(bg, "buttonClicked(int)", function(id) {
    currentVal[id] <<- bg$button(id)$checked
    eval(parse(text=paste("obj$",par," <- ",!as.logical(levels),
                   sep="")))
  })

  #lyt <- Qt$QHBoxLayout()
  #lyt$addWidget(parLabel,1,Qt$Qt$AlignRight)
  #lyt$addWidget(dropList)

  setLayout(lyt)
})

qsetMethod("getPar", logicalWidgetQt, function() {
  par
})

qsetMethod("getValue", logicalWidgetQt, function() {
  levels[currentVal]
})

qsetMethod("setValue", logicalWidgetQt, function(val) {
  if(all(val %in% levels)) {
    currentVal <- levels %in% val
    sapply(seq_along(levels), function(i) {
      bg$button(i)$setChecked(currentVal[i])
    })
    eval(parse(text=paste("obj$",par," <- levels[currentVal]",sep="")))
  } else {
    stop("Error: one or more levels specified are not valid")
  }
})

qsetMethod("setDefault", logicalWidgetQt, function() {
  val <- obj$field(par)
  currentVal <- levels %in% val
  sapply(seq_along(levels), function(i) {
    bg$button(i)$setChecked(currentVal[i])
  })
})

# widget for changing integer values (obselete at this point since there
# are not parameters of this type)
qsetClass("IntWidgetQt", Qt$QWidget, function(obj, par, type, parent = NULL)
{
  super(parent)
  this$obj <- obj; this$par <- par; this$type <- type

  initVal <- obj$field(par)

  #parInfo <- obj$output()$parinfo[names(obj$output()$parinfo) == par]
  #this$parLabel <- Qt$QLabel(paste(parInfo,":",sep=""))
  #parLabel$setToolTip(
  #  obj$output()$tooltipinfo[names(obj$output()$tooltipinfo) == par])

  this$spin <- Qt$QSpinBox()
  if(type == "Negati") {
    spin$setMinimum(-999999)
    spin$setMaximum(-1)
  } else if(type == "Positi") {
    spin$setMinimum(1)
    spin$setMaximum(999999)
  } else if(type == "Nonneg") {
    spin$setMinimum(0)
    spin$setMaximum(999999)
  } else {
    spin$setMinimum(-999999)
    spin$setMaximum(0)
  }
  spin$setValue(initVal)

  # update obj when spinbox changes
  qconnect(spin, "valueChanged(int)", function(val) {
    eval(parse(text=paste("obj$",par," <- spin$value",sep="")))
  })

  lyt <- Qt$QHBoxLayout()
  #lyt$addWidget(parLabel,1,Qt$Qt$AlignRight)
  lyt$addWidget(spin)

  setLayout(lyt)
})

qsetMethod("getPar", IntWidgetQt, function() {
  par
})

qsetMethod("getValue", IntWidgetQt, function() {
  spin$value
})

qsetMethod("setValue", IntWidgetQt, function(val) {
  spin$setValue(val)
})

qsetMethod("setDefault", IntWidgetQt, function() {
  val <- obj$field(par)
  spin$setValue(val)
})



# widget to handle changing character strings
qsetClass("CharWidgetQt", Qt$QWidget, function(obj, par, parent = NULL) {
  super(parent)
  this$obj <- obj; this$par <- par

  initText <- obj$field(par)

  this$parEdit <- Qt$QLineEdit(initText)

  qconnect(parEdit, "editingFinished", function() {
    setValue(parEdit$text)
  })

  lyt <- Qt$QHBoxLayout()
  lyt$addWidget(parEdit)
  setLayout(lyt)
})

qsetMethod("getPar", CharWidgetQt, function() {
  par
})

qsetMethod("getValue", CharWidgetQt, function() {
  parEdit$text
})


qsetMethod("setValue", CharWidgetQt, function(txt) {
    parEdit$setText(txt)
    eval(parse(text=paste("obj$",par," <- parEdit$text",sep="")))
})

qsetMethod("setDefault", CharWidgetQt, function() {
  txt <- obj$field(par) 
  parEdit$setText(txt)  
})

## # function
## qsetClass("functionWidgetQt", Qt$QWidget, function(obj, par, parent = NULL) {
##   super(parent)
##   this$obj <- obj; this$par <- par

##   initText <- body(obj$field(par))[2]
##   initText <- deparse(initText)

##   this$parEdit <- Qt$QTextEdit(initText)
##   parEdit$LineWrapMode
##   sort(ls(parEdit))
##   fl <- tempfile()
##   class(initText)

##   cat(paste(initText, collapse = "\n"), file =fl)
##   system(paste("less", fl))
##    temp <- gsub("<-", "=", initText)
## Qt$QTextEdit(temp)
##   qconnect(parEdit, "editingFinished", function() {
##     setValue(parEdit$text)
##   })

##   lyt <- Qt$QHBoxLayout()
##   lyt$addWidget(parEdit)
##   setLayout(lyt)
## })

## qsetMethod("getPar", functionWidgetQt, function() {
##   par
## })

## qsetMethod("getValue", functionWidgetQt, function() {
##   parEdit$text
## })

# also updates the obj object
## qsetMethod("setValue", functionWidgetQt, function(txt) {
##     parEdit$setText(txt)
##     eval(parse(text=paste("obj$",par," <- parEdit$text",sep="")))
## })

## qsetMethod("setDefault", functionWidgetQt, function() {
##   txt <- body(obj$field(par))[2]
##   parEdit$setText(txt)  
## })

# label for a given parameter, with appropriate text and tooltip
qsetClass("ParLabel", Qt$QLabel, function(obj, par, parent = NULL) {
  super(parent)

  this$obj <- obj; this$par <- par

  ## parInfo <- obj$output()$parinfo[names(obj$output()$parinfo) == par]
  parInfo <- par
  setText(paste(parInfo,":",sep=""))
  ## setToolTip(
  ##   obj$output()$tooltipinfo[names(obj$output()$tooltipinfo) == par])
})


## http://stackoverflow.com/questions/5173692/how-to-return-number-of-decimal-places-in-r
decimalplaces <- function(x) {
    if ((x %% 1) != 0) {
        nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
    } else {
        return(0)
    }
}



setClass("ColorWidgetQt",
            contains = c("ColorWidget", "Qt"))
setMethod("widget", "ColorWidgetQt", function(obj, ...){
  ColorWidgetQt(obj, ...)
})


setClass("ColorEnumWidgetQt",
            contains = c("ColorEnumWidget", "Qt"))
setMethod("widget", "ColorEnumWidgetQt", function(obj, ...){
  ColorEnumWidgetQt(obj, ...)
})


setClass("GlyphEnumWidgetQt",
            contains = c("GlyphEnumWidget", "Qt"))
setMethod("widget", "GlyphEnumWidgetQt", function(obj, ...){
  GlyphEnumWidgetQt(obj, ...)  
})



setClass("SingleEnumWidgetQt", 
            contains = c("SingleEnumWidget", "Qt"))
setMethod("widget", "SingleEnumWidgetQt", function(obj, ...){
  SingleEnumWidgetQt(obj, ...)  
})


setClass("MultipleEnumWidgetQt",
            contains = c("MultipleEnumWidget", "Qt"))
setMethod("widget", "MultipleEnumWidgetQt", function(obj, ...){
  MultipleEnumWidgetQt(obj, ...)  
})



setClass("PositiveIntegerWidgetQt",
            contains = c("PositiveIntegerWidget", "Qt"))
setMethod("widget", "PositiveIntegerWidgetQt", function(obj, ...){
  IntWidgetQt(obj, ...)  
})



setClass("NegativeIntegerWidgetQt",
            contains = c("NegativeIntegerWidget", "Qt"))
setMethod("widget", "NegativeIntegerWidgetQt", function(obj, ...){
  IntWidgetQt(obj, ...)  
})


setClass("NonpositiveIntegerWidgetQt",
            contains = c("NonpositiveIntegerWidget", "Qt"))
setMethod("widget", "NonpositiveIntegerWidgetQt", function(obj, ...){
  IntWidgetQt(obj, ...)  
})


setClass("NonnegativeIntegerWidgetQt",
            contains = c("NonnegativeIntegerWidget", "Qt"))
setMethod("widget", "NonnegativeIntegerWidgetQt", function(obj, ...){
  IntWidgetQt(obj, ...)  
})


setClass("NumericWithRangeWidgetQt",
            contains = c("NumericWithRangeWidget", "Qt"))
setMethod("widget", "NumericWithRangeWidgetQt", function(obj, ...){
  RangeWidgetQt(obj, ...)  
})


setClass("NumericWithMin0Max1WidgetQt",
            contains = c("NumericWithMin0Max1Widget", "Qt"))
setMethod("widget", "NumericWithMin0Max1WidgetQt", function(obj, ...){
  RangeWidgetQt(obj, ...)  
})


setClass("logicalWidgetQt",
            contains = c("logicalWidget", "Qt"))
setMethod("widget", "logicalWidgetQt", function(obj, ...){
  logicalWidgetQt(obj, ...)  
})


setClass("characterWidgetQt",
          contains = c("characterWidget", "Qt"))
setMethod("widget", "characterWidgetQt", function(obj, ...){
  CharWidgetQt(obj, ...)
})



## setRefClass("factorWidgetQt",
##             methods = list(
##               widget = function(){
##                 WidgetQt(.self)
##               }
##               ),

##             contains = c("factorWidget", "Qt"))

## setRefClass("functionWidgetQt",
##             contains = c("functionWidget", "Qt"))


setMethod("Widget", "PropertySet", function(obj, ...){
  new("PropertySetWidgetQt", obj, ...)
})
setMethod("Widget", "Color", function(obj, ...){
  new("ColorWidgetQt", obj, ...)
})
setMethod("Widget", "ColorEnum", function(obj, ...){
  new("ColorEnumWidgetQt", obj, ...)
})
setMethod("Widget", "GlyphEnum", function(obj, ...){
  new("GlyphEnumWidgetQt", obj, ...)
})
setMethod("Widget", "SingleEnum", function(obj, ...){
  new("SingleEnumWidgetQt", obj, ...)
})
setMethod("Widget", "MultipleEnum", function(obj, ...){
  new("MultipleEnumWidgetQt", obj, ...)
})
setMethod("Widget", "PositiveInteger", function(obj, ...){
  new("PositiveIntegerWidgetQt", obj, ...)
})
setMethod("Widget", "NegativeInteger", function(obj, ...){
  new("NegativeIntegerWidgetQt", obj, ...)
})
setMethod("Widget", "NonpositiveInteger", function(obj, ...){
  new("NonpositiveIntegerWidgetQt", obj, ...)
})
setMethod("Widget", "NonnegativeInteger", function(obj, ...){
  new("NonnegativeIntegerWidgetQt", obj, ...)
})
setMethod("Widget", "NumericWithRange", function(obj, ...){
  new("NumericWithRangeWidgetQt", obj, ...)
})
setMethod("Widget", "NumericWithMin0Max1", function(obj, ...){
  new("NumericWithMin0Max1WidgetQt", obj, ...)
})


setMethod("Widget", "character", function(obj, ...){
  new("characterWidgetQt", obj, ...)
})

setMethod("Widget", "logical", function(obj, ...){
  new("logicalWidgetQt", obj, ...)
})

## setMethod("widget", "ANY", function(obj, ...){
##   Widget(obj)$widget()
## })


## setMethod("Widget", "factor", function(obj, ...){
##   new("factorWidgetQt", ...)
## })

## setMethod("Widget", "function", function(obj, ...){
##   new("functionWidgetQt", ...)
## })
