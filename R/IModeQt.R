setRefClass("IModeGroupWidgetQt", contains = c("Qt", "IModeGroupWidget"),
            methods = list(
                  widget = function(id){
                    if(missing(id))
                      id <- id()
                    if(length(id)>1){
                      warning("only support exlusive mode now, use the first")
                      id <- id[1]
                    }
                   ControlPanel(items[[id]]$pars)
                  },
                 menu = function(w){
                    if(missing(w))
                      w <- Qt$QMainWindow()
                    if(length(id()) < 1) setId(defaultId)
                    ##create actions for each item in group
                    m <- Qt$QMenu(text)
                    actionList <- list()
                    length(actionList) <- length(items)
                    actionGroup <- Qt$QActionGroup(w)
                    actionGroup$setExclusive(exclusive)

                    ## put default ID in list first, then add separator
                    dID <- defaultId
                    actionList[[dID]] <- Qt$QAction(items[[dID]]$text, actionGroup)
                    actionList[[dID]]$setCheckable(TRUE)
                    actionList[[dID]]$setChecked(TRUE)
                    qconnect(actionList[[dID]], "triggered", function(){
                      setId(dID)
                    })
                    actionGroup$addAction(actionList[[dID]])
                    m$addAction(actionList[[dID]])
                    m$addSeparator()
                    sapply((1:length(items))[-dID], function(i) {
                      actionList[[i]] <- Qt$QAction(items[[i]]$text, actionGroup)
                      actionList[[i]]$setCheckable(TRUE)
                      qconnect(actionList[[i]], "triggered", function(){
                        setId(i)
                      })
                      actionGroup$addAction(actionList[[i]])
                      m$addAction(actionList[[i]])
                    })
                    m
                 }
              ))

IModeGroupWidgetQt <- function(scaleMode = ScaleMode(),
                       brushMode = BrushMode(),
                       identifyMode = IdentifyMode(),
                       exclusive = TRUE,
                       text = "Interaction Group"){
  items <- ItemList(scaleMode = scaleMode,
                    brushMode = brushMode,
                    identifyMode = identifyMode)
  obj <- new("IModeGroupWidgetQt", items = items, exclusive = exclusive)
  obj$setText(text)
  obj
}

