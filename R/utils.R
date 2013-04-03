setGeneric("getQtEnum", function(x,...) standardGeneric("getQtEnum"))
setMethod("getQtEnum", "DragModeSingleEnum", function(x){
  val <- switch(x,
                NoDrag = Qt$QGraphicsView$NoDrag,
                ScrollHandDrag = Qt$QGraphicsView$ScrollHandDrag,
                RubberBandDrag = Qt$QGraphicsView$RubberBandDrag)
})


