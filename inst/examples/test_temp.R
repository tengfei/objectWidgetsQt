library(objectProperties)
library(objectWidgets)
library(qtbase)

new.gen <- setNumericWithRange(min = 0.005, max = 0.006)

gpar.gen <- setRefClass("GraphicPars", fields = properties(list(a = new.gen@className),
                                            prototype = list(a = new(new.gen@className, 0.0055))),
                        contains = "PropertySet")


obj <- gpar.gen$new()
ControlPanel(obj, title = "hello world", decimal.extra = 4)


new.gen <- setIntegerWithRange(min = 5L, max = 1000L)

gpar.gen <- setRefClass("GraphicPars", fields = properties(list(a = new.gen@className),
                                            prototype = list(a = new(new.gen@className, 55))),
                        contains = "PropertySet")

obj <- gpar.gen$new()
ControlPanel(obj, title = "hello world", decimal.extra = 4)


geomSingleEnum <- setSingleEnum("geom", c("rect", "triangle"))
obj <- geomSingleEnum("rect")

obj.gen <- setRefClass("test", fields = properties(list(a = "logical",
                                 b = "function")), contains = "PropertySet")
obj <- obj.gen$new(a = TRUE, b = cat)
ControlPanel(obj)
