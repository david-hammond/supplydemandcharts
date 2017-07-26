
approxIntersection <- function(path1, path2){
  # Helper function to identify approximate curve intersections by brute force
  distanceMatrix <- proxy::dist(path1, path2)
  whichMin <- which(distanceMatrix == min(distanceMatrix), arr.ind = TRUE)
  return((path1[whichMin[1], ]+path2[whichMin[2], ])/2)
}  # This is where a long bezier() output vector is useful

supply.demand = function(y.axis = "Price", x.axis = "Quantity"){
  require(Hmisc)
  require(ggplot2)
  require(proxy)
  require(grid)
  # Replicating http://en.wikipedia.org/w/index.php?title=File:Supply-demand-right-shift-demand.svg&page=1
  x <- c(1, 8, 9)
  y <- c(1, 5, 9)
  supply1 <- data.frame(bezier(x, y, evaluation = 500))
  x <- c(1, 3, 9)
  y <- c(9, 3, 1)
  demand1 <- data.frame(bezier(x, y, evaluation = 500))

  intersection <- approxIntersection(supply1, demand1)

  textAnnotations <- data.frame(label = c("S1", "D1"),
                                x = c(8, 1),  # DF of line labels
                                y = c(8, 8))

  p <- qplot(x = 0:10, y = 0:10, geom = "blank")  # Draw an empty plot
  p <- p + geom_path(data = supply1, aes(x = x, y = y),  # Add supply curve
                         size = 1, colour = "BLUE")
  p <- p + geom_path(data = demand1, aes(x = x, y = y),  # Add demand 1
                         size = 1, colour = "RED")
  p <- p + geom_point(data = intersection,  # Add points at intersections
                          aes(x = x, y = y), size = 3)
  p <- p + geom_segment(data = intersection,  # Add dotted lines
                            aes(x = x, y = 0, xend = x, yend = y),
                            lty = 2)
  p <- p + geom_segment(data = intersection,  # Add dotted lines
                            aes(x = 0, y = y, xend = x, yend = y),
                            lty = 2)
  p <- p + geom_text(data = textAnnotations,  # Add curve labels
                         aes(x = x, y = y, label = label))
  p <- p + scale_x_continuous(x.axis, expand = c(0, 0),  # Clean up axis
                                  breaks = intersection$x,
                                  labels = expression(Q[1]))
  p <- p + scale_y_continuous(y.axis, expand = c(0, 0),  # Clean up axis
                                  breaks = intersection$y,
                                  labels = expression(P[1]))
  p <- p + theme_classic()  # New in ggplot2 0.9.3. Time to update!
  p <- p + coord_equal()  # Force fixed x-y relationship
  p$supply = supply1
  p$demand = demand1
  p$intersection = intersection
  return(p)
}
supply.demand.consumer.surplus = function(y.axis = "Price", x.axis = "Quantity",
                                          consumer.surplus = "Consumer Surplus",
                                          producer.surplus = "Producer Surplus"){
  require(Hmisc)
  require(ggplot2)
  require(proxy)
  require(grid)
  # Replicating http://en.wikipedia.org/w/index.php?title=File:Supply-demand-right-shift-demand.svg&page=1
  x <- c(1, 8, 9)
  y <- c(1, 5, 9)
  supply1 <- data.frame(bezier(x, y, evaluation = 500))
  x <- c(1, 3, 9)
  y <- c(9, 3, 1)
  demand1 <- data.frame(bezier(x, y, evaluation = 500))

  intersection <- approxIntersection(supply1, demand1)

  textAnnotations <- data.frame(label = c("S1", "D1", consumer.surplus, producer.surplus),
                                x = c(8, 1, 2, 2),  # DF of line labels
                                y = c(8, 8, 4, 3))

  p <- qplot(x = 0:10, y = 0:10, geom = "blank")  # Draw an empty plot
  p <- p + geom_path(data = supply1, aes(x = x, y = y),  # Add supply curve
                     size = 1, colour = "BLUE")
  p <- p + geom_path(data = demand1, aes(x = x, y = y),  # Add demand 1
                     size = 1, colour = "RED")
  p <- p + geom_point(data = intersection,  # Add points at intersections
                      aes(x = x, y = y), size = 3)
  p <- p + geom_segment(data = intersection,  # Add dotted lines
                        aes(x = x, y = 0, xend = x, yend = y),
                        lty = 2)
  p <- p + geom_segment(data = intersection,  # Add dotted lines
                        aes(x = 0, y = y, xend = x, yend = y),
                        lty = 2)
  p <- p + geom_text(data = textAnnotations,  # Add curve labels
                     aes(x = x, y = y, label = label))
  p <- p + scale_x_continuous(x.axis, expand = c(0, 0),  # Clean up axis
                              breaks = intersection$x,
                              labels = expression(Q[1]))
  p <- p + scale_y_continuous(y.axis, expand = c(0, 0),  # Clean up axis
                              breaks = intersection$y,
                              labels = expression(P[1]))
  p <- p + theme_classic()  # New in ggplot2 0.9.3. Time to update!
  p <- p + coord_equal()  # Force fixed x-y relationship
  p$supply = supply1
  p$demand = demand1
  p$intersection = intersection
  return(p)
}
supply.demand.blank = function(){
  require(Hmisc)
  require(ggplot2)
  require(proxy)
  require(grid)
  # Replicating http://en.wikipedia.org/w/index.php?title=File:Supply-demand-right-shift-demand.svg&page=1
  x <- c(1, 8, 9)
  y <- c(1, 5, 9)
  supply1 <- data.frame(bezier(x, y, evaluation = 500))
  x <- c(1, 3, 9)
  y <- c(9, 3, 1)
  demand1 <- data.frame(bezier(x, y, evaluation = 500))

  intersection <- approxIntersection(supply1, demand1)

  textAnnotations <- data.frame(label = c("S1", "D1"),
                                x = c(8, 1),  # DF of line labels
                                y = c(8, 8))

  p <- qplot(x = 0:10, y = 0:10, geom = "blank")  # Draw an empty plot
  p <- p + geom_path(data = supply1, aes(x = x, y = y),  # Add supply curve
                     size = 1, colour = "BLUE")
  p <- p + geom_path(data = demand1, aes(x = x, y = y),  # Add demand 1
                     size = 1, colour = "RED")
  p <- p + geom_point(data = intersection,  # Add points at intersections
                      aes(x = x, y = y), size = 3)
  p <- p + geom_text(data = textAnnotations,  # Add curve labels
                     aes(x = x, y = y, label = label))
  p <- p + theme_classic()  # New in ggplot2 0.9.3. Time to update!
  p <- p + coord_equal()  # Force fixed x-y relationship
  p$supply = supply1
  p$demand = demand1
  p$intersection = intersection
  return(p)
}

shift.demand = function(direction = "right", y.axis = "Price", x.axis = "Quantity"){
  p = supply.demand.blank()
  shift = ifelse(direction == "right",2,-2)
  shift.x = ifelse(direction == "right",5,1)
  shift.y = ifelse(direction == "right",8,4)
  axis.x = ifelse(direction == "right",0,0)
  demand2 <- p$demand + shift
  intersectS1D2 <- approxIntersection(p$supply, demand2)
  intersection <- data.frame(rbind(p$intersection, intersectS1D2))


  textAnnotations <- data.frame(label = c("D2"),
                                x = shift.x,  # DF of line labels
                                y = shift.y)
  p = p + geom_path(data = demand2, aes(x = x, y = y),  # Add demand 2
                    size = 1, colour = "RED")
  p = p + geom_point(data = intersection,  # Add points at intersections
                              aes(x = x, y = y), size = 3)
  p <- p + geom_segment(data = intersection,  # Add dotted lines
                            aes(x = x, y = -1, xend = x, yend = y),
                            lty = 2)
  p <- p + geom_segment(data = intersection,  # Add dotted lines
                            aes(x = axis.x, y = y, xend = x, yend = y),
                            lty = 2)
  p <- p + geom_text(data = textAnnotations,  # Add curve labels
                         aes(x = x, y = y, label = label))
  p <- p + scale_x_continuous(x.axis, expand = c(0, 0),
                              limits = c(axis.x,10),# Clean up axis
                                  breaks = intersection$x,
                                  labels = expression(Q[1], Q[2]))
  p <- p + scale_y_continuous(y.axis, expand = c(0, 0),  # Clean up axis
                                  breaks = intersection$y,
                                  labels = expression(P[1],P[2]))
  return(p)
}

shift.supply = function(direction = "right", y.axis = "Price", x.axis = "Quantity"){
  p = supply.demand.blank()
  shift = ifelse(direction == "right",2,-2)
  supply2 <- p$supply
  supply2$x = supply2$x + shift
  shift.x = ifelse(direction == "right",9,6)
  shift.y = ifelse(direction == "right",6,8)
  axis.x = ifelse(direction == "right",0,0)

  intersectS1D2 <- approxIntersection(supply2, p$demand)
  intersection <- data.frame(rbind(p$intersection, intersectS1D2))


  textAnnotations <- data.frame(label = c("S2"),
                                x = shift.x,  # DF of line labels
                                y = shift.y)
  p = p + geom_path(data = supply2, aes(x = x, y = y),  # Add demand 2
                    size = 1, colour = "BLUE")
  p = p + geom_point(data = intersection,  # Add points at intersections
                     aes(x = x, y = y), size = 3)
  p <- p + geom_segment(data = intersection,  # Add dotted lines
                        aes(x = x, y = 0, xend = x, yend = y),
                        lty = 2)
  p <- p + geom_segment(data = intersection,  # Add dotted lines
                        aes(x = axis.x, y = y, xend = x, yend = y),
                        lty = 2)
  p <- p + geom_text(data = textAnnotations,  # Add curve labels
                     aes(x = x, y = y, label = label))
  p <- p + scale_x_continuous(x.axis, expand = c(0, 0),  # Clean up axis
                              limits = c(axis.x,10),# Clean up axis
                              breaks = intersection$x,
                              labels = expression(Q[1], Q[2]))
  p <- p + scale_y_continuous(y.axis, expand = c(0, 0),  # Clean up axis
                              breaks = intersection$y,
                              labels = expression(P[1],P[2]))
  return(p)
}

