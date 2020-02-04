
########################################## LIBRARY ##########################################

#install.packages("devtools")
#install.packages("plumber")
#install.packages("rgl")
#library(devtools)
#devtools::install_github('delta1epsilon/BoxPacking')
#library(BoxPacking)
#library(jsonlite)
library(rgl)
library(plumber)

########################################## GET / ##########################################

#* @get /
home <- function () {
  return("WELCOME TO LOAD OPTIMIZATION - PAKET ID")
}


########################################## POST /LOADOPT ##########################################

#* @post /loadopt     
loadopt <- function(boxes, containers) {
  ########################################## EMS ##########################################
  
  #' Create Empty Maximal Spaces
  #'
  #' @param container - An object of class 'Container' or 'EMS' 
  #' @param box       - An object of class 'Box'
  #'  
  #' @return A list of instance of class EMS or empty list
  #' @examples 
  #' container <- Container(width = 2, length = 4, height = 2)
  #' box <- Box(width = 1, length = 1, height = 1, origin = c(0, 0,0))
  #' 
  #' CreateEMS(container, box)
    CreateEMS <- function (container, 
                         box) {
    
    # stop if box origin is not specified
    if (length(box@origin) == 0) {
      stop('Specify origin for the box')
    }
    
    #' Check if the box is otside the container
    #'
    #' @param container - An object of class 'Container' or 'EMS'
    #' @param box       - An object of class 'Box'
    #' @return TRUE/FALSE 
    CheckIfBoxIsOutside <- function (container, box) {
      # get top and bottom vertexes of the container
      container_vertex1 <- container@origin
      container_vertex2 <- container@origin + c(container@length, container@height, container@width)
      
      # get 4 vertexes of the box
      box_vertex1 <- box@origin
      box_vertex2 <- box@origin + c(0, box@height, 0)
      box_vertex3 <- box@origin + c(0, 0, box@width)
      box_vertex4 <- box@origin + c(box@length, 0, 0)
      
      is_outside <- 
        (container_vertex1[2] >= box_vertex2[2] |
           container_vertex2[2] <= box_vertex1[2] |
           container_vertex2[1] <= box_vertex1[1] |
           container_vertex1[1] >= box_vertex4[1] |
           container_vertex1[3] >= box_vertex3[3] |
           container_vertex2[3] <= box_vertex1[3])
      
      return(is_outside)
    }
    
    #' Check if EMS has nonzero parameters
    #'
    #' @param EMS - An instance of class EMS
    #' @return TRUE if EMS is valid (all parameters are nonzero) otherwise FALSE
    CheckIfEMSvalid <- function (EMS_object) {
      if (EMS_object@length <= 0 | EMS_object@height <= 0 | EMS_object@width <= 0 ) {
        return(FALSE)
      } else {
        return(TRUE)
      }
    }
    
    
    if (CheckIfBoxIsOutside(container, box)) {
      return(list())        
    }
    
    ems_list <- list()
    
    if (box@stack == 0) {
      # EMS 1:
      EMS1 <- EMS(origin = container@origin, 
                  length = box@origin[1] - container@origin[1],
                  height = container@height,
                  width = container@width
      )
      
      if (CheckIfEMSvalid(EMS1)) {
        ems_list <- c(ems_list, EMS1)
      }
      
      # EMS 2:
      EMS2 <- EMS(origin = container@origin, 
                  length = container@length,
                  height = container@height,
                  width = box@origin[3] - container@origin[3]
      )
      
      if (CheckIfEMSvalid(EMS2)) {
        ems_list <- c(ems_list, EMS2)
      }
      
      # EMS 3:
      EMS3 <- EMS(origin = c(box@origin[1], container@origin[2:3]) + c(box@length, 0, 0), 
                  height = container@height,
                  width = container@width
      )
      EMS3@length <- ((container@origin + c(container@length, 0, 0)) - EMS3@origin)[1]
      
      if (CheckIfEMSvalid(EMS3)) {
        ems_list <- c(ems_list, EMS3)
      }
      
      # EMS 4:
      EMS4 <- EMS(origin = c(container@origin[1:2], box@origin[3]) + c(0, 0, box@width), 
                  length = container@length,
                  height = container@height
      )
      EMS4@width <- ((container@origin + c(0, 0, container@width)) - EMS4@origin)[3]
      
      if (CheckIfEMSvalid(EMS4)) {
        ems_list <- c(ems_list, EMS4)
      }
      
      # EMS 5:
      "EMS5 <- EMS(origin = c(box@origin[1], box@origin[2], box@origin[3]) + c(0, box@height, 0), # fix box melayang
                  length = box@length, # fix box ditaro di ems yg lbh kecil
                  width = box@width
      )
      EMS5@height <- ((container@origin + c(0, container@height, 0)) - EMS5@origin)[2]
      
      if (CheckIfEMSvalid(EMS5)) {
        ems_list <- c(ems_list, EMS5)
      }"
      
      # EMS 6:
      EMS6 <- EMS(origin = container@origin, 
                  length = container@length,
                  height = box@origin[2] - container@origin[2],
                  width = container@width
      )
      
      if (CheckIfEMSvalid(EMS6)) {
        ems_list <- c(ems_list, EMS6)
      }
    }
    
    if (box@stack == 1) {
      # EMS 1:
      EMS1 <- EMS(origin = container@origin, 
                  length = box@origin[1] - container@origin[1],
                  height = container@height,
                  width = container@width
      )
      
      if (CheckIfEMSvalid(EMS1)) {
        ems_list <- c(ems_list, EMS1)
      }
      
      # EMS 2:
      EMS2 <- EMS(origin = container@origin, 
                  length = container@length,
                  height = container@height,
                  width = box@origin[3] - container@origin[3]
      )
      
      if (CheckIfEMSvalid(EMS2)) {
        ems_list <- c(ems_list, EMS2)
      }
      
      # EMS 3:
      EMS3 <- EMS(origin = c(box@origin[1], container@origin[2:3]) + c(box@length, 0, 0), 
                  height = container@height,
                  width = container@width
      )
      EMS3@length <- ((container@origin + c(container@length, 0, 0)) - EMS3@origin)[1]
      
      if (CheckIfEMSvalid(EMS3)) {
        ems_list <- c(ems_list, EMS3)
      }
      
      # EMS 4:
      EMS4 <- EMS(origin = c(container@origin[1:2], box@origin[3]) + c(0, 0, box@width), 
                  length = container@length,
                  height = container@height
      )
      EMS4@width <- ((container@origin + c(0, 0, container@width)) - EMS4@origin)[3]
      
      if (CheckIfEMSvalid(EMS4)) {
        ems_list <- c(ems_list, EMS4)
      }
      
      # EMS 5:
      EMS5 <- EMS(origin = c(box@origin[1], box@origin[2], box@origin[3]) + c(0, box@height, 0), # fix box melayang
                  length = box@length, # fix box ditaro di ems yg lbh kecil
                  width = box@width
      )
      EMS5@height <- ((container@origin + c(0, container@height, 0)) - EMS5@origin)[2]
      
      if (CheckIfEMSvalid(EMS5)) {
        ems_list <- c(ems_list, EMS5)
      }
      
      # EMS 6:
      EMS6 <- EMS(origin = container@origin, 
                  length = container@length,
                  height = box@origin[2] - container@origin[2],
                  width = container@width
      )
      
      if (CheckIfEMSvalid(EMS6)) {
        ems_list <- c(ems_list, EMS6)
      }
    }
    
    
    return(ems_list)
  }
  
  
  #' Check if an EMS is totally inscribed by other EMS
  #'
  #' @param ems_to_check - An object of class EMS to check if it's in ems
  #' @param ems          - An object of class EMS
  #' @return TRUE if ems_to_check is inside ems otherwise FALSE
  CheckIfEMSisInsideOtherEMS <- function (ems_to_check, ems) {
    # get 2 vertexes of ems_to_check
    ems_to_check_vertex1 <- ems_to_check@origin
    ems_to_check_vertex2 <- 
      ems_to_check@origin + c(ems_to_check@length, ems_to_check@height, ems_to_check@width)
    
    # get 2 vertexes of ems
    ems_vertex1 <- ems@origin
    ems_vertex2 <- ems@origin + c(ems@length, ems@height, ems@width)
    
    ems_is_inside <- 
      all(ems_to_check_vertex1 >= ems_vertex1) &
      all(ems_to_check_vertex2 <= ems_vertex2)
    
    if (ems_is_inside) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  
  
  #' Remove those EMS from list which are inside other EMS
  #'
  #' @param ems_list - A list of objects of class EMS
  #' @return An updated list of EMS objects
  EliminateEMSList <- function (ems_list) {
    sequence <- 1:length(ems_list)
    ind_remove <- c()
    
    for (i in sequence) {
      ems <- ems_list[[i]]
      
      for (j in setdiff(sequence, i)) {
        if (CheckIfEMSisInsideOtherEMS(ems, ems_list[[j]])) {
          # the ems in inside other ems
          ind_remove <- c(ind_remove, i)
          break
        }
      }
    }
    
    if (length(ind_remove) != 0) {
      ems_list <- ems_list[-ind_remove]
    }
    
    return(ems_list) 
  }
  
  
  #' Check If the Box equals to the Container
  #'
  #' @param box - An object of class Box
  #' @param ems - An object of class EMS
  #' @return TRUE/FALSE
  CheckIfBoxEqualsEMS <- function (box, ems) {
    BoxEqualsEMS <- 
      (all(box@origin == ems@origin) &
         box@length == ems@length &
         box@height == ems@height &
         box@width == ems@width
      )
    return(BoxEqualsEMS)
  }
  
  
  #' Update list of container's EMS after box is placed
  #'
  #' @param ems_list - A list of objects of class EMS 
  #' @param box      - An object of class Box
  #'
  #' @return A list of objects of class EMS
  UpdateEMS <- function (ems_list, box) {
    new_ems_list <- ems_list
    
    # indeces of EMS that are going to be updated and 
    # therefore replaced by it's update 
    ind_to_remove <- c()
    
    for (i in 1:length(ems_list)) {
      ems <- ems_list[[i]]
      
      new_ems <- CreateEMS(ems, box)
      if (length(new_ems) != 0 | CheckIfBoxEqualsEMS(box, ems)) {
        ind_to_remove <- c(ind_to_remove, i)
        new_ems_list <- c(new_ems_list, new_ems)
      }
      
      if (length(new_ems) == 0 &  # fixing ems hilang krn box unstackable
          box@stack == 0 &
          box@origin[[1]] == ems@origin[[1]] &
          box@origin[[2]] == ems@origin[[2]] &
          box@origin[[3]] == ems@origin[[3]] &
          box@length == ems@length &
          box@width == ems@width) {
        ind_to_remove <- c(ind_to_remove, i)
        new_ems_list <- c(new_ems_list, new_ems)
      }
    } 
    
    # remove EMS that were updated
    if (length(ind_to_remove) != 0) {
      new_ems_list <- new_ems_list[-ind_to_remove]
    }
    
    # break when there is no ems left  # fixing box terakhir ukuran sama dengan ems
    if (all(is.na(new_ems_list))) {
      return(new_ems_list)
    }
    else {
      # remove EMS that are inside other EMS
      new_ems_list <- EliminateEMSList(new_ems_list)
      return(new_ems_list)
    }
  }
  
  
  #' Prioritize EMS in EMS list (sort EMS by distance to the box origin)
  #'
  #' @param ems_list - A list with objects of class EMS
  #' @return A list with objects of class EMS
  PrioritizeEMS <- function (ems_list) {
    
    #' Calculate distance (Euclidean) from EMS origin to Box origin
    #' 
    #' @param ems - An object of class EMS
    #' @return A numeric
    CalculateDistanceToBoxOrigin <- function (ems) {
      distance <- sqrt(sum(ems@origin^2)) 
      return(distance)
    }
    
    # calculate distance for each EMS in ems_list
    distances <- sapply(ems_list, CalculateDistanceToBoxOrigin)
    
    # compute EMS order
    ems_order <- order(distances)
    
    # prioritize EMS
    ems_list <- ems_list[ems_order]
    return(ems_list)
  }
  
  ########################################## objects definition ##########################################
  
  #' An s4 class to represent a Container
  #'
  #' @slot origin A length-three vector 
  #' @slot length A numeric
  #' @slot height A numeric
  #' @slot width A numeric
  #'
  #' @examples 
  #' # create a container with size 2 x 2 x 2
  #' c1 <- Container(length = 2, height = 2, width = 2)
  #'
  #' @export Container
  Container <- setClass('Container',
                        slots = c(origin = 'numeric',
                                  length = 'numeric',
                                  height = 'numeric',
                                  width = 'numeric',
                                  ems = 'list'  # list of instances of class EMS 
                        ),
                        prototype = list(origin = c(0, 0, 0)),
                        validity = function (object) {  # make sure that all parameters are positive
                          if(object@length <= 0 | object@height <= 0 | object@width <= 0) {
                            return("A number <= 0 for one of the parameters was given.")
                          }
                          return(TRUE)
                        }
  )
  
  # create method 'initialize' for Container class to create 
  # slot EMS (initial EMS is whole Container) when
  # an instance of class Container is being created 
  #' @export 
  setMethod('initialize', 
            'Container',
            function (.Object, ...) {
              .Object <- callNextMethod()
              .Object@ems <- 
                list(
                  EMS(origin = .Object@origin,
                      length = .Object@length,
                      height = .Object@height,
                      width = .Object@width
                  )
                )
              return(.Object)
            }
  )
  
  #' An s4 class to represent a Box
  #'
  #' @slot origin A length-three vector 
  #' @slot length A numeric
  #' @slot height A numeric
  #' @slot width A numeric
  #' @slot weight A numeric
  #'
  #' @examples 
  #' # create a box with size 1 x 1 x 1
  #' b1 <- Box(length = 1, height = 1, width = 1)
  #'
  #' @export Box
  Box <- setClass('Box',
                  slots = c(origin = 'numeric',
                            length = 'numeric',
                            height = 'numeric',
                            width = 'numeric',
                            weight = 'numeric',
                            rotate = 'numeric',  # tambah parameter box tdk bs dijatuhkan
                            stack = 'numeric'  # tambah parameter box tdk bs ditumpuk
                  ),
                  validity = function (object) {  # make sure that all parameters are positive
                    if(object@length <= 0 | object@height <= 0 | object@width <= 0) {
                      return("A number <= 0 for one of the parameters was given.")
                    }
                    return(TRUE)
                  }
  ) 
  
  # define class for Empty Maximal Spaces
  #' @export EMS
  EMS <- setClass('EMS',
                  slots = c(origin = 'numeric',
                            length = 'numeric',
                            height = 'numeric',
                            width = 'numeric'
                  )
  )
  
  ########################################## operations on boxes ##########################################
  
  #' Check If the Box fits into the EMS
  #'
  #' @param box - An object of class Box
  #' @param ems - An object of class EMS
  #' @return TRUE/FALSE
  CheckIfBoxFitsIntoEMS <- function (box, ems) {
    if (box@rotate == 0) {  # box tidak bisa dijatuhkan -> hilangkan fit2
      # check if the box originaly fits
      fit1 <-
        (box@length <= ems@length & 
           box@height <= ems@height & 
           box@width <= ems@width)
      
      "# check if the box fits after rotation 1
      fit2 <-
       (box@height <= ems@length &
          box@length <= ems@height &
          box@width <= ems@width)"
      
      # check if the box fits after rotation 2
      fit3 <-
        (box@width <= ems@length & 
           box@height <= ems@height & 
           box@length <= ems@width)
      
      fits <- (fit1 | fit3)
      return(fits)
    }
    if (box@rotate == 1) {  # box bisa dijatuhkan -> normal
      # check if the box originaly fits
      fit1 <-
        (box@length <= ems@length & 
           box@height <= ems@height & 
           box@width <= ems@width)
      
      # check if the box fits after rotation 1
      fit2 <-
        (box@height <= ems@length & 
           box@length <= ems@height & 
           box@width <= ems@width)
      
      # check if the box fits after rotation 2
      fit3 <-
        (box@width <= ems@length & 
           box@height <= ems@height & 
           box@length <= ems@width)
      
      fits <- (fit1 | fit2 | fit3)
      return(fits)
    }
  }
  
  
  #' Perform Best Placement Selection
  #'
  #' @param box - An object of class Box
  #' @param ems - An object of class EMS
  #' @return An object of class Box
  PerformPlacementSelection <- function (box, ems) {
    possible_rotations <- list()
    possible_margins <- list()
    
    if (box@rotate == 0) {  # box tidak bisa dijatuhkan -> hilangkan rotation 1
      # original fit
      if (box@length <= ems@length & 
          box@height <= ems@height & 
          box@width <= ems@width) {
        margins <- c(ems@length - box@length, 
                     ems@height - box@height,
                     ems@width - box@width
        )
        possible_rotations <- c(possible_rotations, list(box))
        possible_margins <- c(possible_margins, list(margins))
      }
      
      "# rotation 1
      if (box@height <= ems@length & 
          box@length <= ems@height & 
          box@width <= ems@width) {
        margins <- c(ems@length - box@height, 
                     ems@height - box@length,
                     ems@width - box@width
        )
        rotated_box <- box
        rotated_box@length <- box@height 
        rotated_box@height <- box@length 
      
        possible_rotations <- c(possible_rotations, list(rotated_box))
        possible_margins <- c(possible_margins, list(margins))    
      }"
      
      # rotation 2
      if (box@width <= ems@length & 
          box@height <= ems@height & 
          box@length <= ems@width) {
        margins <- c(ems@length - box@width, 
                     ems@height - box@height,
                     ems@width - box@length
        )
        rotated_box <- box
        rotated_box@length <- box@width 
        rotated_box@width <- box@length
        
        possible_rotations <- c(possible_rotations, list(rotated_box))
        possible_margins <- c(possible_margins, list(margins))    
      }
    }
    
    if (box@rotate == 1) {  # box bisa dijatuhkan -> normal
      # original fit
      if (box@length <= ems@length & 
          box@height <= ems@height & 
          box@width <= ems@width) {
        margins <- c(ems@length - box@length, 
                     ems@height - box@height,
                     ems@width - box@width
        )
        possible_rotations <- c(possible_rotations, list(box))
        possible_margins <- c(possible_margins, list(margins))
      }
      
      # rotation 1
      if (box@height <= ems@length & 
          box@length <= ems@height & 
          box@width <= ems@width) {
        margins <- c(ems@length - box@height, 
                     ems@height - box@length,
                     ems@width - box@width
        )
        rotated_box <- box
        rotated_box@length <- box@height 
        rotated_box@height <- box@length 
        
        possible_rotations <- c(possible_rotations, list(rotated_box))
        possible_margins <- c(possible_margins, list(margins))    
      }
      
      # rotation 2
      if (box@width <= ems@length & 
          box@height <= ems@height & 
          box@length <= ems@width) {
        margins <- c(ems@length - box@width, 
                     ems@height - box@height,
                     ems@width - box@length
        )
        rotated_box <- box
        rotated_box@length <- box@width 
        rotated_box@width <- box@length
        
        possible_rotations <- c(possible_rotations, list(rotated_box))
        possible_margins <- c(possible_margins, list(margins))    
      }
    }
    
    if (length(possible_rotations) == 0) {
      stop('The box does not fit into the EMS')
    } else {
      # select rotation with smalest margin
      best_ind <- which.min(sapply(possible_margins, min))    
      rotated_box <- possible_rotations[[best_ind]]
    }
    
    return(rotated_box)
  }
  ########################################## operations on chromosomes ##########################################
  
  #' Create a Chromosome
  #'
  #' @param n_boxes      - An integer
  #' @param n_containers - An integer
  #' @return A list with first element BPS (Box Plasement Sequence) 
  #'         and second element CLS (Container Loading Sequence) and
  #'         their appropriate sequences
  CreateChromosome <- function (n_boxes, n_containers) {
    chromosome <- list(BPS = sample(1:n_boxes), CLS = sample(1:n_containers))
    return(chromosome)
  }
  
  
  #' Create 4 Initial Custom Chromosomes
  #'
  #' @param boxes - A list of objects of class Box
  #' @return A list of 4 chromosomes 
  CustomChromosomeInitialization <- function (boxes, n_containers) {
    chromosomes <- list()  # mengubah urutan penempatan box, yaitu stackable lalu unstackable
    
    boxes_1 <- NULL
    boxes_length_1 <- NULL
    boxes_width_1 <- NULL
    boxes_height_1 <- NULL
    boxes_volume_1 <- NULL
    for (box in boxes) {
      if (box@stack == 1) {
        boxes_1 <- c(boxes_1, box)
        boxes_length_1 <- c(boxes_length_1, box@length)
        boxes_width_1 <- c(boxes_width_1, box@width)
        boxes_height_1 <- c(boxes_height_1, box@height)
        boxes_volume_1 <- c(boxes_volume_1, box@length * box@height * box@width)
      }
      if (box@stack == 0) {
        boxes_length_1 <- c(boxes_length_1, NA)
        boxes_width_1 <- c(boxes_width_1, NA)
        boxes_height_1 <- c(boxes_height_1, NA)
        boxes_volume_1 <- c(boxes_volume_1, NA)
      }
    }
    m_1 <- length(boxes_1)
    
    
    boxes_0 <- NULL
    boxes_length_0 <- NULL
    boxes_width_0 <- NULL
    boxes_height_0 <- NULL
    boxes_volume_0 <- NULL
    for (box in boxes) {
      if (box@stack == 1) {
        boxes_length_0 <- c(boxes_length_0, NA)
        boxes_width_0 <- c(boxes_width_0, NA)
        boxes_height_0 <- c(boxes_height_0, NA)
        boxes_volume_0 <- c(boxes_volume_0, NA)
      }
      if (box@stack == 0) {
        boxes_0 <- c(boxes_0, box)
        boxes_length_0 <- c(boxes_length_0, box@length)
        boxes_width_0 <- c(boxes_width_0, box@width)
        boxes_height_0 <- c(boxes_height_0, box@height)
        boxes_volume_0 <- c(boxes_volume_0, box@length * box@height * box@width)
      }
    }
    m_0 <- length(boxes_0)
    
    
    # sort descending according to volume
    ind_by_volume_1 <- order(boxes_volume_1, decreasing = TRUE)
    ind_by_volume_1 <- ind_by_volume_1[1:m_1]
    ind_by_volume_0 <- order(boxes_volume_0, decreasing = TRUE)
    ind_by_volume_0 <- ind_by_volume_0[1:m_0]
    ind_by_volume <- c(ind_by_volume_1, ind_by_volume_0)
    chromosome <- list(BPS = ind_by_volume, CLS = order(1:n_containers))
    chromosomes <- c(chromosomes, list(chromosome))
    
    # sort descending according to length
    ind_by_length_1 <- order(boxes_length_1, decreasing = TRUE)
    ind_by_length_1 <- ind_by_length_1[1:m_1]
    ind_by_length_0 <- order(boxes_length_0, decreasing = TRUE)
    ind_by_length_0 <- ind_by_length_0[1:m_0]
    ind_by_length <- c(ind_by_length_1, ind_by_length_0)
    chromosome <- list(BPS = ind_by_length, CLS = order(1:n_containers))
    chromosomes <- c(chromosomes, list(chromosome))
    
    # sort descending according to height
    ind_by_height_1 <- order(boxes_height_1, decreasing = TRUE)
    ind_by_height_1 <- ind_by_height_1[1:m_1]
    ind_by_height_0 <- order(boxes_height_0, decreasing = TRUE)
    ind_by_height_0 <- ind_by_height_0[1:m_0]
    ind_by_height <- c(ind_by_height_1, ind_by_height_0)
    chromosome <- list(BPS = ind_by_height, CLS = order(1:n_containers))
    chromosomes <- c(chromosomes, list(chromosome))
    
    # sort descending according to width
    ind_by_width_1 <- order(boxes_width_1, decreasing = TRUE)
    ind_by_width_1 <- ind_by_width_1[1:m_1]
    ind_by_width_0 <- order(boxes_width_0, decreasing = TRUE)
    ind_by_width_0 <- ind_by_width_0[1:m_0]
    ind_by_width <- c(ind_by_width_1, ind_by_width_0)
    chromosome <- list(BPS = ind_by_width, CLS = order(1:n_containers))
    chromosomes <- c(chromosomes, list(chromosome))
    
    return(chromosomes)
  }
  
  
  #' Initialize a Population
  #'
  #' @param population_size - An integer
  #' @param n_containers    - An integer
  #' @param boxes           - A list of objects of class Box
  #' @return A list of chromosomes 
  InitializePopulation <- function (population_size, 
                                    n_containers, 
                                    boxes) {
    # get number of boxes
    n_boxes <- length(boxes)
    
    # create 4 custom chromosomes
    population <- 
      CustomChromosomeInitialization(boxes = boxes, 
                                     n_containers = n_containers
      )
    
    if (population_size <= 4) {
      population <- population[1:population_size]
    } else {
      n_chromosomes_to_create <- population_size - 4
      for (i in 1:n_chromosomes_to_create) {
        chromosome <- CreateChromosome(n_boxes = n_boxes, n_containers = n_containers)
        population <- c(population, list(chromosome))
      }
    }
    
    return(population)
  }
  
  ########################################## PackBoxes  ##########################################
  
  # TODO: write a function for choosing best box rotation
  
  
  #' Pack the Boxes into the Containers
  #'
  #' @param boxes                      - A list of objects of class Box 
  #' @param containers                 - A list of objects of class Container
  #' @param box_packing_sequence       - A vector
  #' @param container_loading_sequence - A vector
  #' @return A list of Containers and corresponding Boxes placed into 
  #'         this Containers 
  PackBoxes <- function (boxes, 
                         containers, 
                         box_packing_sequence, 
                         container_loading_sequence) {
    
    n <- length(containers)  # number of containers
    m <- length(boxes)  # number of boxes
    
    if (n == 0) {
      stop('Specify containers')
    }
    if (m == 0) {
      stop('Specify boxes')
    }
    
    #' Create an empty Packing Solution
    #'
    #' @param containers - A list of containers
    #' @return A list
    CreatePackingSolution <- function (containers) {
      packing_solution <- list()
      for (i in 1:length(containers)) {
        packing_solution <- c(packing_solution, list(containers[i]))
      }
      
      return(packing_solution)
    }
    
    # create empty packing solution
    packing_solution <- CreatePackingSolution(containers)
    
    # initialize box placement to FALSE
    placed_boxes <- rep(FALSE, m)
    
    for (con_i in 1:n) {  # for each container
      container_ind <- container_loading_sequence[con_i]  # get index of container from chromosome
      
      for (box_i in 1:m) {  # for each box
        box_ind <- box_packing_sequence[box_i]  # get index of box from chromosome
        
        if (placed_boxes[box_ind]) {
          # the box is already placed, move to next one
          next
        } else {
          # the box isn't placed yet
          
          # get box
          box <- boxes[[box_ind]]
          
          # get containers EMS
          con_EMS <- packing_solution[[container_ind]][[1]]@ems  # a list
          
          if (is.null(containers)) {  # fixing 1x1x1 on 1x1x1
            break;
          }
          
          # prioritize container's EMS
          con_EMS <- PrioritizeEMS(con_EMS)
          
          for (ems in con_EMS) {  # for each EMS in the container
            if (CheckIfBoxFitsIntoEMS(box, ems)) {
              
              # 1. choose best box placement
              box <- PerformPlacementSelection(box, ems)
              
              # 2. place box in ems: set box origin to EMS origin 
              box@origin <- ems@origin 
              
              # 3. write placement in packing solution 
              packing_solution[[container_ind]] <- 
                c(packing_solution[[container_ind]], box)
              
              # 4. update EMS for the container
              packing_solution[[container_ind]][[1]]@ems <- 
                UpdateEMS(packing_solution[[container_ind]][[1]]@ems, box)
              
              # 5. mark the box as placed:
              placed_boxes[box_ind] <- TRUE
              
              break
            }                           
          }
        }
      }
    }
    
    return(packing_solution)
  }
  
  
  #' Calcuate a Fitness of Packing Solution (Percent of Wasted Space)
  #'
  #' @param packing_solution - A list
  #' @return A number between 0 and 1
  CalculateFitness <- function (packing_solution) {
    
    #' Calculate volume of an object
    #'
    #' @param object - An object of class Box or Container
    #' @return A numeric
    CalculateVolume <- function (object) {
      volume <- object@length * object@height * object@width
      return(volume)
    }
    
    container_volume <- 0
    boxes_volume <- 0
    
    for (i in 1:length(packing_solution)) {
      if (length(packing_solution[[i]]) == 1) {
        # the Container is empty
        next
      } else {
        container_volume <- container_volume + CalculateVolume(packing_solution[[i]][[1]])
        
        for (j in 2:length(packing_solution[[i]])) {
          boxes_volume <- boxes_volume + CalculateVolume(packing_solution[[i]][[j]])
        }
      }
    }
    
    # calculate percent of wasted space
    fitness <- 1 - (boxes_volume / container_volume)
    return(fitness)
  }
  
  ########################################## plotcube ##########################################
  
  #' A custom function to initialize RGL device
  #'
  #' @param new.device - A logical value. If TRUE, creates a new device
  #' @param width the width of the device
  #' @import rgl
  RGLInit <- function(new.device = FALSE, width = 500) {
    if (new.device | rgl.cur() == 0) {  # rgl.cur(): returns active device ID
      rgl.open()
      par3d(windowRect = 50 + c(0, 0, width, width))
      rgl.bg(color = "white")
    }
    rgl.viewpoint(theta = 40, phi = 20)
  }
  
  #' Plot 3D cube
  #'
  #' @param object - An object of class 'Container', 'Box' or 'EMS'
  #' @param plot_origin  - logical, whether to plot point at the origin
  #' @examples
  #'
  #' RGLInit(new.device = T)  # create new device with specific settings
  #' # plot a container
  #' container <- Container(width = 2, length = 4, height = 2)
  #' PlotCube(container)
  #'
  #' # plot a box
  #' box <- Box(width = 1, length = 1, height = 1, origin = c(0, 0,0))
  #' PlotCube(box)
  PlotCube <- function (object,
                        plot_origin = TRUE, ...) {
    
    # stop if object origin is not specified
    if (length(object@origin) == 0) {
      stop('Specify origin for the object')
    }
    
    origin <- object@origin
    length <- object@length
    height <- object@height
    width <- object@width
    
    vertex1 <- origin
    vertex2 <- origin + c(0, height, width)
    vertex3 <- origin + c(length, height, 0)
    vertex4 <- origin + c(length, 0, width)
    
    # create data frame with coordinates of lines
    # to be joined to form a cube
    lines <- data.frame(vertex1, origin + c(0, height, 0))
    lines <- cbind(lines, data.frame(vertex1, origin + c(length, 0, 0)))
    lines <- cbind(lines, data.frame(vertex1, origin + c(0, 0, width)))
    
    lines <- cbind(lines, data.frame(vertex2, origin + c(0, 0, width)))
    lines <- cbind(lines, data.frame(vertex2, origin + c(0, height, 0)))
    lines <- cbind(lines, data.frame(vertex2, origin + c(length, height, width)))
    
    lines <- cbind(lines, data.frame(vertex3, origin + c(0, height, 0)))
    lines <- cbind(lines, data.frame(vertex3, origin + c(length, 0, 0)))
    lines <- cbind(lines, data.frame(vertex3, origin + c(length, height, width)))
    
    lines <- cbind(lines, data.frame(vertex4, origin + c(0, 0, width)))
    lines <- cbind(lines, data.frame(vertex4, origin + c(length, 0, 0)))
    lines <- cbind(lines, data.frame(vertex4, origin + c(length, height, width)))
    
    lines <- t(lines)
    
    if (class(object) == 'Container') {
      cube_color <- 'black'
    } else {
      # randomly select color for cube
      colors <- c('blue', 'red', 'green', 'orange')
      cube_color <- sample(colors, 1)
    }
    
    # plot cube
    segments3d(lines, line_antialias = TRUE, color = cube_color, ...)
    if (plot_origin) {
      points3d(x = 0, y = 0, z = 0, color = 'red', size = 7)
    }
  }
  
  
  #' Plot a Packing Solution
  #'
  #' @param packing_solution - A list
  #' @return Returns as many plots of Containers with placed Boxes
  #'         as many nonempty Containers are in the packing solution
  PlotPackingSolution <- function (packing_solution) {
    
    for (i in 1:length(packing_solution)) {
      if (length(packing_solution[[i]]) == 1) {
        # the Container is empty
        next
      } else {
        # initialize device
        RGLInit(new.device = T)
        
        # plot a container
        PlotCube(packing_solution[[i]][[1]])
        
        # plot boxes
        for (j in 2:length(packing_solution[[i]])) {
          PlotCube(packing_solution[[i]][[j]])
        }
      }
    }
    
  }
  
  ########################################## PerformBoxPacking ##########################################
  
  #' Perform Box Packing
  #'
  #' @param containers      A list of objects of class Container
  #' @param boxes           A list of objects of class Box
  #' @param n_iter          An integer; Number of iterations
  #' @param population_size An integer; Number of Chromosomes in each generation
  #' @param elitism_size    An integer; Number of the best chromosomes to be
  #'                          choosen to next generaion
  #' @param crossover_prob  A numeric in [0; 1]; A probability for chromosome crossover
  #' @param mutation_prob   A numeric in [0; 1]; A probability for chromosome mutation
  #' @param verbose         Logical; Whether to print info during program execution
  #' @param plotSolution    Logical; Whether to plot a Packing Solution
  #'
  #' @return A Packing Solution list
  #' @examples
  #'
  #' # create containers
  #' c1 <- Container(length = 2, height = 2, width = 2)
  #' c2 <- Container(length = 2, height = 2, width = 2)
  #'
  #' # create boxes
  #' b1 <- Box(length = 0.5, height = 0.5, width = 0.5)
  #' b2 <- Box(length = 1, height = 0.5, width = 0.5)
  #' b3 <- Box(length = 0.5, height = 0.5, width = 0.5)
  #' b4 <- Box(length = 0.5, height = 0.5, width = 0.5)
  #' b5 <- Box(length = 0.5, height = 0.5, width = 0.5)
  #' b6 <- Box(length = 2, height = 0.5, width = 0.5)
  #' b7 <- Box(length = 1, height = 0.5, width = 0.5)
  #' b8 <- Box(length = 1, height = 0.5, width = 0.5)
  #' b9 <- Box(length = 0.5, height = 0.5, width = 0.5)
  #' b10 <- Box(length = 0.5, height = 0.5, width = 0.5)
  #' b11 <- Box(length = 1.5, height = 1.5, width = 1.5)
  #' b12 <- Box(length = 1.5, height = 0.5, width = 0.5)
  #' b13 <- Box(length = 1, height = 1, width = 1)
  #' b14 <- Box(length = 1, height = 1, width = 1)
  #'
  #' boxes <- list(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14)
  #' containers <- list(c1, c2)
  #'
  #' # Box Packing
  #' solution <-
  #'      PerformBoxPacking(containers = containers,
  #'                        boxes = boxes,
  #'                        n_iter = 4,
  #'                        population_size = 30,
  #'                        elitism_size = 5,
  #'                        crossover_prob = 0.5,
  #'                        mutation_prob = 0.5,
  #'                        verbose = TRUE,
  #'                        plotSolution = TRUE
  #'                        )
  #'
  #' @export
  PerformBoxPacking <- function (containers,  # menghilangkan elitism, selection, mutation, crossover
                                 boxes,
                                 population_size,
                                 verbose = FALSE,
                                 plotSolution = FALSE) {
    
    # TODO: think about case with 1-2 boxes, 1-2 containers
    
    # TODO: write verifications of arguments
    if (length(containers) == 0) {
      stop('Specify containers')
    }
    if (length(boxes) == 0) {
      stop('Specify boxes')
    }
    if (population_size <= 0) {
      stop('Population size cant be <= 0')
    } #else if (population_size > 0 & population_size < .) {
    #      print('Bad choice for Population Size')
    # }
    
    
    n <- length(containers)  # number of containers
    m <- length(boxes)  # number of boxes
    
    # Initialization
    population <- InitializePopulation(population_size = population_size,
                                       n_containers = n,
                                       boxes = boxes
    )
    chromosome_fitness <- rep(0, population_size)
    
    population_size <- length(population)
    for (chromosome_i in 1:population_size) {
      if (verbose) {
        cat('  Chromosome:', chromosome_i, 'out of ', population_size, '\n')
      }
      
      chromosome <- population[[chromosome_i]]
      
      # perform packing
      packing_solution <-
        PackBoxes(boxes = boxes,
                  containers = containers,
                  box_packing_sequence = chromosome$BPS,
                  container_loading_sequence = chromosome$CLS
        )
      
      # calculate fitness of current chromosome
      chromosome_fitness[chromosome_i] <- CalculateFitness(packing_solution)
    }

    
    # choose solution of packing after all iterations
    best_chromosome <- population[[which.min(chromosome_fitness)]]
    best_chromosome_packing_solution <-
      PackBoxes(boxes = boxes,
                containers = containers,
                box_packing_sequence = best_chromosome$BPS,
                container_loading_sequence = best_chromosome$CLS
      )
    
    if (plotSolution) {
      PlotPackingSolution(best_chromosome_packing_solution)
    }
    
    return(best_chromosome_packing_solution)
  }
  
  ########################################## DEF BOX & CONTAINER ##########################################
  
  boxess <- list()
  for (i in 1:nrow(boxes)) {
    box_temp <- Box(length = boxes[i,1], height = boxes[i,2], width = boxes[i,3], rotate = boxes[i,4], stack = boxes[i,5])
    if (box_temp@rotate != 0 | box_temp@rotate != 1) {
      box_temp@rotate == 1
    }
    if (box_temp@stack != 0 | box_temp@stack != 1) {
      box_temp@stack == 1
    }
    boxess <- c(boxess, box_temp)
  }
  
  containerss <- list()
  for (i in 1:nrow(containers)) {
    container_temp <- Container(length = containers[i,1], height = containers[i,2], width = containers[i,3])
    containerss <- c(containerss, container_temp)
  }
  
  ########################################## RUN PROGRAM ##########################################
  
  # Box Packing
  solution <-
    PerformBoxPacking(containers = containerss,
                      boxes = boxess,
                      population_size = 4,
                      verbose = FALSE,
                      plotSolution = TRUE
    )
  
  ########################################## CREATE DATA SOLUTION ##########################################
  
  datasolution <- list()
  for (j in 1:length(solution)) {
    if (length(solution[[j]]) != 1) {
      databox <- NULL
      for (i in 2:length(solution[[j]])) {
        tempbox <- data.frame("length" = solution[[j]][[i]]@length,
                              "height" = solution[[j]][[i]]@height,
                              "width"  = solution[[j]][[i]]@width,
                              "x" = solution[[j]][[i]]@origin[[1]],
                              "y" = solution[[j]][[i]]@origin[[2]],
                              "z" = solution[[j]][[i]]@origin[[3]])
        databox <- rbind(databox, tempbox)
      }
      tempcontainer <- list(length = solution[[j]][[1]]@length,
                            height = solution[[j]][[1]]@height,
                            width = solution[[j]][[1]]@width,
                            box = databox)
      datacontainer <- list(container = tempcontainer)
      datasolution <- c(datasolution, datacontainer)
    }
  }

  return(datasolution)
}

#r <- plumb("~/load-optimization-local/loadopt.R")
#r$run(port=5003) 