bool_str_title_cs <- stringr::str_to_title(c("YES", "NO", "1", "0", "Y", "N", "T", "F", "TRUE", "FALSE"))

parse_bool <- function(x) {
  x <- stringr::str_to_upper(as.character(x))
  x <- car::recode(x, "c('0','FALSE','F','NO','N')='FALSE';c('1','TRUE','T','Y','YES')='TRUE';else=NA")

  if (is.na(x)) stop("x must be boolean coded: i.e. Yes, No, 1, 0, TRUE, FALSE, Y, N")

  readr::parse_logical(x)
}

parse_bool_and_add <- function(x, additionals = NULL, return_val_true = "Yes", return_val_false = "No") {
  if (!is.null(additionals) && (x %in% additionals)) {
    x
  } else if (is.logical( (bool_res <- parse_bool(x)) )) {
    ifelse(bool_res, return_val_true, return_val_false)
  } else {
    stop(paste0("Expected a boolean string + the following additionals:", paste0(additionals, collapse = ", ")))
  }
}

# parses a string into two defined sex values
# returns either "Female" or "Male
parse_sex <- function(x,
                      male_numeric_code = 1,
                      female_numeric_code = 0,
                      male_bool_code = TRUE
                      ) {
  if (male_numeric_code == female_numeric_code)
    stop("male_numeric_code must not be the same as female_numeric_code")

  if (is.null(x)) return(NULL)
  if (is.na(x)) return(NULL)

  x <- stringr::str_to_upper(as.character(x))

  male_bool_str <- ifelse(male_bool_code, ",'TRUE','Y','YES'", ",'FALSE','N','NO'")
  female_bool_str <- ifelse(!male_bool_code, ",'TRUE','Y','YES'", ",'FALSE','N','NO'")

  recode_str <- stringr::str_interp("c('M','MALE','${male_numeric_code}'${male_bool_str})='Male';c('F','FEMALE','${female_numeric_code}'${female_bool_str})='Female';else=NA")

  x <- car::recode(x, recode_str)

  if (is.na(x)) stop("x must be a valid gender code.")

  x
}

#' Parses a CHD string containing common names of CHD types and translates them
#' to the STS coding => {"None", "One", "Two", "Three"}
#'
#' Returns NULL if string cannot be parsed into a CHD value
#'
#' @param chd the chd string, will be automatically be lowercase transformed,
#'            examples: "KHK1", "KHK-1",
#'            "CHD-1", "CHD1", "1-KHK", "1-vessel", "One vessels", "Eins", "One", "KHK-I",
#'            "II-khk", "II-CHD, "I vessel" or
#'            "None" strings: ("none", "no", "n", "nein", "0", "f", "false")
#'
#'
parseVesselsDisease <- function(chd) {
  if (is.null(chd)) return(NULL)

  chd <- stringr::str_to_lower(as.character(chd))

  # check None values:
  if (chd %in% c("none", "no", "n", "nein", "0", "f", "false")) {
    return("None")
  }

  # Numeric Vessel disease number
  transtab <- c("1" = "One", "2" = "Two", "3" = "Three")
  res <- stringr::str_match(chd, "(khk|chd|vessels?)-? ?([123])")
  if (!is.null(res[1,1]) & !is.na(res[1,1])) {
    return(unname(transtab[res[1,3]]))
  }

  res <- stringr::str_match(chd, "([123])-? ?(khk|chd|vessels?)")
  if (!is.null(res[1,1]) & !is.na(res[1,1])) {
    return(unname(transtab[res[1,2]]))
  }

  # Roman Numeric Vessel disease number
  transtab <- c("i" = "One", "ii" = "Two", "iii" = "Three")

  res <- stringr::str_match(chd, "(khk|chd|vessels?)-? ?(i{1,3})")
  if (!is.null(res[1,1]) & !is.na(res[1,1])) {
    return(unname(transtab[res[1,3]]))
  }

  res <- stringr::str_match(chd, "(i{1,3})-? ?(khk|chd|vessels?)")
  if (!is.null(res[1,1]) & !is.na(res[1,1])) {
    return(unname(transtab[res[1,2]]))
  }

  return(NULL)
}

#' Parses regurgitation state to the STS nomenclature
#'
#' Function takes a value, converts it to a string and tries to to sensefully
#' convert it to a STS nomenclature.
#'
#' @param regurg_str The following values will be detected:
#'                     - "None" or Boolean NO strings
#'                     - "undocumented" | "not documented" | "nicht dokumentiert" | "undokumentiert"
#'                        | "unbekannt" | "nicht bekannt" | "unknown" | "not known"
#'                     - "trivial/trace"
#'                     - "mild" | "1+" | "1"
#'                     - "moderate" | "2+" | "2" | "mittel" | "mittelgradig" | "mittelschwer"
#'                     - "severe" | "3+" | "3" | "schwer" | "schwergradig"
parse_vd_regurg <- function(regurg_str) {
  regurg_str <- stringr::str_to_lower(stringr::str_trim(regurg_str))

  # dectect no values:
  if (regurg_str %in% c('0','false','f','no','n', 'none', 'keine', 'nein')) {
    return("None")
  }

  # dectect "undocumented values"
  if (regurg_str %in% c("undocumented", "not documented", "nicht dokumentiert",
                        "undokumentiert", "unbekannt", "unknown", "not known",
                        "nicht bekannt")) {
    return("Not documented")
  }

  # detect trivial / trace
  if (regurg_str %in% c("trivial", "trace", "spur")) {
    return("Trivial/Trace")
  }

  # detect mild
  res <- stringr::str_match(regurg_str, "mild|1\\+?")
  if (!is.na(res[1,1])) {
    return("Mild")
  }

  # detect mild
  res <- stringr::str_match(regurg_str, "mild|1\\+?")
  if (!is.na(res[1,1])) {
    return("Moderate")
  }

  # detect moderate
  res <- stringr::str_match(regurg_str, "moderate|medium|2\\+?|mittel.*")
  if (!is.na(res[1,1])) {
    return("Moderate")
  }

  # detect severe
  res <- stringr::str_match(regurg_str, "severe|3\\+?|schwer.*")
  if (!is.na(res[1,1])) {
    return("Severe")
  }

  return(NULL)
}


#' Calculates the eGFR
#'
#' Uses Cockroft-Gault formula
#'
#' @param crea Plasma Creatinin in mg/dL (1 mg/dL = 88.4 umol/L)
#' @param weight weight in kg
#' @param age age in years
#' @param sex Gender of the patient, should be "Male" or "Female", 'F' or 'M',
#'            plus in addition
#'             all boolean FALSE strings --> Male
#'             all boolean TRUE strings --> Female
#'
#' @return the calculated eGFR
#' @export
cc_eGFR = function(crea,
                   weight,
                   age,
                   sex,
                   digits_round = 0) {

  # ensure types
  weight <- ensurer::ensure(weight, is.numeric(.), . >= 10 , . <= 250)
  age <- ensurer::ensure(age, is.numeric(.), . >= 18 , . <= 95)
  crea <- ensurer::ensure(crea, is.numeric(.), . >= 0.1 , . <= 20)
  sex <- parse_sex(sex)
  digits_round <- ensurer::ensure(digits_round, is.numeric(.), . %in% 0:100)

  # convert crea to umol/L
  crea <- crea * 88.4

  sex_coef<- 1
  if (sex == "Female")
    sex_coef<- 0.85

  egfr <- (140 - age) * weight * sex_coef / (crea * 0.8136)

  round(egfr, digits = digits_round)
}

# Input data
# plot.data - dataframe comprising one row per group (cluster); col1 = group name; cols 2-n = variable values
# axis.labels - names of axis labels if other than column names supplied via plot.data [Default = colnames(plot.data)[-1]
#
# Grid lines
# grid.min - value at which mininum grid line is plotted [Default = -0.5]
# grid.mid - value at which 'average' grid line is plotted [Default = 0]
# grid.max - value at which 'average' grid line is plotted [Default = 0.5]
#
# Plot centre
# centre.y - value of y at centre of plot [default < grid.min]
# label.centre.y - whether value of y at centre of plot should be labelled [Default=FALSE]
#
# Plot extent
# #Parameters to rescale the extent of the plot vertically and horizontally, in order to
# #allow for ggplot default settings placing parts of axis text labels outside of plot area.
# #Scaling factor is defined relative to the circle diameter (grid.max-centre.y).
#
# plot.extent.x.sf - controls relative size of plot horizontally [Default 1.2]
# plot.extent.y.sf - controls relative size of plot vertically [Default 1.2]
#
# Grid lines
# #includes separate controls for the appearance of some aspects the 'minimum', 'average' and 'maximum' grid lines.
#
# grid.line.width [Default=0.5]
# gridline.min.linetype [Default=“longdash”]
# gridline.mid.linetype [Default=“longdash”]
# gridline.max.linetype [Default=“longdash”]
# gridline.min.colour [Default=“grey”]
# gridline.mid.colour [Default=“blue”]
# gridline.max.colour [Default=“grey”]
#
# Grid labels
# grid.label.size - text size [Default=4]
# gridline.label.offset - displacement to left/right of central vertical axis [Default=-0.02(grid.max-centre.y)]
# *label.gridline.min - whether or not to label the mininum gridline [Default=TRUE]
#
# Axis and Axis label
# axis.line.colour - line colour [Default=“grey”]
# axis.label.size - text size [Default=3]
# axis.label.offset - vertical displacement of axis labels from maximum grid line, measured relative to circle diameter [Default=1.15]
#
# x.centre.range - controls axis label alignment. Default behaviour is to left-align axis labels on left hand side of
# plot (x < -x.centre.range); right-align labels on right hand side of plot (x > +x.centre.range); and centre align
# those labels for which -x.centre.range < x < +x.centre.range
#
# Cluster plot lines
# group.line.width [Default=1]
# group.point.size [Default=4]
#
# Background circle
# background.circle.colour [Default=“yellow”]
# background.circle.transparency [Default=0.2]
#
# Plot legend
# plot.legend - whether to include a plot legend [Default = FALSE for one cluster; TRUE for 2+ clusters]
# legend.title [Default=“Cluster”]
# legend.text.size [Default=grid.label.size=4]
radial_plot <- function(plot.data,
                             axis.labels=colnames(plot.data)[-1],
                             grid.min=-0.5,  #10,
                             grid.mid=0,  #50,
                             grid.max=0.5,  #100,
                             centre.y=grid.min - ((1/9)*(grid.max-grid.min)),
                             plot.extent.x.sf=1.2,
                             plot.extent.y.sf=1.2,
                             x.centre.range=0.02*(grid.max-centre.y),
                             label.centre.y=FALSE,
                             grid.line.width=0.5,
                             gridline.min.linetype="longdash",
                             gridline.mid.linetype="longdash",
                             gridline.max.linetype="longdash",
                             gridline.min.colour="grey",
                             gridline.mid.colour="blue",
                             gridline.max.colour="grey",
                             grid.label.size=4,
                             gridline.label.offset=-0.02*(grid.max-centre.y),
                             label.gridline.min=TRUE,
                             axis.label.offset=1.15,
                             axis.label.size=3,
                             axis.line.colour="grey",
                             group.line.width=1,
                             group.point.size=4,
                             background.circle.colour="yellow",
                             background.circle.transparency=0.2,
                             plot.legend=if (nrow(plot.data)>1) TRUE else FALSE,
                             legend.title="Cluster",
                             legend.text.size=grid.label.size ) {

  var.names <- colnames(plot.data)[-1]  #'Short version of variable names
  #axis.labels [if supplied] is designed to hold 'long version' of variable names
  #with line-breaks indicated using \n

  #caclulate total plot extent as radius of outer circle x a user-specifiable scaling factor
  plot.extent.x=(grid.max+abs(centre.y))*plot.extent.x.sf
  plot.extent.y=(grid.max+abs(centre.y))*plot.extent.y.sf

  #Check supplied data makes sense
  if (length(axis.labels) != ncol(plot.data)-1)
    return("Error: 'axis.labels' contains the wrong number of axis labels")
  if(min(plot.data[,-1])<centre.y)
    return("Error: plot.data' contains value(s) < centre.y")
  if(max(plot.data[,-1])>grid.max)
    return("Error: 'plot.data' contains value(s) > grid.max")

  #Declare required internal functions

  CalculateGroupPath <- function(df) {
    #Converts variable values into a set of radial x-y coordinates
    #Code adapted from a solution posted by Tony M to
    #http://stackoverflow.com/questions/9614433/creating-radar-chart-a-k-a-star-plot-spider-plot-using-ggplot2-in-r

    #Args:
    #  df: Col 1 -  group ('unique' cluster / group ID of entity)
    #      Col 2-n:  v1.value to vn.value - values (e.g. group/cluser mean or median) of variables v1 to v.n

    path <- as.factor(as.character(df[,1]))

    ##find increment
    angles = seq(from=0, to=2*pi, by=(2*pi)/(ncol(df)-1))

    ##create graph data frame
    graphData= data.frame(seg="", x=0,y=0)
    graphData=graphData[-1,]

    for(i in levels(path)){

      pathData = subset(df, df[,1]==i)

      for(j in c(2:ncol(df))){

        #pathData[,j]= pathData[,j]

        graphData=rbind(graphData, data.frame(group=i,
                                              x=pathData[,j]*sin(angles[j-1]),
                                              y=pathData[,j]*cos(angles[j-1])))
      }
      ##complete the path by repeating first pair of coords in the path
      graphData=rbind(graphData, data.frame(group=i,
                                            x=pathData[,2]*sin(angles[1]),
                                            y=pathData[,2]*cos(angles[1])))

    }

    #Make sure that name of first column matches that of input data (in case !="group")
    colnames(graphData)[1] <- colnames(df)[1]

    graphData #data frame returned by function

  }

  CaclulateAxisPath = function(var.names,min,max) {
    #Caculates x-y coordinates for a set of radial axes (one per variable being plotted in radar plot)

    #Args:
    #var.names - list of variables to be plotted on radar plot
    #min - MININUM value required for the plotted axes (same value will be applied to all axes)
    #max - MAXIMUM value required for the plotted axes (same value will be applied to all axes)

    #var.names <- c("v1","v2","v3","v4","v5")
    n.vars <- length(var.names) # number of vars (axes) required

    #Cacluate required number of angles (in radians)
    angles <- seq(from=0, to=2*pi, by=(2*pi)/n.vars)

    #calculate vectors of min and max x+y coords
    min.x <- min*sin(angles)
    min.y <- min*cos(angles)
    max.x <- max*sin(angles)
    max.y <- max*cos(angles)

    #Combine into a set of uniquely numbered paths (one per variable)
    axisData <- NULL
    for (i in 1:n.vars) {
      a <- c(i,min.x[i],min.y[i])
      b <- c(i,max.x[i],max.y[i])
      axisData <- rbind(axisData,a,b)
    }

    #Add column names + set row names = row no. to allow conversion into a data frame
    colnames(axisData) <- c("axis.no","x","y")
    rownames(axisData) <- seq(1:nrow(axisData))

    #Return calculated axis paths
    as.data.frame(axisData)
  }


  funcCircleCoords <- function(center = c(0,0), r = 1, npoints = 100){
    #Adapted from Joran's response to http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }

  ### Convert supplied data into plottable format

  # (a) add abs(centre.y) to supplied plot data
  #[creates plot centroid of 0,0 for internal use, regardless of min. value of y
  # in user-supplied data]
  plot.data.offset <- plot.data
  plot.data.offset[,2:ncol(plot.data)]<- plot.data[,2:ncol(plot.data)]+abs(centre.y)
  #print(plot.data.offset)

  # (b) convert into radial coords
  group <-NULL
  group$path <- CalculateGroupPath(plot.data.offset)
  #print(group$path)

  # (c) Calculate coordinates required to plot radial variable axes
  axis <- NULL
  axis$path <- CaclulateAxisPath(var.names,grid.min+abs(centre.y),grid.max+abs(centre.y))
  #print(axis$path)

  # (d) Create file containing axis labels + associated plotting coordinates

  #Labels
  axis$label <- data.frame(
    text=axis.labels,
    x=NA,
    y=NA )
  #print(axis$label)

  #axis label coordinates
  n.vars <- length(var.names)
  angles = seq(from=0, to=2*pi, by=(2*pi)/n.vars)
  axis$label$x <- sapply(1:n.vars, function(i, x) {((grid.max+abs(centre.y))*axis.label.offset)*sin(angles[i])})
  axis$label$y <- sapply(1:n.vars, function(i, x) {((grid.max+abs(centre.y))*axis.label.offset)*cos(angles[i])})
  #print(axis$label)

  # (e) Create Circular grid-lines + labels

  #caclulate the cooridinates required to plot circular grid-lines for three user-specified
  #y-axis values: min, mid and max [grid.min; grid.mid; grid.max]
  gridline <- NULL
  gridline$min$path <- funcCircleCoords(c(0,0),grid.min+abs(centre.y),npoints = 360)
  gridline$mid$path <- funcCircleCoords(c(0,0),grid.mid+abs(centre.y),npoints = 360)
  gridline$max$path <- funcCircleCoords(c(0,0),grid.max+abs(centre.y),npoints = 360)
  #print(head(gridline$max$path))

  #gridline labels
  gridline$min$label <- data.frame(x=gridline.label.offset,y=grid.min+abs(centre.y),
                                   text=as.character(grid.min))
  gridline$max$label <- data.frame(x=gridline.label.offset,y=grid.max+abs(centre.y),
                                   text=as.character(grid.max))
  gridline$mid$label <- data.frame(x=gridline.label.offset,y=grid.mid+abs(centre.y),
                                   text=as.character(grid.mid))
  #print(gridline$min$label)
  #print(gridline$max$label)
  #print(gridline$mid$label)


  ### Start building up the radar plot

  # Delcare 'theme_clear', with or without a plot legend as required by user
  #[default = no legend if only 1 group [path] being plotted]
  theme_clear <- theme_bw() +
    theme(axis.text.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_blank(),
          legend.key=element_rect(linetype="blank"))

  if (plot.legend==FALSE) theme_clear <- theme_clear + theme(legend.position="none")

  #Base-layer = axis labels + plot extent
  # [need to declare plot extent as well, since the axis labels don't always
  # fit within the plot area automatically calculated by ggplot, even if all
  # included in first plot; and in any case the strategy followed here is to first
  # plot right-justified labels for axis labels to left of Y axis for x< (-x.centre.range)],
  # then centred labels for axis labels almost immediately above/below x= 0
  # [abs(x) < x.centre.range]; then left-justified axis labels to right of Y axis [x>0].
  # This building up the plot in layers doesn't allow ggplot to correctly
  # identify plot extent when plotting first (base) layer]

  #base layer = axis labels for axes to left of central y-axis [x< -(x.centre.range)]
  base <- ggplot(axis$label) + xlab(NULL) + ylab(NULL) + coord_equal() +
    geom_text(data=subset(axis$label,axis$label$x < (-x.centre.range)),
              aes(x=x,y=y,label=text),size=axis.label.size,hjust=1) +
    scale_x_continuous(limits=c(-plot.extent.x,plot.extent.x)) +
    scale_y_continuous(limits=c(-plot.extent.y,plot.extent.y))

  # + axis labels for any vertical axes [abs(x)<=x.centre.range]
  base <- base + geom_text(data=subset(axis$label,abs(axis$label$x)<=x.centre.range),
                           aes(x=x,y=y,label=text),size=axis.label.size,hjust=0.5)

  # + axis labels for any vertical axes [x>x.centre.range]
  base <- base + geom_text(data=subset(axis$label,axis$label$x>x.centre.range),
                           aes(x=x,y=y,label=text),size=axis.label.size,hjust=0)

  # + theme_clear [to remove grey plot background, grid lines, axis tick marks and axis text]
  base <- base + theme_clear

  #  + background circle against which to plot radar data
  base <- base + geom_polygon(data=gridline$max$path,aes(x,y),
                              fill=background.circle.colour,
                              alpha=background.circle.transparency)

  # + radial axes
  base <- base + geom_path(data=axis$path,aes(x=x,y=y,group=axis.no),
                           colour=axis.line.colour)

  # ... + group (cluster) 'paths'
  base <- base + geom_path(data=group$path,aes(x=x,y=y,group=group,colour=group),
                           size=group.line.width)

  # ... + group points (cluster data)
  base <- base + geom_point(data=group$path,aes(x=x,y=y,group=group,colour=group),size=group.point.size)

  #... + amend Legend title
  if (plot.legend==TRUE) base  <- base + labs(colour=legend.title,size=legend.text.size)

  # ... + circular grid-lines at 'min', 'mid' and 'max' y-axis values
  base <- base +  geom_path(data=gridline$min$path,aes(x=x,y=y),
                            lty=gridline.min.linetype,colour=gridline.min.colour,size=grid.line.width)
  base <- base +  geom_path(data=gridline$mid$path,aes(x=x,y=y),
                            lty=gridline.mid.linetype,colour=gridline.mid.colour,size=grid.line.width)
  base <- base +  geom_path(data=gridline$max$path,aes(x=x,y=y),
                            lty=gridline.max.linetype,colour=gridline.max.colour,size=grid.line.width)

  # ... + grid-line labels (max; ave; min) [only add min. gridline label if required]
  if (label.gridline.min==TRUE) {
    base <- base + geom_text(aes(x=x,y=y,label=text),data=gridline$min$label,face="bold",size=grid.label.size, hjust=1) }
  base <- base + geom_text(aes(x=x,y=y,label=text),data=gridline$mid$label,face="bold",size=grid.label.size, hjust=1)
  base <- base + geom_text(aes(x=x,y=y,label=text),data=gridline$max$label,face="bold",size=grid.label.size, hjust=1)

  # ... + centre.y label if required [i.e. value of y at centre of plot circle]
  if (label.centre.y==TRUE) {
    centre.y.label <- data.frame(x=0, y=0, text=as.character(centre.y))
    base <- base + geom_text(aes(x=x,y=y,label=text),data=centre.y.label,face="bold",size=grid.label.size, hjust=0.5) }

  return(base)

}
