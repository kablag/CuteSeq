library(RColorBrewer)

addFeatureToLayers <- function(layers, featureId, from, to, compact = TRUE) {
  to.layer <- NA
  if (from < first(layers$seqI))
    from <- first(layers$seqI)
  if (to > last(layers$seqI))
    to <- last(layers$seqI)
  if (compact) {
    for (layer.i in 3:ncol(layers)) {
      if (all(is.na(layers[seqI >= from & seqI <= to][[layer.i]]))) {
        to.layer <- layer.i - 2
        break()
      }
    }
  } else {
    if (all(is.na(layers[seqI >= from & seqI <= to][[ncol(layers)]]))) {
      to.layer <- ncol(layers) - 2
    }
  }
  if (is.na(to.layer))
    to.layer <- ncol(layers) - 1
  layers[seqI >= from & seqI <= to,
         c(sprintf("layer_%s", to.layer)) := featureId]
  NULL
}

addFlat <- function(flatMap, ID, start, end) {
  if ((start < first(flatMap$seqI) &&
      end < first(flatMap$seqI)) ||
      (start < first(flatMap$seqI) &&
       end < first(flatMap$seqI)))
    return(NULL)
  if (start < first(flatMap$seqI))
    start <- first(flatMap$seqI)
  if (end > last(flatMap$seqI))
    end <- last(flatMap$seqI)
  flatMap[J(start:end), map := ID]
  NULL
}



cuteSeq <- function(gbSequence,
                    gbFeatures,
                    gbSequenceStart = 1,
                    colorBy = "type",
                    labelBy = "label",
                    considerStrand = TRUE,
                    linesWidth = 60,
                    geneious = TRUE) {
  features <- as.data.table(
    gbFeatures)[, ID := .I]
  if (geneious && !is.null(features[["note"]])) {
    features[!is.na(note), type := sub("Geneious type: (.*)", "\\1", note)]
  }
  if (considerStrand) {
    features[[colorBy]] <- paste(features[[colorBy]], features$strand)
  }
  uniqueColorByParams <- unique(features[[colorBy]])
  colorMap <-
    data.table(gbtype = uniqueColorByParams,
               color = brewer.pal(length(uniqueColorByParams), "Paired"))
  setkey(colorMap, gbtype)
  flatMap <- data.table(
    seqI = (gbSequenceStart + gbSequence@offset):
      (gbSequence@length + gbSequence@offset),
    gbSeq = strsplit(as.character(gbSequence), NULL)[[1]],
    map = as.numeric(NA)
  )
  setkey(flatMap, seqI)
  features[, addFlat(flatMap, ID, start, end), by = ID]
  # features[, addFeatureToLayers(allLayers[[strand]], ID, start, end), by = ID]

  flatMap[, color := ifelse(is.na(map),
                            "",
                            colorMap[features[map, get(colorBy)], color]),
          by = seqI]
  flatMap[is.na(map), map := 0]
  flatMap[, dif := map != shift(map, fill = FALSE)]
  flatMap[,
          coloredSeq :=
            ifelse(dif,
                   ifelse(map == 0,
                          sprintf("</span>%s", gbSeq),
                          sprintf("</span><span style='background-color: %s' title='%s'>%s",
                                  color,
                                  features[map, get(labelBy)],
                                  gbSeq)),
                   gbSeq),
          by = seqI]
  if (linesWidth != 0) {
    flatMap[seq(1, nrow(flatMap), by = linesWidth),
            coloredSeq := sprintf("<br>%s", coloredSeq)]
  }
  paste0(c(flatMap$coloredSeq, "</span>"), collapse = "")
}


