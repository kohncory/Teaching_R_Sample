environment_summary <- function() {
  objs <- ls(envir = .GlobalEnv)
  summary_df <- data.frame(
    Name = character(),
    Class = character(),
    Type = character(),
    Size = character(),
    Dimensions = character(),
    Length = integer(),
    "Elements (or Arguments)" = integer(),
    "First 3 Elements (or Arguments)" = character(),
    stringsAsFactors = FALSE,
    check.names = FALSE  # 🔧 Keep column names readable
  )

  for (obj_name in objs) {
    obj <- get(obj_name, envir = .GlobalEnv)
    obj_class <- class(obj)[1]
    obj_type <- typeof(obj)
    obj_size <- format(object.size(obj), units = "auto")

    obj_dim <- dim(obj)
    dim_str <- if (is.null(obj_dim)) {
      if (length(obj) > 1) paste0("1 x ", length(obj)) else NA
    } else {
      paste(obj_dim, collapse = " x ")
    }

    len <- length(obj)

    # Elements or Arguments count
    elem_or_args <- if (is.function(obj)) {
      length(formals(obj))
    } else {
      len
    }

    # Clean preview
    obj_preview <- ""
    if (is.atomic(obj)) {
      preview_vals <- obj[1:min(3, len)]
      if (is.numeric(preview_vals)) {
        preview_vals <- round(preview_vals, 2)
      }
      preview_vals <- as.character(preview_vals[!is.na(preview_vals)])
      if (is.character(obj)) {
        preview_vals <- paste0(""", preview_vals, """)
      }
      obj_preview <- paste(preview_vals, collapse = ", ")
    } else if (is.matrix(obj)) {
      preview_vals <- obj[1:min(3, length(obj))]
      if (is.numeric(preview_vals)) {
        preview_vals <- round(preview_vals, 2)
      }
      preview_vals <- as.character(preview_vals[!is.na(preview_vals)])
      obj_preview <- paste(preview_vals, collapse = ", ")
    } else if (is.factor(obj)) {
      preview_vals <- as.character(levels(obj)[1:min(3, length(levels(obj)))])
      preview_vals <- preview_vals[!is.na(preview_vals)]
      obj_preview <- paste(preview_vals, collapse = ", ")
    } else if (is.data.frame(obj)) {
      preview_vals <- names(obj)[1:min(3, ncol(obj))]
      preview_vals <- preview_vals[!is.na(preview_vals)]
      obj_preview <- paste(preview_vals, collapse = ", ")
    } else if (is.list(obj)) {
      preview_vals <- names(obj)[1:min(3, length(obj))]
      preview_vals <- preview_vals[!is.na(preview_vals) & nzchar(preview_vals)]
      if (length(preview_vals) == 0) {
        preview_vals <- rep("unnamed", min(3, length(obj)))
      }
      obj_preview <- paste(preview_vals, collapse = ", ")
    } else if (is.function(obj)) {
      args <- formals(obj)
      if (length(args) == 0) {
        obj_preview <- "(No Arguments)"
      } else {
        arg_names <- names(args)
        if (is.null(arg_names)) {
          arg_names <- rep("unnamed", length(args))
        }
        formatted_args <- mapply(function(n, val) {
          if (n == "") {
            deparse(val)[1]
          } else if (is.symbol(val)) {
            n
          } else {
            paste0(n, " = ", deparse(val)[1])
          }
        }, arg_names[1:min(3, length(args))], args[1:min(3, length(args))],
        SIMPLIFY = TRUE, USE.NAMES = FALSE)
        obj_preview <- paste(formatted_args, collapse = ", ")
      }
    }

    summary_df <- rbind(summary_df, data.frame(
      Name = obj_name,
      Class = obj_class,
      Type = obj_type,
      Size = obj_size,
      Dimensions = dim_str,
      "Elements (or Arguments)" = elem_or_args,
      "First 3 Elements (or Arguments)" = substr(obj_preview, 1, 80),
      stringsAsFactors = FALSE,
      check.names = FALSE  # 🔧 preserve readable names here too
    ))
  }

  summary_df
}
