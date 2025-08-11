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
    check.names = FALSE
  )

  for (obj_name in objs) {
    # be robust to weird promises/active bindings
    obj <- try(get(obj_name, envir = .GlobalEnv), silent = TRUE)
    if (inherits(obj, "try-error")) next

    obj_class <- class(obj)[1]
    obj_type  <- typeof(obj)
    obj_size  <- format(object.size(obj), units = "auto")

    obj_dim <- dim(obj)
    dim_str <- if (is.null(obj_dim)) {
      if (length(obj) > 1) paste0("1 x ", length(obj)) else NA
    } else {
      paste(obj_dim, collapse = " x ")
    }

    len <- length(obj)

    # elements/args count
    elem_or_args <- if (is.function(obj)) length(formals(obj)) else len

    # preview builder
    obj_preview <- ""

    if (is.function(obj)) {
      args <- formals(obj)
      if (length(args) == 0) {
        obj_preview <- "(No Arguments)"
      } else {
        arg_names <- names(args)
        if (is.null(arg_names)) arg_names <- rep("unnamed", length(args))
        take <- seq_len(min(3, length(args)))
        formatted_args <- mapply(function(n, val) {
          if (n == "") {
            deparse(val)[1]
          } else if (is.symbol(val)) {
            n
          } else {
            paste0(n, " = ", deparse(val)[1])
          }
        }, arg_names[take], args[take], SIMPLIFY = TRUE, USE.NAMES = FALSE)
        obj_preview <- paste(formatted_args, collapse = ", ")
      }

    } else if (is.data.frame(obj)) {
      # show first 3 column names
      preview_vals <- names(obj)[seq_len(min(3, ncol(obj)))]
      preview_vals <- preview_vals[!is.na(preview_vals)]
      obj_preview <- paste(preview_vals, collapse = ", ")

    } else if (is.matrix(obj)) {
      # matrices are atomic with dim; preview first 3 elements (vectorized)
      preview_vals <- as.vector(obj)[seq_len(min(3, length(obj)))]
      if (is.numeric(preview_vals)) preview_vals <- round(preview_vals, 2)
      preview_vals <- as.character(preview_vals[!is.na(preview_vals)])
      obj_preview <- paste(preview_vals, collapse = ", ")

    } else if (is.factor(obj)) {
      # show up to first 3 levels
      lvls <- levels(obj)
      if (!is.null(lvls)) {
        preview_vals <- lvls[seq_len(min(3, length(lvls)))]
        preview_vals <- preview_vals[!is.na(preview_vals)]
        obj_preview <- paste(preview_vals, collapse = ", ")
      }

    } else if (is.atomic(obj)) {
      # generic vectors (numeric, character, logical, etc.)
      take <- seq_len(min(3, len))
      preview_vals <- obj[take]
      if (is.numeric(preview_vals)) preview_vals <- round(preview_vals, 2)
      preview_vals <- preview_vals[!is.na(preview_vals)]

      # wrap character values in quotes
      if (is.character(preview_vals)) {
        preview_vals <- sprintf('"%s"', preview_vals)
      } else if (is.logical(preview_vals)) {
        preview_vals <- ifelse(preview_vals, "TRUE", "FALSE")
      } else {
        preview_vals <- as.character(preview_vals)
      }

      obj_preview <- paste(preview_vals, collapse = ", ")

    } else if (is.list(obj)) {
      # show first 3 element names (or "unnamed")
      nm <- names(obj)
      if (is.null(nm)) {
        preview_vals <- rep("unnamed", min(3, length(obj)))
      } else {
        preview_vals <- nm[seq_len(min(3, length(nm)))]
        preview_vals <- preview_vals[!is.na(preview_vals) & nzchar(preview_vals)]
        if (length(preview_vals) == 0) {
          preview_vals <- rep("unnamed", min(3, length(obj)))
        }
      }
      obj_preview <- paste(preview_vals, collapse = ", ")

    } else {
      # fallback: just note the class
      obj_preview <- paste0("<", obj_class, ">")
    }

    # trim preview to 80 chars for display
    preview_trunc <- substr(obj_preview, 1, 80)

    summary_df <- rbind(
      summary_df,
      data.frame(
        Name = obj_name,
        Class = obj_class,
        Type = obj_type,
        Size = obj_size,
        Dimensions = dim_str,
        "Elements (or Arguments)" = elem_or_args,
        "First 3 Elements (or Arguments)" = preview_trunc,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    )
  }

  summary_df
}
