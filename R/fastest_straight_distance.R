# ============================================================
# Fastest straight-line distance analysis for GPS tracks
# ============================================================
# Finds, for each athlete, the fastest predicted time to cover
# a given straight-line distance (default 500 m), using:
#
# 1) First point >= distance from each start
# 2) Average speed between start and that point
# 3) Predicted time for exactly `distance`
#
# Tracks are automatically split when consecutive timestamps
# differ by more than `max_gap` seconds (default = 20).
#
# No interpolation of geometry or time.
# ============================================================

library(sf)

# ------------------------------------------------------------
# INTERNAL: fastest predicted distance for one continuous track
# ------------------------------------------------------------
.fastest_distance_one <- function(
  x,
  y,
  time,
  distance,
  min_time = NULL,
  max_speed = NULL
) {
  n <- length(x)
  best_t_pred <- Inf
  best_i <- NA_integer_
  best_j <- NA_integer_

  for (i in seq_len(n - 1)) {
    xi <- x[i]
    yi <- y[i]
    ti <- time[i]

    for (j in (i + 1):n) {
      dx <- x[j] - xi
      dy <- y[j] - yi
      d <- sqrt(dx^2 + dy^2)

      if (d >= distance) {
        dt_ij <- as.numeric(time[j] - ti)

        # Optional plausibility filters
        if (!is.null(min_time) && dt_ij < min_time) {
          break
        }
        if (!is.null(max_speed) && (d / dt_ij) > max_speed) {
          break
        }

        v_avg <- d / dt_ij
        t_pred <- distance / v_avg

        if (t_pred < best_t_pred) {
          best_t_pred <- t_pred
          best_i <- i
          best_j <- j
        }
        break
      }
    }
  }

  if (is.infinite(best_t_pred)) {
    return(NULL)
  }

  list(
    t_pred_sec = best_t_pred,
    start_index = best_i,
    end_index = best_j
  )
}

# ------------------------------------------------------------
# PUBLIC: fastest straight-line distance per athlete
# ------------------------------------------------------------
fastest_straight_distance <- function(
  sf_points,
  athlete_col,
  time_col,
  distance = 500,
  max_gap = 20, # seconds (DEFAULT)
  min_time = NULL,
  max_speed = NULL
) {
  stopifnot(inherits(sf_points, "sf"))
  if (st_crs(sf_points)$units_gdal != "metre") {
    stop("CRS is geographic (degrees). Project to a planar CRS first.")
  }

  # Extract coordinates (must already be projected in meters)
  coords <- st_coordinates(sf_points)

  dt <- data.table::as.data.table(sf_points)
  data.table::setDT(dt)
  dt[, `:=`(
    x = coords[, 1],
    y = coords[, 2],
    time_num = as.numeric(get(time_col))
  )]
  data.table::setorderv(dt, cols = c(athlete_col, "time_num"))

  # ----------------------------------------------------------
  # Split tracks by time gaps
  # ----------------------------------------------------------
  dt[, segment_id := 1L, by = athlete_col]

  if (!is.null(max_gap)) {
    dt[,
      segment_id := cumsum(
        c(TRUE, diff(time_num) > max_gap)
      ),
      by = athlete_col
    ]
  }

  # ----------------------------------------------------------
  # Compute fastest distance per (athlete, segment)
  # ----------------------------------------------------------
  seg_res <- dt[,
    {
      out <- .fastest_distance_one(
        x = x,
        y = y,
        time = get(time_col),
        distance = distance,
        min_time = min_time,
        max_speed = max_speed
      )

      if (is.null(out)) {
        NULL
      } else {
        i <- out$start_index
        j <- out$end_index

        d_end <- sqrt((x[j] - x[i])^2 + (y[j] - y[i])^2)
        dt_ij <- as.numeric(get(time_col)[j] - get(time_col)[i])

        .(
          distance_m = distance,
          predicted_time_sec = out$t_pred_sec,
          avg_speed_mps = d_end / dt_ij,
          start_time = get(time_col)[i],
          end_time = get(time_col)[j],
          chord_length_m = d_end,
          start_x = x[i],
          start_y = y[i],
          end_x = x[j],
          end_y = y[j],
          segment_id = segment_id[1]
        )
      }
    },
    by = .(athlete = get(athlete_col), segment_id)
  ]

  # Keep only valid segment results
  if (nrow(seg_res) == 0) {
    return("None found")
  }
  seg_res <- seg_res[!is.na(predicted_time_sec)]
  # ----------------------------------------------------------
  # Keep best segment per athlete
  # ----------------------------------------------------------
  final <- seg_res[,
    .SD[which.min(predicted_time_sec)],
    by = athlete
  ]

  data.table::setnames(final, "athlete", athlete_col)
  final[, avg_speed_kmh := avg_speed_mps * 60 * 60 / 1000]
  final[,]
}

# ------------------------------------------------------------
# OPTIONAL: build sf LINESTRING geometry for results
# ------------------------------------------------------------
fastest_straight_geometry <- function(result_dt, crs) {
  lines <- mapply(
    function(x1, y1, x2, y2) {
      st_linestring(
        matrix(c(x1, y1, x2, y2), ncol = 2, byrow = TRUE)
      )
    },
    result_dt$start_x,
    result_dt$start_y,
    result_dt$end_x,
    result_dt$end_y,
    SIMPLIFY = FALSE
  )

  st_as_sf(
    result_dt,
    geometry = st_sfc(lines, crs = crs)
  )
}
