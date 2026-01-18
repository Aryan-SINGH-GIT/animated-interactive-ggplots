library(animint2)
library(dplyr)
library(grDevices)

# -------------------------
# Properly Styled kNN Animation
# -------------------------
knn_animint <- function(train, test, cl, k = 30, 
                        interval = 1000,
                        out.dir = "knn_animint",
                        github_repo = NULL) {
  
  # -------------------------
  # 1. Prepare Data
  # -------------------------
  train_df <- as.data.frame(train)
  colnames(train_df) <- c("x", "y")
  train_df$class <- as.character(cl)
  train_df$train_id <- 1:nrow(train_df)
  
  test_df <- as.data.frame(test)
  colnames(test_df) <- c("x", "y")
  n_test <- nrow(test_df)
  
  # k sequence
  if (k <= 5) {
    k_sequence <- 1:k
  } else {
    k_sequence <- unique(c(seq(1, min(10, k), by = 1), seq(15, k, by = 5)))
    if (!k %in% k_sequence) k_sequence <- c(k_sequence, k)
    k_sequence <- sort(k_sequence)
  }
  
  # -------------------------
  # 2. Build Animation Frames
  # -------------------------
  all_data <- list()
  frame_id <- 1
  
  for (test_idx in 1:n_test) {
    test_point <- test_df[test_idx, ]
    
    # Calculate distances
    distances <- sqrt((train_df$x - test_point$x)^2 + (train_df$y - test_point$y)^2)
    train_sorted <- train_df
    train_sorted$distance <- distances
    train_sorted <- train_sorted[order(distances), ]
    train_sorted$rank <- 1:nrow(train_sorted)
    
    for (k_val in k_sequence) {
      
      train_sorted$is_knn <- train_sorted$rank <= k_val
      
      # Votes
      knn_subset <- train_sorted[train_sorted$is_knn, ]
      votes <- knn_subset %>%
        group_by(class) %>%
        summarise(votes = n(), .groups = "drop")
      
      predicted <- votes %>% arrange(desc(votes)) %>% slice(1) %>% pull(class)
      
      # Training points
      train_frame <- train_sorted %>%
        mutate(
          frame_id = frame_id,
          k = k_val,
          test_idx = test_idx,
          is_neighbor = is_knn
        )
      
      # LINES TO K-NEAREST NEIGHBORS (the missing dotted lines!)
      lines_frame <- train_sorted %>%
        filter(is_knn) %>%
        mutate(
          x_start = test_point$x,
          y_start = test_point$y,
          x_end = x,
          y_end = y,
          frame_id = frame_id,
          line_group = paste0("test", test_idx, "_train", train_id)
        )
      
      # Convex hull polygon
      polygon_frame <- data.frame()
      if (sum(train_sorted$is_knn) >= 3) {
        knn_points <- train_sorted[train_sorted$is_knn, ]
        hull_idx <- chull(knn_points$x, knn_points$y)
        polygon_frame <- knn_points[hull_idx, c("x", "y")]
        polygon_frame$frame_id <- frame_id
        polygon_frame$poly_group <- paste0("test", test_idx, "_k", k_val)
      }
      
      # Circle at k-th neighbor
      radius <- train_sorted$distance[k_val]
      angles <- seq(0, 2*pi, length.out = 100)
      circle_frame <- data.frame(
        x = test_point$x + radius * cos(angles),
        y = test_point$y + radius * sin(angles),
        frame_id = frame_id,
        circle_group = paste0("test", test_idx, "_k", k_val)
      )
      
      # Current test point
      test_current <- data.frame(
        x = test_point$x,
        y = test_point$y,
        frame_id = frame_id
      )
      
      # Previously classified
      prev_classified <- data.frame()
      if (test_idx > 1) {
        for (prev_idx in 1:(test_idx - 1)) {
          prev_point <- test_df[prev_idx, ]
          prev_dist <- sqrt((train_df$x - prev_point$x)^2 + (train_df$y - prev_point$y)^2)
          prev_sorted <- train_df[order(prev_dist), ]
          prev_knn <- prev_sorted[1:k, ]
          prev_vote <- prev_knn %>%
            group_by(class) %>%
            summarise(n = n(), .groups = "drop") %>%
            arrange(desc(n)) %>%
            slice(1)
          
          prev_classified <- rbind(prev_classified, data.frame(
            x = prev_point$x,
            y = prev_point$y,
            class = prev_vote$class,
            frame_id = frame_id
          ))
        }
      }
      
      # Future unclassified
      future_points <- data.frame()
      if (test_idx < n_test) {
        future_points <- test_df[(test_idx + 1):n_test, ] %>%
          mutate(frame_id = frame_id)
      }
      
      # Votes
      votes_frame <- votes %>%
        mutate(frame_id = frame_id, k = k_val)
      
      # Title
      title_text <- sprintf("Test Point %d/%d | k = %d | Predicted: %s", 
                            test_idx, n_test, k_val, predicted)
      
      all_data[[frame_id]] <- list(
        training = train_frame,
        lines = lines_frame,      # THE DOTTED LINES!
        polygon = polygon_frame,
        circle = circle_frame,
        test_current = test_current,
        test_prev = prev_classified,
        test_future = future_points,
        votes = votes_frame,
        title = data.frame(
          x = mean(range(train_df$x)),
          y = max(train_df$y) + 0.8,
          label = title_text,
          frame_id = frame_id
        )
      )
      
      frame_id <- frame_id + 1
    }
  }
  
  # -------------------------
  # 3. Combine Data
  # -------------------------
  bind_list <- function(data_list, element_name) {
    bind_rows(lapply(data_list, function(x) x[[element_name]]))
  }
  
  training_all <- bind_list(all_data, "training")
  lines_all <- bind_list(all_data, "lines")
  polygon_all <- bind_list(all_data, "polygon")
  circle_all <- bind_list(all_data, "circle")
  test_current_all <- bind_list(all_data, "test_current")
  test_prev_all <- bind_list(all_data, "test_prev")
  test_future_all <- bind_list(all_data, "test_future")
  votes_all <- bind_list(all_data, "votes")
  title_all <- bind_list(all_data, "title")
  
  # -------------------------
  # 4. Color Scheme
  # -------------------------
  unique_classes <- unique(cl)
  n_classes <- length(unique_classes)
  
  color_values <- c("#4472C4", "#70AD47", "#FFC000", "#C00000")[1:n_classes]
  names(color_values) <- unique_classes
  
  shape_values <- c(1, 2, 0, 5)[1:n_classes]
  names(shape_values) <- unique_classes
  
  # Calculate proper axis ranges
  x_range <- range(c(train_df$x, test_df$x))
  y_range <- range(c(train_df$y, test_df$y))
  x_padding <- diff(x_range) * 0.1
  y_padding <- diff(y_range) * 0.1
  
  # -------------------------
  # 5. Create Plots (NORMAL SIZE POINTS!)
  # -------------------------
  
  # Feature Space Plot
  p_feature <- ggplot() +
    # Polygon
    geom_polygon(
      data = polygon_all,
      aes(x = x, y = y, group = poly_group),
      fill = "#FFFFCC",
      alpha = 0.4,
      showSelected = "frame_id"
    ) +
    # Circle
    geom_path(
      data = circle_all,
      aes(x = x, y = y, group = circle_group),
      color = "gray40",
      linetype = "dashed",
      size = 1,
      showSelected = "frame_id"
    ) +
    # DOTTED LINES TO K-NEAREST NEIGHBORS (CRITICAL!)
    geom_segment(
      data = lines_all,
      aes(x = x_start, y = y_start, xend = x_end, yend = y_end, group = line_group),
      color = "gray60",
      linetype = "dotted",
      size = 0.8,
      alpha = 0.6,
      showSelected = "frame_id"
    ) +
    # Non-neighbor points (faded)
    geom_point(
      data = training_all %>% filter(!is_neighbor),
      aes(x = x, y = y, color = class, shape = class),
      size = 2.5,
      alpha = 0.3,  # Very faded
      showSelected = "frame_id"
    ) +
    # K-nearest neighbors (SAME SIZE but brighter!)
    geom_point(
      data = training_all %>% filter(is_neighbor),
      aes(x = x, y = y, color = class, shape = class),
      size = 2.5,  # SAME SIZE as non-neighbors
      alpha = 1.0,  # Fully opaque for color highlight
      stroke = 1.2,  # Thicker border
      showSelected = "frame_id"
    ) +
    # Previously classified (same size, thicker border)
    geom_point(
      data = test_prev_all,
      aes(x = x, y = y, color = class, shape = class),
      size = 2.5,  # Same size
      alpha = 1.0,
      stroke = 1.5,
      showSelected = "frame_id"
    ) +
    # Future test points (more visible!)
    geom_text(
      data = test_future_all,
      aes(x = x, y = y),
      label = "?",
      color = "#8B4513",  # Darker brown for better visibility
      size = 8,  # Increased from 6
      fontface = "bold",
      vjust = 0.5,  # Center vertically
      showSelected = "frame_id"
    ) +
    # Current test point (slightly bigger, bright red)
    geom_point(
      data = test_current_all,
      aes(x = x, y = y),
      shape = 19,
      color = "#FF0000",  # Bright red
      size = 3,  # Only slightly bigger
      alpha = 1.0,
      showSelected = "frame_id"
    ) +
    # Outer ring for current test point
    geom_point(
      data = test_current_all,
      aes(x = x, y = y),
      shape = 1,
      color = "#FF0000",
      size = 4,
      stroke = 2,
      showSelected = "frame_id"
    ) +
    # Title
    geom_text(
      data = title_all,
      aes(x = x, y = y, label = label),
      size = 5,
      fontface = "bold",
      vjust = 0,
      showSelected = "frame_id"
    ) +
    scale_color_manual(values = color_values, name = "Class") +
    scale_shape_manual(values = shape_values, name = "Class") +
    scale_x_continuous(
      breaks = pretty(x_range, n = 10),
      limits = c(x_range[1] - x_padding, x_range[2] + x_padding)
    ) +
    scale_y_continuous(
      breaks = pretty(y_range, n = 10),
      limits = c(y_range[1] - y_padding, y_range[2] + y_padding)
    ) +
    coord_fixed() +
    labs(x = "x₁", y = "x₂") +
    theme_bw() +
    theme(
      legend.position = "top",
      panel.grid.major = element_line(color = "gray85"),
      panel.grid.minor = element_line(color = "gray92"),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12)
    )
  
  # Vote Chart
  p_votes <- ggplot() +
    geom_bar(
      data = votes_all,
      aes(x = class, y = votes, fill = class),
      stat = "identity",
      position = "identity",
      width = 0.6,
      showSelected = "frame_id"
    ) +
    scale_fill_manual(values = color_values) +
    scale_y_continuous(
      breaks = seq(0, k, by = 5),
      limits = c(0, k)
    ) +
    labs(
      title = "Classification Votes",
      x = "Class",
      y = "Votes"
    ) +
    theme_bw() +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 11)
    )
  
  # -------------------------
  # 6. Create animint (HORIZONTAL LAYOUT)
  # -------------------------
  viz <- animint(
    featureSpace = p_feature,
    voteChart = p_votes,
    title = "k-Nearest Neighbors Classification",
    source = "https://github.com/Aryan-SINGH-GIT/animated-interactive-ggplots/blob/master/02_animint2_interactions/knn_animint.R",
    time = list(
      variable = "frame_id",
      ms = interval
    ),
    first = list(frame_id = 1),
    width = list(
      featureSpace = 900,   # Larger horizontal
      voteChart = 400
    ),
    height = list(
      featureSpace = 700,   # Good vertical size
      voteChart = 500
    )
  )
  
  # -------------------------
  # 7. Export to GitHub Pages
  # -------------------------
  if (!is.null(github_repo)) {
    # Deploy to GitHub Pages
    animint2pages(viz, out.dir = out.dir, github_repo = github_repo)
  } else {
    # If no repo specified, open locally with animint2dir
    message("No github_repo specified. Opening locally with animint2dir().")
    message("To deploy to GitHub Pages, provide github_repo parameter.")
    animint2dir(viz, out.dir = out.dir, open.browser = TRUE)
  }
  
  return(invisible(viz))
}

# -------------------------
# Example Usage - Deploy to GitHub Pages
# -------------------------
set.seed(1)

x <- matrix(c(rnorm(80, mean = -1), rnorm(80, mean = 1)), ncol = 2, byrow = TRUE)
y <- matrix(rnorm(10, mean = 0, sd = 1.2), ncol = 2)

# DEPLOY TO GITHUB PAGES
# Replace "your-username" with your actual GitHub username
knn_animint(
  train = x,
  test = y,
  cl = rep(c("first class", "second class"), each = 40),
  k = 30,
  interval = 1000,
  out.dir = "knn_animint",
  github_repo = "knn-animint"  # Your repo name
)