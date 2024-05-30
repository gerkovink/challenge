library(ggmice)
mdgerko <-
  function (data,
            vrb = "all",
            square = FALSE,
            rotate = FALSE,
            cluster = NULL)
  {
    if (!is.data.frame(data) & !is.matrix(data)) {
      stop("Dataset should be a 'data.frame' or 'matrix'.")
    }
    if (vrb == "all") {
      vrb <- names(data)
    }
    if (!is.null(cluster)) {
      if (cluster %nin% names(data[, vrb])) {
        stop(
          "Cluster variable not recognized, please provide the variable name as a character string."
        )
      }
    }
    pat <- mice::md.pattern(data[, vrb], plot = FALSE)
    pat[, 10] <- 9
    pat[14,] <- c(rep(748, 9), 6732)
    rws <- nrow(pat)
    cls <- ncol(pat)
    vrb <- colnames(pat)[-cls]
    frq <- row.names(pat)[-rws]
    na_row <- pat[-rws, cls]
    na_col <- pat[rws,-cls]
    if (is.null(cluster)) {
      pat_clean <- cbind(.opacity = 1, pat[-rws, vrb])
    }
    else {
      pats <- purrr::map(split(data[, vrb], ~ get(cluster)),
                         ~ {
                           mice::md.pattern(., plot = FALSE) %>% pat_to_chr(.,
                                                                            ord = vrb)
                         })
      pat_used <- purrr::map_dfr(pats, ~ {
        pat_to_chr(pat) %in% .x
      }) %>% rowMeans()
      pat_clean <- data.frame(.opacity = pat_used, pat[-rws,
                                                       vrb])
    }
    long <-
      data.frame(y = 1:(rws - 1), pat_clean, row.names = NULL) %>%
      tidyr::pivot_longer(cols = vrb,
                          names_to = "x",
                          values_to = ".where") %>%
      dplyr::mutate(
        x = as.numeric(factor(
          .data$x, levels = vrb,
          ordered = TRUE
        )),
        .where = factor(
          .data$.where,
          levels = c(0,
                     1),
          labels = c("imputed", "overimputed")
        ),
        .opacity = as.numeric(.data$.opacity)
      )
    gg <- ggplot2::ggplot(
      long,
      ggplot2::aes(
        .data$x,
        .data$y,
        fill = .data$.where,
        alpha = 0.1 + .data$.opacity / 2
      )
    ) +
      ggplot2::geom_tile(color = "black") + ggplot2::scale_fill_manual(values = c(overimputed = "#B61A51B3",
                                                                                  imputed = "#B61A51B3")) + ggplot2::scale_alpha_continuous(limits = c(0,
                                                                                                                                                       1), guide = "none") + ggplot2::scale_x_continuous(
                                                                                                                                                         breaks = 1:(cls -
                                                                                                                                                                       1),
                                                                                                                                                         labels = na_col,
                                                                                                                                                         sec.axis = ggplot2::dup_axis(labels = vrb,
                                                                                                                                                                                      name = "Variable\n(name)")
                                                                                                                                                       ) + ggplot2::scale_y_reverse(
                                                                                                                                                         breaks = 1:(rws -
                                                                                                                                                                       1),
                                                                                                                                                         labels = frq,
                                                                                                                                                         sec.axis = ggplot2::dup_axis(labels = na_row,
                                                                                                                                                                                      name = "Pattern\n(number of (over)imputed entries)")
                                                                                                                                                       ) + ggplot2::labs(
                                                                                                                                                         x = "Variable\n(number of (over)imputed entries)",
                                                                                                                                                         y = "Pattern\n(frequency)",
                                                                                                                                                         fill = "",
                                                                                                                                                         alpha = ""
                                                                                                                                                       ) +
      ggmice:::theme_minimice()
    if (square) {
      gg <- gg + ggplot2::coord_fixed()
    }
    if (rotate) {
      gg <-
        gg + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
    }
    return(gg)
  }

plot1 <- ggmice::plot_pattern(boys, square = TRUE, rotate = TRUE)
plot2 <- mdgerko(boys, square = TRUE, rotate = TRUE)

library(gridExtra)
grid.arrange(plot1, plot2, ncol = 2)