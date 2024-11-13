#' Create an ideogram plot for primary genomic assemblies
#'
#' This function generates a basic ideogram plot of chromosomes, including the chromosome length and the telomere regions.
#' It visualizes the telomeres by marking the start and end points, and adjusting the plot appearance according to
#' specified parameters such as color, size, and scaling. This function was built to use datasets generated with genome_table
#'
#' @param genome.table A data frame containing genomic data, with columns for chromosome names, chromosome lengths,
#'        and the start and end positions of telomere regions (`begin_telo_start`, `begin_telo_end`, `end_telo_start`,
#'        `end_telo_end`, `begin_telo_bp`, and `end_telo_bp`).
#' @param plot_title A character string specifying the plot title. Default is `NULL`, in which case no title is added.
#' @param chr_color A character string specifying the color of the chromosome segments. Default is "dodgerblue2".
#' @param chr_size A numeric value specifying the size (linewidth) of the chromosome segments. Default is 8.
#' @param tel_color A character string specifying the color of the telomere points. Default is "black".
#' @param tel_shape A numeric value specifying the shape of the telomere points. Default is 16 (filled circle).
#' @param y_scale A numeric value for scaling the y-axis. Default is `1e-6` (for scaling the length to Mb).
#' @param y_scale_suffix A character string to append to the y-axis labels, usually a unit suffix like "Mb". Default is "Mb".
#' @param legend_pos A character string specifying the position of the legend. Default is "bottom".
#' @param legend_size A numeric value specifying the size of the legend keys. Default is 0.25.
#'
#' @return A `ggplot` object representing the ideogram plot with chromosome and telomere information visualized.
#'
#' @examples
#' # Example usage
#' genome.table <- data.frame(
#'   Chromosome = c("Chr1", "Chr2"),
#'   Length = c(100000000, 200000000),
#'   begin_telo_start = c(0, 0),
#'   begin_telo_end = c(5000000, 10000000),
#'   end_telo_start = c(95000000, 190000000),
#'   end_telo_end = c(100000000, 200000000),
#'   begin_telo_bp = c(5000000, 10000000),
#'   end_telo_bp = c(5000000, 10000000)
#' )
#' primary_ideogram(genome.table)
#'
#' @importFrom ggplot2 ggplot aes geom_segment geom_point scale_y_continuous labs theme_classic theme unit
#' @importFrom scales label_number
#'
#' @export
primary_ideogram <- function(genome.table, plot_title = NULL, chr_color = "dodgerblue2", chr_size = 8,
                             tel_color = "black", tel_shape = 16, y_scale = 1e-6, y_scale_suffix = "Mb",
                             legend_pos = "bottom", legend_size = 0.25) {

  p <- genome.table %>%
    ggplot(aes(x = Chromosome, y = Length)) +
    geom_segment(aes(y = begin_telo_start, yend = Length),
                 color = chr_color,
                 linewidth = chr_size,
                 lineend = "round") +
    geom_point(aes(x = Chromosome, y = begin_telo_end, size = begin_telo_bp),
               shape = tel_shape,
               color = tel_color) +
    geom_point(aes(x = Chromosome, y = end_telo_end, size = end_telo_bp),
               shape = tel_shape,
               color = tel_color) +
    scale_y_continuous(labels = label_number(scale = y_scale, suffix = y_scale_suffix)) +
    labs(y = "Chromosome Length", x = "Chromosome", size = "Telomere Length", title = plot_title) +
    theme_classic(base_size = 6) +
    theme(legend.position = legend_pos,
          legend.key.size = unit(legend_size, "cm"),
          plot.title = element_text(hjust = 0.5, face = "bold"))

  return(p)
}
