#' Generate an ideogram plot from a fasta file
#'
#' This function creates an ideogram plot based on a genome sequence provided in a FASTA file. It reads the genome,
#' filters for specified chromosomes, calculates telomeric repeat counts, and visualizes the chromosome lengths with telomere regions marked.
#' The output includes a data table and a `ggplot` generated ideogram plot.
#'
#' @param path_fasta A character string specifying the file path to the fasta file containing the genome sequence.
#' @param chr_names A character string specifying the pattern to identify chromosome names in the data. Default is "Chr".
#' @param tel_start_seq A character string representing the telomeric repeat sequence at the start of the chromosome. Default is "CCCTAAA".
#' @param tel_end_seq A character string representing the telomeric repeat sequence at the end of the chromosome. Default is "TTTAGGG".
#' @param size_windows A numeric value specifying the window size (in base pairs) for counting telomeric repeats. Default is `1e6` (1 Mb).
#' @param min_tel_count A numeric value representing the minimum number of telomeric repeats required to retain telomeric counts in the plot. Default is 25.
#' @param sample_name A character string specifying the name of the sample for labeling purposes. Default is `NULL`.
#' @param title_plot A character string specifying the title of the plot. Default is `NULL`.
#' @param color_chr A character string specifying the color of the chromosome segments in the plot. Default is "dodgerblue2".
#' @param size_chr A numeric value specifying the size (linewidth) of the chromosome segments. Default is 8.
#' @param color_tel A character string specifying the color of the telomere points. Default is "black".
#' @param shape_tel A numeric value specifying the shape of the telomere points. Default is 16 (filled circle).
#' @param scale_y A numeric value for scaling the y-axis. Default is `1e-6` to convert lengths to megabases (Mb).
#' @param suffix_y_scale A character string to append to the y-axis labels, usually a unit like "Mb". Default is "Mb".
#' @param pos_legend A character string specifying the position of the legend. Default is "bottom".
#' @param size_legend A numeric value specifying the size of the legend keys. Default is 0.25.
#'
#' @return A list containing:
#' \describe{
#'   \item{genomic.table}{A data frame with chromosome, length, and telomere information.}
#'   \item{ideogram}{A `ggplot` object representing the ideogram plot.}
#' }
#'
#' @examples
#' # Example usage
#' # ggideoprimary(path_fasta = "path/to/genome.fasta", sample_name = "Sample_1", title_plot = "Genome Ideogram")
#'
#' @importFrom Biostrings readDNAStringSet
#' @importFrom data.table data.table
#' @importFrom ggplot2 ggplot aes geom_segment geom_point scale_y_continuous labs theme_classic theme unit
#' @importFrom scales label_number
#' @importFrom dplyr filter
#'
#' @export
ggideoprimary <- function(path_fasta, chr_names = "Chr", tel_start_seq = "CCCTAAA", tel_end_seq = "TTTAGGG",
                          size_windows = 1e6, min_tel_count = 25, sample_name = NULL, title_plot = NULL, color_chr = "dodgerblue2",
                          size_chr = 8, color_tel = "black", shape_tel = 16, scale_y = 1e-6, suffix_y_scale = "Mb",
                          pos_legend = "bottom", size_legend = 0.25){

  # Read in fasta
  genome <- readDNAStringSet(path_fasta)

  # Create table of contigs, chromosomes, and lengths
  length.table <- data.table(Chromosome = names(genome), Length = width(genome))

  # Filter for only chromosomes, based on starting string
  length.table <- select_chr(length.table, chr_string = chr_names)

  # Extract the size of the genome
  genome.size <- sum(length.table$Length)

  # Count telomeric sequence repeat
  tel_count.table <- telomere_repeat_number(fasta = genome, window = size_windows, tel_start = tel_start_seq, tel_end = tel_end_seq)

  # Filter to maintain telomeric counts over a certain threshold
  tel.table <- tel_count.table %>%
    filter(CCCTAAA_Counts >= min_tel_count | TTTAGGG_Counts >= min_tel_count)

  # Create the larger table necessary for plotting
  plotting.table <- genome_table(length.table, tel.table, name = sample_name, genome_size = genome.size)

  # Remove leading 0s for proper ordering and plotting
  plotting.table <- remove_lead_0s(plotting.table, chr_string = chr_names)

  # Set levels so that chromosomes are plotted in the proper order by number
  plotting.table$Chromosome <- factor(plotting.table$Chromosome,
    levels = unique(plotting.table$Chromosome)[order(as.numeric(gsub(chr_names, "", unique(plotting.table$Chromosome))))])


  # plot the ideogram
  graphic <- primary_ideogram(plotting.table, plot_title = title_plot, chr_color = color_chr, chr_size = size_chr,
                        tel_color = color_tel, tel_shape = shape_tel, y_scale = scale_y, y_scale_suffix = suffix_y_scale,
                        legend_pos = pos_legend, legend_size = size_legend)

  return(list(genomic.table = plotting.table, ideogram = graphic))
}

