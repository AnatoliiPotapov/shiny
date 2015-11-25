plot_ggplot <- function(data, nls) {
  df <- data.frame(
    x = data[,1],
    y = data[,3],
    y_nls = predict(nls[[2]])
  )
  dataset = melt(df, id.vars = c("x"))
  ggplot(data = dataset, aes(x=x, y=value, group=variable, colour=variable)) +
    geom_line() +
    xlab("Номер такта") +
    ylab("Температура, С") +
    theme(legend.position = "bottom")
}