six <- 0
no_six <- 0


for (j in seq_along(breakout_data)) {
  if (nrow(breakout_data[[j]])>0) {
    df1 <- breakout_data[[j]]
    efsize <- df1[2, 26]
    if (efsize > 1) {
      out[[j]] <- nrow(subset(df1, df1$period == "current"))
    }
  }
  
}
break_points <- unlist(out)
if ('6' %in% break_points) {
  six <- six + 1
}
else {
  no_six <- no_six + 1
}


print(six)
print(no_six)
perc <- no_six/(six + no_six)
print(perc)

out <- vector("list", length(subcat_breakout))
for (i in seq_along(subcat_breakout)) {
  df <- subcat_breakout[[i]]
  out[[i]] <- nrow(subset(df, df$period == "current"))
}
break_points <- unlist(out)
print(summary(break_points))
print(table(break_points))