#Teorema de taylor

num = 1+0.5+(0.5^2/2)+(0.5^3/6)+(0.5^4/24)+(0.5^5/120) 

trunc_number_n_decimals <- function(numberToTrunc, nDecimals)
{
  numberToTrunc <- numberToTrunc + (10^-(nDecimals+5))
  splitNumber <- strsplit(x=format(numberToTrunc, digits=20, format=f), split="\\.")[[1]]
  decimalPartTrunc <- substr(x=splitNumber[2], start=1, stop=nDecimals)
  truncatedNumber <- as.numeric(paste0(splitNumber[1], ".", decimalPartTrunc))
  return(truncatedNumber)
}

num_trunc = trunc_number_n_decimals(num, 4)

cat("El resutado es =", num_trunc)