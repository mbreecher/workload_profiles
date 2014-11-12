lm_eqn = function(y, x){
  m=lm(y ~ poly(x, 4, raw = T))
  eq <- substitute(italic(y) == a + b %.% italic(x)+ c %.% italic(x)^2~ + d %.% italic(x)^3~ + e %.% italic(x)^4~ ","~~italic(r)^2~"="~r2,
                 list(a = format(coef(m)[1], digits = 3),
                      b = format(coef(m)[2], digits = 3),
                      c = format(coef(m)[3], digits = 3),
                      d = format(coef(m)[4], digits = 3),
                      e = format(coef(m)[5], digits = 3),
                      r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq))
}