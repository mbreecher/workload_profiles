lm_eqn = function(x, y, order = 4){
  m=lm(y ~ poly(x, order))
  eq <- substitute(italic(y) == a + b %.% italic(x)+ c %.% italic(x)^2~ + d %.% italic(x)^3~ + e %.% italic(x)^4~ ","~~italic(r)^2~"="~r2,
                 list(a = format(coef(m)[1], digits = 2),
                      b = format(coef(m)[2], digits = 2),
                      c = format(coef(m)[3], digits = 2),
                      d = format(coef(m)[4], digits = 2),
                      e = format(coef(m)[5], digits = 2),
                      r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq))
}