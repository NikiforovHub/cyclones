#model_formula = Delta_P ~ (Pcenter * dmax * d)^2 + I(d^2)
# model_formula = Delta_P ~ (Pcenter * dmax * d)^3 + poly(d,3,raw = T) + poly(dmax,3,raw = T) + poly(Pcenter,3,raw = T)
# model = lm(model_formula, data = model_frame_total)
# print(summary(model))

#model_formula = Delta_P ~ (Pcenter + dmax + d)^2 + I(d^2) + I(dmax^2) + I(Pcenter^2) - dmax - Pcenter:dmax

model_formula = Delta_P ~ Pcenter + d + I(d^2) + Pcenter + I(d/Pcenter) + I(d/dmax)

model = lm(model_formula, data = model_frame)
print(summary(model))
plot(model_frame$Delta_P[1:500], type="l",x = model_frame$d[1:500],
     xlab="Distance", ylab="Pressure difference")
lines(predict(model,newdata = model_frame[1:500,]), type = "l", col=2,x = model_frame$d[1:500])
# lines(symb_model(d = model_frame_total_1$d[1:50]), type = "l", col=3,x = model_frame_total_1$d[1:50])

# model_formula = Delta_P ~ I(Pcenter) + I(Pcenter^2) + I(Pcenter^3) + I(dmax) + I(dmax^2)+ I(dmax^3) + 
#   I(d) + I(d^2) + I(d^3) + I(Pcenter*dmax) + I(Pcenter*d) + I(d*dmax) +
#   I(Pcenter^2*dmax) + I(Pcenter*d^2) + I(d*dmax^2)
# model = lm(model_formula, data = model_frame_total)
# print(summary(model))

model_frame_total$model_predict = predict(model, newdata=model_frame_total)

