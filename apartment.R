apartment.test.data <- read.csv("apartment test data.csv")

model.full = lm(Price ~ (Bedrooms * Bathrooms * Sq..Feet * Distance * Type * factor(Zip.code) * Electric * Furnished), data = na.omit(apartment.test.data))
model.full = lm(Price ~ (Bedrooms * Bathrooms * Sq..Feet * Distance * Type * Electric * Furnished), data = na.omit(apartment.test.data))
#model.full = lm(Price ~ (Bedrooms + Bathrooms + Sq..Feet + Distance + Type + factor(Zip.code) + Electric + Furnished), data = na.omit(apartment.test.data))
summary(model)

model.null = lm(Price ~ 1, data = na.omit(apartment.test.data))

model.final = step(model.null,  # Starting model
     scope = list(lower = model.null, upper = model.full), # Minimum and full models
     direction = "both")  # direction

summary(model.final)

model.full2 = lm(Price ~ (Bedrooms * Bathrooms * Sq..Feet * Type * Electric * Furnished), data = na.omit(apartment.test.data))

model.final2 = step(model.null,  # Starting model
                   scope = list(lower = model.null, upper = model.full2), # Minimum and full models
                   direction = "both")  # direction

model.nested = lm(formula = Price ~ Bathrooms + Bedrooms +  Sq..Feet + 
                    Bathrooms:Bedrooms + Bedrooms:Sq..Feet, 
                  data = na.omit(apartment.test.data))
anova(model.nested, model.final)
