# ggrain

The goal is to have a geom_rain() so the following code runs:

```
ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_rain()
```

This should work for one timepoint and many.

List of priorities:

- flips based on x & y arg
- has a longitudinal option
- allows customization
- has an automatic scalling function?


Eventually it should have a pre/post option with flanking/grouped rainclouds.