
# try geom_smooth to show a fitted curve
ggplot(data=roxbury.sample) + 
  geom_smooth(mapping = aes(x=LOT.SQFT, y=TOTAL.VALUE))

# show the fitted curve AND the actual points
ggplot(data=roxbury.sample) + 
  geom_smooth(mapping = aes(x=LOT.SQFT, y=TOTAL.VALUE)) + 
  geom_point(mapping = aes(x=LOT.SQFT, y=TOTAL.VALUE))
