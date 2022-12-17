# FSharp-PPM-Image-Transformer
A library of F# functions to perform image operations on .ppm images.

# Grayscale

  Converts the image into grayscale and returns the
  resulting image as a list of lists. Pixels in grayscale
  have the same value for each of the Red, Green and Blue
  values in the RGB value. Conversion to grayscale is done
  by using a WEIGHTED AVERAGE calculation. A normal average
  (adding the three values and dividing by 3) is NOT the best,
  since the human eye does not perceive the brightness of
  red, green and blue the same. The human eye perceives
  green as brighter than red and it perceived red as brighter
  than blue. Research has shown that the following weighted
  values should be used when calculating grayscale.
  - the green value should account for 58.7% of the grayscale amount.
  - the red value should account for 29.9% of the grayscale amount.
  - the blue value should account for 11.4% of the grayscale amount.
 
  So if the RGB values were (25, 75, 250), the grayscale amount
  would be 80, (25 * 0.299 + 75 * 0.587 + 250 * 0.114 => 80)
  and then all three RGB values would become 80 or (80, 80, 80).
  We will use truncation to cast from the floating point result
  to the integer grayscale value.
 
  Returns: updated image.
  
# Threshold
 
  Thresholding increases image separation --- dark values
  become darker and light values become lighter. Given a
  threshold value in the range 0 < threshold < color depth,
  each RGB value is compared to see if it's > threshold.
  If so, that RGB value is replaced by the color depth;
  if not, that RGB value is replaced with 0.
 
  Example: if threshold is 100 and depth is 255, then given
  a pixel (80, 120, 160), the new pixel is (0, 255, 255).
 
  Returns: updated image.
  
# FlipHorizontal
 
  Flips an image so that what’s on the left is now on
  the right, and what’s on the right is now on the left.
  That is, the pixel that is on the far left end of the
  row ends up on the far right of the row, and the pixel
  on the far right ends up on the far left. This is
  repeated as you move inwards toward the row's center.
 
  Returns: updated image.
 
# RotateRight90:
 
  Rotates the image to the right 90 degrees.
 
  Returns: updated image.
 
 
