//
// F# image processing functions.
//
// A Seriese of F# functions to alter ppm images by recursively performing operations on each pixel of a file.
//
// Sean Kudrna, UIC, November 2022
//

namespace ImageLibrary

module Operations =

  //
  // Grayscale:
  //
  // Converts the image into grayscale and returns the 
  // resulting image as a list of lists. Pixels in grayscale
  // have the same value for each of the Red, Green and Blue
  // values in the RGB value.  Conversion to grayscale is done
  // by using a WEIGHTED AVERAGE calculation.  A normal average
  // (adding the three values and dividing by 3) is not the best,
  // since the human eye does not perceive the brightness of 
  // red, green and blue the same.  The human eye perceives 
  // green as brighter than red and it perceived red as brighter
  // than blue.  Research has shown that the following weighted
  // values should be used when calculating grayscale.
  //  - the green value should account for 58.7% of the grayscale.
  //  - the red value should account for   29.9% of the grayscale.
  //  - the blue value should account for  11.4% of the grayscale.
  //
  // So if the RGB values were (25, 75, 250), the grayscale amount 
  // would be 80, (25 * 0.299 + 75 * 0.587 + 250 * 0.114 => 80)
  // and then all three RGB values would become 80 or (80, 80, 80).
  // We will use truncation to cast from the floating point result 
  // to the integer grayscale value.
  //
  // Returns: updated image.
  //

  // Return a row of pixles converted to grayscale
  let rec tuple_traversed_grayscale (pixleRowList:(int*int*int) list) newList =
    match pixleRowList with
    | [] -> newList // Do nothing, return newImageList
    | (r,g,b)::rest -> 
                       let pixleMath = int((float r) * 0.299 + (float g) * 0.587 + (float b) * 0.114)
                       let newPixle = (pixleMath, pixleMath, pixleMath) // Set pixle = grayscale weighted average
                       tuple_traversed_grayscale rest (newPixle::newList) // Call tuple_traversed again with the rest of row and
                                                                      // add the updated pixle to newList

  // Convert every row to grayscale and return as list of row lists
  let rec list_traversed_grayscale (image:(int*int*int) list list) (width:int) (height:int) (newImageList:(int*int*int) list list) =
    match height with
    | 0 -> newImageList //0 -> newImageList // When h=0 do nothing, return newImageList
    | _ -> let pixleRowList = image.[height-1] // Get the row of pixles (sublist)
           let newImageRow = List.rev (tuple_traversed_grayscale pixleRowList []) // Now, traverse the tuple and update newImageList
           list_traversed_grayscale image width (height-1) (newImageRow::newImageList) // Call list_traversed again to repeat for 
                                                                                       //next row of pixles

  // Convert the photo to grayscale
  let rec Grayscale (width:int) 
                    (height:int) 
                    (depth:int) 
                    (image:(int*int*int) list list) = 
    list_traversed_grayscale image width height []


  //
  // Threshold
  //
  // Thresholding increases image separation --- dark values 
  // become darker and light values become lighter. Given a 
  // threshold value in the range 0 < threshold < color depth,
  // each RGB value is compared to see if it's > threshold.
  // If so, that RGB value is replaced by the color depth;
  // if not, that RGB value is replaced with 0. 
  //
  // Example: if threshold is 100 and depth is 255, then given 
  // a pixel (80, 120, 160), the new pixel is (0, 255, 255).
  //
  // Returns: updated image.
  //

  // Determine how to adjust the pixle value depending on threshold
  let thresh_conversion (depth:int) (threshold:int) (value:int) = 
    if value > threshold then
      depth
    else
      0

  // Return a row of pixles converted to threshold version
  let rec tuple_traversed_threshold (depth:int) (threshold:int) (pixleRowList:(int*int*int) list) newList =
    match pixleRowList with
    | [] -> newList // Do nothing, return newImageList
    | (r,g,b)::rest -> 
                       let red = thresh_conversion depth threshold r
                       let green = thresh_conversion depth threshold g
                       let blue = thresh_conversion depth threshold b

                       let newPixle = (red, green, blue)
                       tuple_traversed_threshold depth threshold rest (newPixle::newList) // Call tuple_traversed again with the rest of row and
                                                                                          // add the updated pixle to newList

  // Convert every row to threshold version and return as list of row lists
  let rec list_traversed_threshold (image:(int*int*int) list list) (depth:int) (threshold:int) (height:int) (newImageList:(int*int*int) list list) =
    match height with
    | 0 -> newImageList //0 -> newImageList // When h=0 do nothing, return newImageList
    | _ -> let pixleRowList = image.[height-1] // Get the row of pixles (sublist)
           let newImageRow = List.rev (tuple_traversed_threshold depth threshold pixleRowList []) // Now, traverse the tuple and update newImageList
           list_traversed_threshold image depth threshold (height-1) (newImageRow::newImageList)  // Call list_traversed again to repeat for 
                                                                                                  //next row of pixles
  // Convert the photo to threshold
  let rec Threshold (width:int) 
                    (height:int)
                    (depth:int)
                    (image:(int*int*int) list list)
                    (threshold:int) = 
    // for now, just return the image back, i.e. do nothing:
    list_traversed_threshold image depth threshold height []


  //
  // FlipHorizontal:
  //
  // Flips an image so that what’s on the left is now on 
  // the right, and what’s on the right is now on the left. 
  // That is, the pixel that is on the far left end of the
  // row ends up on the far right of the row, and the pixel
  // on the far right ends up on the far left. This is 
  // repeated as you move inwards toward the row's center.
  //
  // Returns: updated image.
  //
  
  // Convert every row to reversed version and return as list of row lists
  let rec list_traversed_horizontal (image:(int*int*int) list list) (width:int) (height:int) (newImageList:(int*int*int) list list) =
    match height with
    | 0 -> newImageList //0 -> newImageList // When h=0 do nothing, return newImageList
    | _ -> let pixleRowList = image.[height-1] // Get the row of pixles (sublist)
           let reversedRow = List.rev pixleRowList // Reverse the row list
           list_traversed_horizontal image width (height-1) (reversedRow::newImageList) // Call list_traversed again to repeat for 
                                                                                        //next row of pixles

  let rec FlipHorizontal (width:int)
                         (height:int)
                         (depth:int)
                         (image:(int*int*int) list list) = 
    // for now, just return the image back, i.e. do nothing:
    list_traversed_horizontal image width height []


  //
  // Edge Detection:
  //
  // Edge detection is an algorithm used in computer vision to help
  // distinguish different objects in a picture or to distinguish an
  // object in the foreground of the picture from the background.
  //
  // Edge Detection replaces each pixel in the original image with
  // a black pixel, (0, 0, 0), if the original pixel contains an 
  // "edge" in the original image.  If the original pixel does not
  // contain an edge, the pixel is replaced with a white pixel 
  // (255, 255, 255).
  //
  // An edge occurs when the color of pixel is "signigicantly different"
  // when compared to the color of two of its neighboring pixels. 
  // We only compares each pixel in the image with the 
  // pixel immediately to the right of it and with the pixel
  // immediately below it. If either pixel has a color difference
  // greater than a given threshold, then it is "significantly
  // different" and an edge occurs. Note that the right-most column
  // of pixels and the bottom-most column of pixels can not perform
  // this calculation so the final image contain one less column
  // and one less row than the original image.
  //
  // To calculate the "color difference" between two pixels, we
  // treat the each pixel as a point on a 3-dimensional grid and
  // we calculate the distance between the two points using the
  // 3-dimensional extension to the Pythagorean Theorem.
  // Distance between (x1, y1, z1) and (x2, y2, z2) is
  //  sqrt ( (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2 )
  //
  // The threshold amount will need to be given, which is an 
  // integer 0 < threshold < 255.  If the color distance between
  // the original pixel either of the two neighboring pixels 
  // is greater than the threshold amount, an edge occurs and 
  // a black pixel is put in the resulting image at the location
  // of the original pixel. 
  //
  // Returns: updated image.
  //
  // Return a row of pixles converted to grayscale
                        // pixel // pixel to right // pixel below
  let convertPixelValue (x1, y1, z1) (x2, y2, z2) (x3, y3, z3) (threshold:int) =
    let distance = (sqrt (((x1-x2)^2) + ((y1-y2)^2) + ((z1-z2)^2)))
    let distance2 = (sqrt (((x1-x3)^2) + ((y1-y3)^2) + ((z1-z3)^3)))
    if ((distance > threshold) || (distance2 > threshold)) then
      (255, 255, 255)
    else
      (0, 0, 0)
                                        

  let rec tuple_traversed_EdgeDetect (pixleRowList:(int*int*int) list) (rBelow, gBelow, bBelow) newList (threshold:int) =
    match pixelRowList with
    | [] -> newList // Do nothing, return newImageList
    | (r,g,b)::(r2,g2,b2)::rest -> 
                       let newPixel = convertPixelValue (r,g,b) (r2,g2,b2) (rBelow, gBelow, bBelow) threshold
                       tuple_traversed_EdgeDetect rest (newPixle::newList) rowBelow // Call tuple_traversed again with the rest of row and
                                                                                    // add the updated pixle to newList

  // Convert every row to grayscale and return as list of row lists
  let rec list_traversed_EdgeDetect (image:(int*int*int) list list) (width:int) (height:int) (threshold:int) (newImageList:(int*int*int) list list) =
    match height with
    | 1 -> newImageList //1 -> newImageList // When h=1 do nothing, return newImageList
    | _ -> let pixelRowList = image.[height-1] // Get the row of pixles (sublist)
           let pixelRowBelow = List.head (image.[height-2]) // Get the  row of pixles below 
           let newImageRow = List.rev (tuple_traversed_EdgeDetect pixelRowList pixelRowBelow [] threshold) // Now, traverse the tuple and update newImageList
           list_traversed_EdgeDetect image width (height-1) (newImageRow::newImageList) // Call list_traversed again to repeat for 
                                                                                        //next row of pixles

  //
  // RotateRight90:
  //
  // Rotates the image to the right 90 degrees.
  //
  // Returns: updated image.
  //

  // Get head
  let getHead L:(int*int*int) =
    match L with
    | [] -> (0,0,0)
    | hd::tl -> hd

  // Get tail
  let getTail L =
    match L with
    | []  -> []
    | hd::tl -> tl

  // Transform
  let rec col_to_row (width:int) (image:(int*int*int) list list) (newImageList:(int*int*int) list list) =
    match width with
    | 0 -> newImageList
    | _ -> let newRow = List.rev (List.map (getHead) image)
           let newRowList = List.map (getTail) image
           col_to_row (width-1) newRowList (newRow::newImageList) 

  let rec RotateRight90 (width:int) (height:int) (depth:int) (image:(int*int*int) list list) = 
    List.rev (col_to_row width image [])
