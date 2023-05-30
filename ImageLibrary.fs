//
// F# image processing functions.
//
// Library that Includes functions for threshold rotating grayscaling and flipping images
//
// Robert Broniarczyk UIC Fall 2022
//

namespace ImageLibrary

module Operations =
  //
  // all functions must be indented
  //

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
  let private gray2 (image:(int*int*int)) = 
    let (a,b,c) = image
    let x  = (float a) * 0.299 + (float b) * 0.587 + (float c) * 0.114
    let z = ((int)x,(int)x,(int)x)
    z
  // let rec gray (image:(int*int*int) list) =
  //   match image with
  //   |hd::[] -> gray2 hd
  //   |hd::tl -> gray2 hd::gray tl
  //   |[] -> []
  let rec Grayscale (width:int) 
                    (height:int) 
                    (depth:int) 
                    (image:(int*int*int) list list) = 
    // for now, just return the image back, i.e. do nothing:
    match image with 
    |hd::tl -> List.map gray2 hd::Grayscale width height depth tl
    |[] -> []
    


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
  let private calThresh (depth:int) (threshold:int) (a:int) =
    if a > threshold then
      depth
    else
      0
  let private ThreshTurn (depth:int) (threshold:int) (image:(int*int*int)) = 
    let (x,b,c) = image
    let a1 = calThresh depth threshold x
    let b1 = calThresh depth threshold b
    let c1 = calThresh depth threshold c
    (a1,b1,c1)
  let rec Threshold (width:int) 
                    (height:int)
                    (depth:int)
                    (image:(int*int*int) list list)
                    (threshold:int) = 
    // for now, just return the image back, i.e. do nothing:
    match image with
    |hd::tl -> List.map (ThreshTurn depth threshold) hd::Threshold width height depth tl threshold
    |[] -> []


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
  let rec FlipHorizontal (width:int)
                         (height:int)
                         (depth:int)
                         (image:(int*int*int) list list) = 
    // for now, just return the image back, i.e. do nothing:
    match image with
    |hd::tl -> List.rev hd::FlipHorizontal width height depth tl
    |[] -> []


 


  //
  // RotateRight90:
  //
  // Rotates the image to the right 90 degrees.
  //
  // Returns: updated image.
  //
  let rec RR (image:(int*int*int) list list) =
    match image with
    |hd::tl -> List.rev hd::RR tl
    |[] -> []
  let rec RotateRight90 (width:int)
                        (height:int)
                        (depth:int)
                        (image:(int*int*int) list list) = 
    // for now, just return the image back, i.e. do nothing:
    let Timage = List.transpose image
    RR Timage
    

