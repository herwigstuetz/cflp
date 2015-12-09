module CFLPPlot where

import CFLP

import Data.Array
import Data.Functor
import Data.Traversable

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

-- Plotting of CFLP
plotCFLP :: CFLP -> String -> IO ()
plotCFLP cflp name = do
  toFile def name $ do
    layout_title .= "CFLP"
    layout_background .= solidFillStyle (opaque white)
    layout_foreground .= (opaque black)
    layout_left_axis_visibility . axis_show_ticks .= False

    plot $ liftEC $ do
      area_spots_title .= "facilities"
      area_spots_fillcolour .= blue
      area_spots_max_radius .= 20
      area_spots_values .= (spotDataFromFacilities $ facilities cflp)

    plot $ liftEC $ do
      area_spots_title .= "clients"
      area_spots_fillcolour .= green
      area_spots_max_radius .= 20
      area_spots_values .= (spotDataFromClients $ clients cflp)

    plot $ liftEC $ do
      area_spots_title .= "facilities"
      area_spots_fillcolour .= blue
      area_spots_opacity .= 1.0
      area_spots_max_radius .= 20
      area_spots_values .= (spotDataFromFacilities $ filter (\i -> y i == 1.0) $ facilities cflp)

    let fs  = facilities cflp
        cs  = clients cflp
        ds  = elems $ distances cflp
        ds' = [ (f, c, x)
               | Distance i j _ x <- ds
               , let Just f = findFacility fs i
               , let Just c = findClient cs j
               , x > 0.0]
        ds'' = map (\(f, c, x) -> (facilityPos f, clientPos c, x)) ds'


    void $ forM ds'' $ \ (f, c, x) -> do
      setColors [opaque red]
      plot $ line "lines" $ [map fromPosition [f, c]]


fromPosition :: Position -> (Double, Double)
fromPosition (Position x y) = (x, y)

spotDataFromFacilities :: Facilities -> [(Double, Double, Double)]
spotDataFromFacilities fs = map (\(Facility _ _ u _ (Position xPos yPos)) -> (xPos, yPos, u)) fs

spotDataFromClients :: Clients -> [(Double, Double, Double)]
spotDataFromClients cs = map (\(Client _ d (Position xPos yPos)) -> (xPos, yPos, d)) cs
