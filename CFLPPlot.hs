module CFLPPlot where

import CFLP

import Data.Array
import Data.Function (on)
import Data.Functor
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import Data.Traversable

--import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy

normalize xs = map (\x -> x / (maximum xs)) xs

-- Plotting of CFLP
plotCFLP :: CFLP -> String -> IO ()
plotCFLP cflp name = do
  toFile def name $ do
    layout_title .= cflpName cflp
    layout_background .= solidFillStyle (opaque white)
    layout_foreground .= (opaque black)
    layout_left_axis_visibility . axis_show_ticks .= False

    -- the max radius in each area spot states the width of the
    -- largest value. so if we set the max radius of all areas to
    -- the same value, the largest circles of all colors will be
    -- the same size even if they have different values.

    -- so we set the max radius to the same value relative to the
    -- maximum value of each plot. then the circles of all plots are
    -- comparable. the sqrt'ing is necessary due to the internals
    -- of area_spots.

    -- we want the largest radius of a facility to be 20

    let facilitySpots = spotDataFromFacilities $ facilities cflp
        facilityRadii = map (\(_, _, x) -> x) facilitySpots
        maxFacilityRadius = maximum facilityRadii
    plot $ liftEC $ do
      area_spots_title .= "facilities"
      area_spots_fillcolour .= blue
      area_spots_max_radius .= sqrt (maxFacilityRadius / maxFacilityRadius) * 20.0
      area_spots_values .= facilitySpots

    let openFacilitySpots = spotDataFromFacilities
                            $ filter (\i -> y i > 0.9) -- use epsilon due to numerical errors
                            $ facilities cflp
        openFacilityRadii = map (\(_, _, x) -> x) openFacilitySpots
        openFacilityFractions = fractionSent (distances cflp) (clients cflp)
                                $ filter (\i -> y i > 0.9) -- use epsilon due to numerical errors
                                $ facilities cflp
        maxOpenFacilityRadius = maximum $ zipWith (*) openFacilityRadii openFacilityFractions

        openFacilitySpots' = zipWith (\(x,y,r) f -> (x,y,r*f))
          openFacilitySpots openFacilityFractions

    plot $ liftEC $ do
      area_spots_title .= "open facilities"
      area_spots_fillcolour .= blue
      area_spots_opacity .= 1.0
      area_spots_max_radius .= sqrt (maxOpenFacilityRadius / maxFacilityRadius) * 20.0
      area_spots_values .= openFacilitySpots'

    let clientSpots = spotDataFromClients $ clients cflp
        maxClientRadius = maximum $ map (\(_, _, x) -> x) clientSpots
    plot $ liftEC $ do
      area_spots_title .= "clients"
      area_spots_fillcolour .= green
      area_spots_max_radius .= sqrt (maxClientRadius / maxFacilityRadius) * 20.0
      area_spots_values .= clientSpots

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
      setColors [opaque purple]
      plot $ line "" $ [map fromPosition [f, c]]


fromPosition :: Position -> (Double, Double)
fromPosition (Position x y) = (x, y)

spotDataFromFacilities :: Facilities -> [(Double, Double, Double)]
spotDataFromFacilities fs = map (\(Facility _ _ u _ (Position xPos yPos)) -> (xPos, yPos, u)) fs

spotDataFromClients :: Clients -> [(Double, Double, Double)]
spotDataFromClients cs = map (\(Client _ d (Position xPos yPos)) -> (xPos, yPos, d)) cs

fractionSent :: Distances -> Clients -> Facilities -> [Double]
fractionSent ds cs fs = map (\f -> (fracOutFlow f) / (u f)) fs
  where fracOutFlow f = traceMsgId "fof" $ sum [x dist * d | ((fId, cId), dist) <- assocs ds, fId == facilityId f, let Just d = getDemandById cs cId]


readBench :: String -> String -> IO ()
readBench fileName plotName = do
  fileContent <- readFile fileName
  let benchData = read fileContent
  plotBench benchData plotName

plotBench :: [(Int, Int, Int, Double, Double, Double)] -> String -> IO ()
plotBench benchData name = do
 toFile def name $ do
   layout_title .= "Benchmark"
   layout_background .= solidFillStyle (opaque white)
   layout_foreground .= (opaque black)
   layout_left_axis_visibility . axis_show_ticks .= False

   plot $ line "exact" $ [promoteN $ aggregateWithMean (map exactTime benchData)]
   plot $ line "approx" $ [promoteN $ aggregateWithMean (map approxTime benchData)]

exactTime :: (Int, Int, Int, Double, Double, Double) -> ((Int, Int), Double)
exactTime  (_, n, m, t, _, _) = ((n, m), t)

approxTime :: (Int, Int, Int, Double, Double, Double) -> ((Int, Int), Double)
approxTime (_, n, m, _, t, _) = ((n, m), t)


mean :: [Double] -> Double
mean list = (sum list) / (fromIntegral $ length list)

aggregateWithMean :: Ord t => [(t, Double)] -> [(t, Double)]
aggregateWithMean list = map meanOnSnd $ map factorOut $ groupWith fst list

promoteN :: [((Int, Int), Double)] -> [(Double, Double)]
promoteN list = map (\ ((n,m), y) -> (fromIntegral n, y)) list

meanOnSnd :: (t, [Double]) -> (t, Double)
meanOnSnd (a, xs) = (a, mean xs)

factorOut :: [(a, b)] -> (a, [b])
factorOut xs = (fst (xs !! 0), map snd xs)

groupWith :: Ord a => (b -> a) -> [b] -> [[b]]
groupWith f list = groupBy ((==) `on` f) $ sortBy (comparing f) list
