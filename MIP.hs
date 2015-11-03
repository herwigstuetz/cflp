-- | Mixed Integer Program

module MIP where

import           Data.List   (find, group, intercalate, sort)
import           Data.Maybe  (fromMaybe)
import qualified Data.Vector as V

import           Foreign.C

import           CPLEX
import           CPLEX.Param

data MIP = MIP { sense  :: ObjSense
               , obj    :: V.Vector Double
               , rhs    :: V.Vector Sense
               , amat   :: [(Row, Col, Double)]
               , bnd    :: V.Vector (Maybe Double, Maybe Double)
               , ctypes :: V.Vector Type
               } deriving (Show)


class ToMip a where
  toMip :: a -> MIP

class FromMip a where
  fromMip :: MIP -> a

class (ToMip a, FromMip a) => MipSolvable a where
  solve :: a -> a
  solve = fromMip . toMip

cpx_ON :: CInt
cpx_ON  =  1
cpx_OFF :: Integer
cpx_OFF =  0

runLp :: MIP -> CpxEnv -> CpxLp -> IO (Maybe String)
runLp (MIP sense obj rhs amat bnd _) cpxEnv cpxLp =
  copyLp cpxEnv cpxLp sense obj rhs amat bnd

runMip :: MIP -> CpxEnv -> CpxLp -> IO (Maybe String)
runMip (MIP sense obj rhs amat bnd ctypes) cpxEnv cpxLp =
  copyMip cpxEnv cpxLp sense obj rhs amat bnd ctypes


showObjSense :: ObjSense -> String
showObjSense CPX_MAX = "max"
showObjSense CPX_MIN = "min"

showObj :: V.Vector Double -> String
showObj = show

showAMat :: [(Row, Col, Double)] -> V.Vector Sense -> String
showAMat amat rhs = intercalate "\n" [unwords ([show $ a i j | j <- [0..n+n*m-1]] ++ [show $ rhs V.! i]) | i <- [0..n+n*m+m-1]]
  where el (_, _, d) = d
        a :: Int -> Int -> Double
        a i j = fromMaybe 0.0 $ el <$> find (\(Row k, Col l, _) -> i == k && j == l) amat
        fs = map head (group . sort $ map (\(Row i, _, _) -> i) amat)
        cs = map head (group . sort $ map (\(_, Col j, _) -> j) amat)
        n = length fs
        m = length cs

showLP (MIP sense obj rhs amat bnd _) = showObjSense sense
                                        ++ showObj obj
                                        ++ "\n"
                                        ++ showAMat amat rhs

solMip :: String -> MIP -> IO (CpxSolution)
solMip name p = withEnv $ \env -> do
  setIntParam env CPX_PARAM_DATACHECK cpx_ON
  setIntParam env CPX_PARAM_SCRIND cpx_ON
  withLp env name $ \mip -> do
    putStrLn $ "Solving " ++ name ++ ":"

    statusMip <- runMip p env mip

    case statusMip of
      Nothing -> return ()
      Just msg -> error $ "CPXcopylp error: " ++ msg
    putStrLn $ "Solving " ++ name ++ ":"

    -- Solve problem
    statusOpt <- mipopt env mip
    case statusOpt of
      Nothing -> return ()
      Just msg -> error $ "CPXlpopt error: " ++ msg
    putStrLn $ "Solving " ++ name ++ ":"

    -- Retrieve solution
    statusSol <- getMIPSolution env mip
    case statusSol of Left msg -> error msg
                      Right sol -> return sol

solLp :: String -> MIP -> IO (CpxSolution)
solLp name p = withEnv $ \env -> do
  setIntParam env CPX_PARAM_DATACHECK cpx_ON
  setIntParam env CPX_PARAM_SCRIND cpx_ON
  withLp env name $ \lp -> do
    putStrLn $ "Solving " ++ name ++ ":"

    statusLp <- runLp p env lp

    case statusLp of
      Nothing -> return ()
      Just msg -> error $ "CPXcopylp error: " ++ msg
    putStrLn $ "Solving " ++ name ++ ":"

    -- Solve problem
    statusOpt <- lpopt env lp
    case statusOpt of
      Nothing -> return ()
      Just msg -> error $ "CPXlpopt error: " ++ msg
    putStrLn $ "Solving " ++ name ++ ":"

    -- Retrieve solution
    statusSol <- getSolution env lp
    case statusSol of Left msg -> error msg
                      Right sol -> return sol
