-- | Assignment Problem

module AP where

import           Data.Function (on)
import           Data.List     (sortBy)
import qualified Data.Vector   as V

import           CFLP
import           CPLEX
import           MIP

mcfXIdx n m i j | (0 <= i) && (i < n) && (0 <= j) && (j < m) =      j*n + i


-- ERROR: mixing of i=0..n-1 and i in facilityIds (!= 0..n-1)
mcfObj :: CFLP -> V.Vector Double
mcfObj cflp = V.fromList $ map snd $ sortBy (compare `on` fst)
                                            [ (mcfXIdx n m i j, dj * cij) | (i, fId) <- zip [0..n-1] fIds
                                                                          , (j, cId) <- zip [0..m-1] cIds
                                                                          , let Just cij = getDistanceById ds fId cId
                                                                          , let Just dj = getDemandById cs cId]
  where n = length $ facilities cflp
        m = length $ clients cflp
        cs = clients cflp
        ds = distances cflp
        fIds = map facilityId (facilities cflp)
        cIds = map clientId (clients cflp)

mcfRhs :: CFLP -> V.Vector Sense
mcfRhs cflp = V.fromList $
                replicate m (G 1.0) ++ -- \sum_i x_{ij} \geq 1 \forall j
                replicate (n * m) (L 1.0) ++ -- x_{ij} \leq 1 \forall i,j
                map (L . u) (facilities cflp) -- \sum_j d_j x_{ij} \leq u_i \forall i, this needs that ids are ordered
  where n = length (facilities cflp)
        m = length (clients cflp)

mcfConstraints :: CFLP -> [(Row, Col, Double)]
mcfConstraints cflp = fromConstraints $
                        [[(mcfXIdx n m i j, 1.0) | (i, fId) <- zip [0..] fIds] | (j, cId) <- zip [0..] cIds] ++
                        [[(mcfXIdx n m i j, 1.0)] | i <- [0..n-1], j <- [0..m-1]] ++
                        [[(mcfXIdx n m i j, dj) | (j, cId) <- zip [0..m-1] cIds, let Just dj = getDemandById cs cId] | i <- [0..n-1]]
  where cIds = map clientId (clients cflp)
        fIds = map facilityId (facilities cflp)
        cs = clients cflp
        n = length fIds
        m = length cIds

mcfBnds :: CFLP -> V.Vector (Maybe Double, Maybe Double)
mcfBnds cflp = V.fromList $ replicate (n * m) (Just 0.0, Nothing)
  where n = length (facilities cflp)
        m = length (clients cflp)

mcfTypes :: CFLP -> V.Vector Type
mcfTypes cflp = V.fromList $ replicate (n * m) CPX_CONTINUOUS
  where n = length (facilities cflp)
        m = length (clients cflp)

fromOpenedCFLP :: CFLP -> MIP
fromOpenedCFLP cflp = let s = CPX_MIN
                          o = mcfObj cflp
                          r = mcfRhs cflp
                          a = mcfConstraints cflp
                          b = mcfBnds cflp
                          t = mcfTypes cflp
                      in
                        MIP s o r a b t
