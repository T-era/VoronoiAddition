module VPoint(Point, Vector(Vector), Directive, isPara, isSameDirection, distance2, isBetween, reverseVector, (-|>)) where

-- 方向を表す型。
-- isPara a b が False なら、isSameDirection a b は必ず False です。
-- a, b が逆方向の場合、isPara a b = True, isSameDirection a b = False です。
class Directive d where
	--　平行かどうかを判定します。
	isPara :: d -> d -> Bool
	--　同じ方向かどうかを判定します。
	isSameDirection :: d -> d -> Bool

--　(平面上の)点を表します。(x, y)
type Point d = (d, d)

--　方向を表します。(dx, dy)
newtype Vector d = Vector (d, d) deriving (Show)
instance (Eq d) => Eq (Vector d) where
	(==) (Vector (dx1, dy1)) (Vector (dx2, dy2))= dx1 == dx2 && dy1 == dy2
instance (Num d, Eq d) => Directive (Vector d) where
	isPara (Vector (dx1, dy1)) (Vector (dx2, dy2)) = (dx1 * dy2 == dx2 * dy1) -- TODO 0.00001とかと差分比較？
	isSameDirection v1@(Vector (dx1, dy1)) v2@(Vector (dx2, dy2))
		= isPara v1 v2 && signum dx1 == signum dx2 && signum dy1 == signum dy2

-- 指定された2点の距離の二乗を返します。
-- 距離をとる場合、ここからルートを取得してください。
distance2 :: (Num d) => (Point d) -> (Point d) -> d
distance2 (x1, y1) (x2, y2) = dx^2 + dy^2
	where
		dx = x2 - x1
		dy = y2 - y1

-- 3点を引数に受け、ある点(第3引数)が残り2点の[間]にあるかどうかを判定します。
-- [間]とは、2点を対角に持つ矩形領域内です。領域の境界線上は内側と判定します。
-- 
-- 特に3点が一直線上にある場合は、この関数は線分内に第3点がいるかどうかの判定になります。
isBetween :: (Ord d) => Point d -> Point d -> Point d -> Bool
isBetween (x1, y1) (x2, y2) (x, y) = isBetween' x1 x2 x && isBetween' y1 y2 y
isBetween' a b c = (a <= c && c <= b) || (b <= c && c <= a)

-- ある点を起点にして、指定されたベクトル分移動した点を返します。
(-|>) :: (Num d) => Point d -> Vector d -> Point d
(-|>) (x, y) (Vector (dx, dy)) = (x + dx, y + dy)

-- ベクトルを反転します。
reverseVector :: (Num d) => Vector d -> Vector d
reverseVector (Vector (dx, dy)) = Vector (-dx, -dy)
