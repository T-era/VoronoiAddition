module VLine(Line(Line, SLine, CLine), bisector, cross, isSameSide, chop) where

import VPoint

-- 線を表します。
-- 直線、半直線、線分を表します。
data Line d
	-- 直線。pを通り傾きはv。
	= Line { p::Point d, v::Vector d }
	-- 半直線。p1を一方の終端とし、direction方向に無限に伸びる。
	| SLine { end::Point d, direction::Vector d }
	-- 線分。p1, p2を両端とする。
	| CLine { p1::Point d, p2::Point d } deriving (Show)
instance (Fractional d, Eq d) => Eq (Line d) where
	(==) (Line p v1) l2@(Line _ v2)
		= (v1 == v2 || v1 == reverseVector v2) && (toF l2 p == 0)
	(==) (SLine p1 v1) (SLine p2 v2)
		= p1 == p2 && v1 == v2
	(==) (CLine p11 p12) (CLine p21 p22)
		= (p11 == p21 && p12 == p22) || (p11 == p22 && p12 == p21)
	(==) _ _
		= False
instance (Num d, Eq d) => Directive (Line d) where
	isPara l1 l2 = isPara (toVector l1) (toVector l2)
	isSameDirection l1 l2 = isSameDirection (toVector l1) (toVector l2)

-- 線から傾きを抽出します。
toVector :: (Num d) => Line d -> Vector d
toVector (Line _ v) = v
toVector (CLine (x1, y1) (x2, y2)) = Vector (x2-x1, y2-y1)
toVector (SLine _ v) = v 

-- 2点の垂直二等分線を返します。
-- 指定された2点の座標が一致する場合、エラーになります。
bisector :: (Fractional d, Eq d) => Point d -> Point d -> Line d
bisector (x1, y1) (x2, y2)
	| x1 == x2 && y1 == y2 = error "同一の点"
	| otherwise = Line center v
	where
		center = ((x1 + x2) / 2, (y1 + y2) / 2)
		v = Vector (y2 - y1, x1 - x2)

--　与えられた線を直線に変換します。
-- 与えられた線から(もしあれば)終端を取っ払って無限に伸ばします。
imageInf :: (Num d) => Line d -> Line d
imageInf (CLine p@(x1, y1) (x2, y2)) = (Line p (Vector (x2 - x1, y2 - y1)))
imageInf (SLine p v) = Line p v
imageInf l = l

-- 線を受け取り、残り2点がその線の同じ側にあるかどうかを判定します。
-- 線が有限の場合でも、判定では(指定された線を無限に伸ばした)直線とみなして同じ側かどうかを判定します。
-- 点が線上にある場合は特に、二点とも線上ならTrue, どちらか一方だけが線上ならFalseになります。
isSameSide :: (Num d, Eq d) => Line d -> Point d -> Point d -> Bool
isSameSide l p1 p2 = side l p1 == side l p2

-- 線を受け取り、与えられた点が線のどちら側にあるのかを表す数値(1, 0, -1のどれか)を返します。
side :: (Num d) => Line d -> (Point d -> d)
side line p = signum$toF line p

-- 線を受け取り、関数に置き換えます。
-- 線l が、f(x, y) = 0で表現できる場合、戻り値となる関数はf(x, y)です。
toF :: (Num d) => Line d -> (Point d -> d)
toF = toF'.imageInf
	where
		toF' (Line (x, y) (Vector (dx, dy))) (argX, argY) = (argX - x) * dy - (argY - y) * dx

-- 2線の交点を返します。ただし、与えられた線は無限に伸ばした直線として交点を求めます。
-- 2線が平行の場合、Nothingとなります。
imaginaryCross :: (Fractional d, Eq d) => Line d -> Line d -> Maybe (Point d)
imaginaryCross (Line (x1, y1) v1@(Vector (dx1, dy1))) (Line (x2, y2) v2@(Vector (dx2, dy2)))
	| isPara v1 v2 = Nothing
	| otherwise    = Just (x, y)
	where
		x = (-dx2 * hoge1 + dx1 * hoge2) / denomi
		y = (-dy2 * hoge1 + dy1 * hoge2) / denomi
		denomi = dx1 * dy2 - dx2 * dy1
		hoge1 = dy1 * x1 - dx1 * y1
		hoge2 = dy2 * x2 - dx2 * y2
imaginaryCross l1 l2 = imaginaryCross (imageInf l1) (imageInf l2)

-- 2線の交点を求めます。 imaginaryCrossで求めた仮想的な交点を、さらに両線の範囲内にあるかどうかを確かめます。
-- 2線が平行の場合と交点が線の範囲外の場合、Nothingを返します。
cross :: (Fractional d, Ord d, Eq d) => Line d -> Line d -> Maybe (Point d)
cross l1 l2
	| isOut l1 imageCross || isOut l2 imageCross = Nothing
	| otherwise                                  = imageCross
	where
		imageCross = imaginaryCross l1 l2
		isOut _ Nothing  = True
		isOut l (Just p) = not $ isIn l p

-- 線を受け取り、点がその線の[間]にあるかどうかを確かめます。
-- [間]とは、厳密には線を対角線とする矩形領域内かどうかの判定です。
-- 点が線上にあるかどうかを判定するためには、(sideで0になるかどうかとか)別途の判定と絡めて確かめる必要があります。
isIn :: (Num d, Ord d) => Line d -> Point d -> Bool
isIn (Line _ _) _ = True
isIn (CLine p1 p2) p = isBetween p1 p2 p
isIn (SLine (x, y) (Vector (dx, dy))) (px, py)
	= (isSameSign dx (px-x) && isSameSign dy (py-y)) || (x == px && y == py)
	where isSameSign a b = signum a == signum b

-- 線を受け取り、その線上の1点を返します。
-- 線上のどの1点になるかは特に制約しません。
takePoint :: Line d -> Point d
takePoint (CLine p _) = p
takePoint (SLine p _) = p
takePoint (Line p _) = p

-- chop target breakLine side
-- target を与えられた線(breakLine)で切り分けます。
-- 戻り値は、切り分けられた線のうち、breakLineを挟んでSide側のものです。
-- もう一つの戻り値は、chopがtargetに作用したかどうかの真偽値です。(線を掠めるbreakLineを指定された場合、戻り値はtargetと同じ線ですがtrueになる場合があります。)
-- side がbreakLine上にある場合の挙動は未定。
chop :: (Fractional d, Ord d, Eq d) => Line d -> Line d -> Point d -> (Maybe (Line d), Bool)
chop target breakLine side
	| isPara target breakLine = nearside
	| otherwise               = chopAt $ cross target breakLine
	where
		isHereSide = isSameSide breakLine side
		targetIsSameSide = isHereSide $ takePoint target
		nearside
			| targetIsSameSide = (Just target, False)
			| otherwise        = (Nothing, False)
		chopAt Nothing
			| targetIsSameSide = (Just target, False)
			| otherwise        = (Nothing, False)
		chopAt (Just crossPoint) = newLine target
			where
				newLine (Line _ v)
					| isHereSide (crossPoint -|> v) = (Just $ SLine crossPoint v, True)
					| otherwise                     = (Just $ SLine crossPoint $ reverseVector v, True)
				newLine (SLine p v)
					| p == crossPoint               = if isHereSide (crossPoint -|> v)
														then (Just $ SLine p v, False)
														else (Nothing, True)
					| isHereSide p                  = (Just $ CLine p crossPoint, True)
					| isHereSide (crossPoint -|> v) = (Just $ SLine crossPoint v, True)
					| otherwise                     = (Just $ CLine p crossPoint, True)
--					| otherwise    = SLine crossPoint v
				newLine (CLine p1 p2)
					| isHereSide p1 && isHereSide p2 = (Just $ CLine p1 p2, True)
					| isHereSide p1 = (Just $ CLine p1 crossPoint, True)
					| isHereSide p2 = (Just $ CLine p2 crossPoint, True)
					| otherwise     = (Nothing, True)
