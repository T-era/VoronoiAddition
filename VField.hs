module VField(VField, emptyField, addPoint, VLine) where

import VLine
import VPoint(Point, Vector(Vector), isPara, isSameDirection, distance2)
import qualified Data.Map as Map

-- フィールド。[すべての母点]と、その母店が持つ[ボロノイ線のリスト]を辞書として保持します。
type VField d = Map.Map (Point d) [VLine d]
-- (ある母点からみた)ボロノイ線です。[線]と[どの母点との間にある線なのか]をタプルで保持します。
type VLine d = (Point d, Line d)

emptyField = Map.empty

-- フィールドに新たな母点を加えます。
-- 指定された母点が、[すでにある母点のどれかと近すぎる]場合、関数は何も行いません(idとしてふるまいます)。
addPoint :: (Fractional d, Ord d, Eq d) => Point d -> VField d -> VField d
addPoint newPoint field
	| Map.null field              = Map.fromList [(newPoint, [])]
	| isDuplecated newPoint field = field
	| otherwise                   = fieldNext
	where
		fieldNext = relateChain brushUpField nearest field
		(Just nearest) = foldr (nearerTo newPoint) Nothing $ Map.keys field
		brushUpField buddy field' = (nextField, related)
			where
				bisectorLine = bisector newPoint buddy
				(nextNewPointLines, _) = brushUpVLines newPoint (buddy, bisectorLine) (maybe [] id $ Map.lookup newPoint field')
				(nextBudsPointLines, related) = brushUpVLines buddy (newPoint, bisectorLine) (unwrapJust $ Map.lookup buddy field')
				modifiedB = Map.update (\_ -> Just nextBudsPointLines) buddy field'
				nextField = Map.insert newPoint nextNewPointLines modifiedB

-- 指定された点が[すでにある母点のどれかと近すぎる]かどうかを判定します。
isDuplecated :: (Fractional d, Ord d, Eq d) => Point d -> VField d -> Bool
isDuplecated p f = Map.lookup p f /= Nothing

-- 2点を受け、より近い方の点を返します。
nearerTo :: (Num d, Ord d) => Point d -> (Point d -> Maybe (Point d) -> Maybe (Point d))
nearerTo p = nearer
	where
		nearer p1 Nothing = Just p1
		nearer p1 (Just p2)
			| distance2 p p1 < distance2 p p2 = Just p1
			| otherwise                       = Just p2

unwrapJust (Just a) = a

-- タスクを管理して、全て処理します。
-- relateChain task firstArg data
--  task: こなすべき処理。戻り値は(処理結果, 派生して処理するべき引数リスト)というタプルである必要があります。
--  firstArg: task の最初の引数
--  data: taskが処理するデータの初期状態
relateChain :: (Eq a) => (a -> target -> (target, [a])) -> a -> target -> target
relateChain f a t = fst $ relateChainIn [a] t []
	where
		relateChainIn [] target doneList = (target, doneList)
		relateChainIn (arg:ls) target doneList
			| arg `elem` doneList = relateChainIn ls target doneList
			| otherwise           = relateChainIn ls thisResult thisResultNL
			where
				(tempResult, relation) = f arg target
				(thisResult, thisResultNL) = relateChainIn relation tempResult (arg:doneList)

-- VLineリストを取捨選択(もしくはchop)します。
-- brushUpVLines p vl [(bp,lines)]
--  p: the owner of VLine list.
--  vl: new VLine
brushUpVLines :: (Fractional d, Ord d, Eq d) => Point d -> VLine d -> [VLine d] -> ([VLine d], [Point d])
brushUpVLines _ bisector [] = ([bisector], []) -- start
brushUpVLines owner (bp, cr) (vl@(buddy, l):ls)
	| mbChopped == Nothing = brushUpVLines owner (bp, cr) ls
	| modified             = ((buddy, chopped):lines, buddy:neighbors)
	| otherwise            = (vl:lines, neighbors)
	where
		(mbChopped, modified) = chop l cr owner
		Just chopped = mbChopped
		(mbChoppedCr, _) = chop cr l owner
		choppedCr = maybe cr id mbChoppedCr
		(lines, neighbors) = brushUpVLines owner (bp, choppedCr) ls
