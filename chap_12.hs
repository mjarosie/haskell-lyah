import Data.List (break)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

freeTree :: Tree Char
freeTree = 
    Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' Empty Empty)
                (Node 'T' Empty Empty)
            )
            (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)
            )
        )
        (Node 'L'
            (Node 'W'
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty)
            )
            (Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)
            )
        )

data Direction = L | R deriving (Show)
type Directions = [Direction]

changeToP :: Directions -> Tree Char -> Tree Char
changeToP (L:ds) (Node x l r) = Node x (changeToP ds l) r
changeToP (R:ds) (Node x l r) = Node x l (changeToP ds r)
changeToP [] (Node x l r) = Node 'P' l r

elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)
type Breadcrumbs a = [Crumb a]
type Zipper a = (Tree a, Breadcrumbs a)

x -: f = f x

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Node x l r, bs) = Just (l, (LeftCrumb x r):bs)
goLeft (Empty, _) = Nothing

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Node x l r, bs) = Just (r, (RightCrumb x l):bs)
goRight (Empty, _) = Nothing

goUp :: Zipper a -> Maybe (Zipper a)
goUp (t, LeftCrumb x r:bs) = Just (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = Just (Node x l t, bs)
goUp (_, []) = Nothing

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (Empty, bs) = (Empty, bs)

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

topMost :: Zipper a -> Zipper a
topMost z = case (goUp z) of
    Just up -> topMost up
    Nothing -> z

data List a = EmptyList | Cons a (List a) deriving (Show, Read, Eq, Ord)
type ListZipper a = ([a], [a])

goForward :: ListZipper a -> ListZipper a
goForward (x:xs, bs) = (xs, x:bs)

goBack :: ListZipper a -> ListZipper a
goBack (xs, b:bs) = (b:xs, bs)

type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

myDisk :: FSItem  
myDisk = 
	Folder "root"   
		[ File "goat_yelling_like_man.wmv" "baaaaaa"  
		, File "pope_time.avi" "god bless"  
		, Folder "pics"  
			[ File "ape_throwing_up.jpg" "bleargh"  
			, File "watermelon_smash.gif" "smash!!"  
			, File "skull_man(scary).bmp" "Yikes!"  
			]  
		, File "dijon_poupon.doc" "best mustard"  
		, Folder "programs"  
			[ File "fartwizard.exe" "10gotofart"  
			, File "owl_bandit.dmg" "mov eax, h00t"  
			, File "not_a_virus.exe" "really not a virus"  
			, Folder "source code"  
				[ File "best_hs_prog.hs" "main = print (fix error)"  
				, File "random.hs" "main = print 4"  
				]  
			]  
		]

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder currFolderName items, bs) =
	let (before, item:after) = break (nameIs name) items
	in (item, FSCrumb currFolderName before after:bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (File fileName _) = name == fileName
nameIs name (Folder folderName _) = name == folderName

fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (File name d, bs) = (File newName d, bs)
fsRename newName (Folder name items, bs) = (Folder newName items, bs)

fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Folder name items, bs) = (Folder name (item:items), bs)
