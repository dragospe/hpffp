module BinaryTree where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf -- Insert into empty tree, or at a terminal leaf
insert' b (Node left a right) -- Insert into tree rooted at Node
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)


-- Exercise: write a mapTree function
--
-- Implment this, given the below:
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right) 

-- Sample trees
testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf) 

-- Acceptance test for mapTree
mapOkay = if mapTree (+1) testTree' == mapExpected
          then print "yup okay!"
          else error "test failed!"

-- Functions converting the binary tree to lists
--
preorder :: BinaryTree a -> [a]
preorder t = go t []
  where go :: BinaryTree a -> [a] -> [a]
        go Leaf l = l
        go (Node left a right) l' = [a] ++ go left l' ++ go right l'

inorder :: BinaryTree a -> [a]
inorder t = go t []
  where go :: BinaryTree a -> [a] -> [a]
        go Leaf l = l
        go (Node left a right) l' = go left l' ++ [a] ++ go right l'

postorder :: BinaryTree a -> [a]
postorder t = go t []
  where go :: BinaryTree a -> [a] -> [a]
        go Leaf l = l
        go (Node left a right) l' = go left l' ++ go right l' ++ [a]

testTree :: BinaryTree Integer
testTree = 
  Node (Node Leaf 1 Leaf)
       2
       (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder = 
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder test passed!"
  else putStrLn "Preorder test failed"

testInorder :: IO ()
testInorder = 
  if inorder testTree == [1,2,3]
  then putStrLn "Inorder test passed!"
  else putStrLn "Inorder test failed"

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder test passed!"
  else putStrLn "Postorder test failed"

main :: IO ()
main = do 
  testPreorder
  testInorder
  testPostorder


-- Fold the tree, using any ordering
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f b t = foldr f b (preorder t)




-- Vignere cipher: use a fixed keyword to set the index of shifting for a ceaser cipher



