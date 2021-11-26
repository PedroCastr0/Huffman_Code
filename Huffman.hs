module Huffman where
import PriorityQueue(listToHeap, flatten)
import Data.Map (toList, fromListWith)
import System.IO

data Tree = Leaf Char Integer | Node Tree Tree Integer
    deriving(Show, Read)

toTreeList = map (uncurry Leaf)

merge t1 t2 = Node t1 t2 (value t1 + value t2)

value :: Tree -> Integer
value (Leaf _ w) = w
value (Node _ _ w) = w

buildTree :: [(Char, Integer)] -> Tree
buildTree = build . toTreeList
    where
        build (t : []) = t
        build (a : b : []) = merge a b
        build (a : b : cs) = merge (merge a b) (build cs)

listOfFrequency :: String -> [(Char, Integer)]
listOfFrequency text = flatten $ listToHeap $ toList $ fromListWith (+) [(c, 1) | c <- text]

buildCodeMap :: Tree -> [(Char, String)]
buildCodeMap = codeList
            where
                codeList (Leaf c _) = [(c, [])]
                codeList (Node l r _) = map (\(a, b) -> (a, "0" ++ b)) (codeList l) ++ map (\(a, b) -> (a, "1" ++ b)) (codeList r)

encode :: String -> String
encode str = encodeWithTable (buildCodeMap $ buildTree $ listOfFrequency str) str

encodeWithTable :: [(Char, String)] -> String -> String
encodeWithTable table str = concat $ [lookupTable c table | c <- str]

lookupTable :: Char ->  [(Char, String)] -> String
lookupTable c ((ch, str):lst)
            | ch == c = str
            | otherwise  = lookupTable c lst

treeToFile :: (Show a) => FilePath -> a -> IO()
treeToFile filePath dataStruct = do
           writeFile filePath (show dataStruct)

decode :: Tree -> String -> String
decode tree = decodeAux tree
            where
                decodeAux (Leaf c _) [] = [c]
                decodeAux (Leaf c _) bs = c : decodeAux tree bs
                decodeAux (Node l r _) (b:bs) = decodeAux (if b == '0' then l else r) bs

-- Chamar funcao apontando para um arquivo .txt com o texto para ser compactado
encodeHuff path = do
    s <- readFile path
    writeFile "out.txt" (encode s)
    treeToFile "tree.dat" (buildTree $ listOfFrequency s)

-- Le o arquivo out.txt e tree.dat e descompacta para o texto antigo
decodeHuff = do
        out <- readFile "out.txt"
        ds <- readFile "tree.dat"
        return(decode (read ds :: Tree) out)
