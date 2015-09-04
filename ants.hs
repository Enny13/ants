import Data.Attoparsec.Text hiding (takeWhile, length)
import Data.Text hiding (map, zipWith, takeWhile, length, tail, splitAt, lines)
import Data.Tuple
import Data.Map hiding (map)
import System.IO

type Command = (Char, Char)
type Modification = [Char] -> [Char]

syn   = 0
light = 5
hard  = 10
crit  = 100

terminator = ('Х', 'Х')

terminators = terminator : terminators

items :: [Char]
items = ['К', 'Л', 'П', 'Х']

removeAtIndex n xs = let (ys, zs) = splitAt n xs in ys ++ (tail zs)
insertAtIndex n c xs  = let (ys, zs) = splitAt n xs in ys ++ [c] ++ (zs)
changeAtIndex n c xs  = let (ys, zs) = splitAt n xs in ys ++ [c] ++ (tail zs)


fromListSym :: Ord a => [((a, a), b)] -> Map (a, a) b
fromListSym l = (fromList l) `union` (symList l)
	where symList         = fromList . (map sym)
	      sym ((a, b), c) = ((b, a), c)

nonCatastrophic :: [((Command, Command), Int)]
nonCatastrophic = [((('К', 'Л'), ('К', 'Х')), syn),
		   ((('Л', 'К'), ('Л', 'П')), syn),
		   ((('Л', 'Л'), ('Л', 'Х')), syn),
		   ((('П', 'К'), ('П', 'П')), syn),
		   ((('Х', 'К'), ('Х', 'Л')), light),
		   ((('П', 'К'), ('К', 'П')), light),
		   ((('П', 'П'), ('К', 'П')), light),
		   ((('К', 'К'), ('Л', 'Л')), light),
		   ((('К', 'К'), ('Л', 'Х')), light),
		   ((('П', 'Л'), ('П', 'Х')), hard),
		   ((('П', 'Л'), ('К', 'Л')), hard),
		   ((('П', 'Л'), ('К', 'Х')), hard),
		   ((('П', 'Х'), ('К', 'Л')), hard),
		   ((('П', 'Х'), ('К', 'Х')), hard),
		   ((('Х', 'П'), ('П', 'Х')), hard),
		   ((('Х', 'П'), ('П', 'Л')), hard),
		   ((('Х', 'П'), ('К', 'Л')), hard),
		   ((('Х', 'П'), ('К', 'Х')), hard)]

weight :: Command -> Command -> Int
weight c1 c2 
	| c1 == c2  = 0
	| otherwise = findWithDefault crit (c1, c2) $ fromListSym nonCatastrophic


item :: Parser Char
item = satisfy isItem
	where isItem c = elem c items

parseCommand :: Parser Command
parseCommand = do
	c1 <- item
	c2 <- item
	return (c1, c2)

parseCommands :: Parser [Command]
parseCommands = many' parseCommand


parseModif :: Parser Modification
parseModif = do
	n <- decimal 
	choice [del n, ins n, cng n]

del :: Int -> Parser Modification
del n = do
	string (pack "удл")
	return (removeAtIndex n)

ins :: Int -> Parser Modification
ins n = do
	string (pack "вст")
	i <- item
	return (insertAtIndex n i)

cng :: Int -> Parser Modification
cng n = do
	string (pack "изм")
	i <- item
	return (changeAtIndex n i)

estimate :: [Command] -> [Command] -> Int
estimate xs ys = (sum $ zipWith weight xs' ys') + (crit * (size1 - size2))
	where xs' = takeWhile notTerminator (xs ++ terminators)
	      ys' = takeWhile notTerminator (ys ++ terminators)
	      size1 = length xs'
	      size2 = length ys'
	      notTerminator = (/=) terminator 
	      
processExperiment :: [Char] -> [Char] -> Either String Int
processExperiment exp m = do
	modification    <- parseOnly parseModif    $ pack m
	commands        <- parseOnly parseCommands $ pack exp
	commands'       <- parseOnly parseCommands $ pack (modification exp)
	return $ estimate commands commands'

processLine :: [Char] -> Either String Int
processLine l = do
	(ex, m) <- parseOnly lineParser $ pack l
	processExperiment ex m
	where lineParser = do
		ex 		<- takeTill ((==) ':')
		colon   	<- char ':'
		skipWhile ((==) ' ')
		m 		<- many' anyChar
		return $ ((unpack ex), (m))	

toString :: (Show a, Show b) => Either b a -> [Char]
toString (Right a) = show a
toString (Left  b) = show b
	

main = do
	handleR   <- openFile "experiments.txt" ReadMode
	handleW   <- openFile "results.txt" WriteMode
	contents <- hGetContents handleR
	let lns = lines contents
	mapM ((hPutStrLn handleW) . toString . processLine) lns
	hClose handleR
	hClose handleW
	
	
	 --initial configuration of items




