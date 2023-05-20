import Data.Char
import Data.List

main::IO()
main = do
    print (getSunk database)
    print (inBattleAfterDamaged database)
    print (grandchildrenIncreased t1)
    print (grandchildrenIncreased t2)

--task 1
type Name = String
type Date = String
type Class = String
type Result = String
type Launched = Int
data Battle = Battle Name Date deriving Show
data Ship = Ship Name Class Launched deriving Show
data Outcome = Outcome Name Name Result deriving Show
type Database = ([Outcome], [Battle], [Ship])

insertShip :: (Name, Name) -> [(Name, [Name])] -> [(Name, [Name])]
insertShip (ship, battle) []  = [(battle, [ship])] 
insertShip (ship, battle) ((curBat, curShips) : lst) = 
    if battle == curBat 
        then (curBat, ship : curShips) : lst 
    else (curBat, curShips) : insertShip (ship, battle) lst    

getSunk :: Database -> [(Name, [Name])]
getSunk (outcomes, battles, ships) = checkOutcomes outcomes []
    where
        checkOutcomes [] res = res
        checkOutcomes ((Outcome curShip curBat curRes) : outcomes) res
            | curRes == "sunk" = checkOutcomes outcomes (insertShip (curShip, curBat) res)
            | otherwise        = checkOutcomes outcomes res 

getBattleDate :: Name -> [Battle] -> Date
getBattleDate _ [] = error "unexisting battle"
getBattleDate battleName ((Battle batName batDate) : battles) = 
    if battleName == batName
        then batDate
    else getBattleDate battleName battles

participated :: Name -> Name -> [Outcome] -> Bool
participated _ _ [] = False
participated shipName battleName ((Outcome sName bName res) : outcomes) = 
    if shipName == sName && battleName == bName
        then True
    else participated shipName battleName outcomes     

inBattleAfterDamagedShip :: (Name, Date) -> Database -> Bool
inBattleAfterDamagedShip (shipName, "never") _ = False
inBattleAfterDamagedShip _ (outcomes, [], ships) = False
inBattleAfterDamagedShip (shipName, dateDmg) (outcomes,((Battle batName batDate) : battles),ships) =
    if participated shipName batName outcomes && dateDmg < batDate
        then True
    else inBattleAfterDamagedShip (shipName, dateDmg) (outcomes,battles,ships)

whenDamaged :: Name -> Database -> (Name, Date)
whenDamaged shipName ([], _, _) = (shipName, "never")
whenDamaged shipName ( (Outcome curShip curBat res) : outcomes, battles, ships) =
    if curShip == shipName && res == "damaged"
        then (shipName, getBattleDate curBat battles)
    else whenDamaged shipName (outcomes, battles, ships)

inBattleAfterDamaged :: Database -> [Name]
inBattleAfterDamaged dat@(outcomes, battles, ships) =
    [sName | (Ship sName sClass sLnchd) <- ships, inBattleAfterDamagedShip (whenDamaged sName dat) dat] 

-------------------------------- Data ----------------------------------
outcomes :: [Outcome]
outcomes = [ Outcome "Bismarck" "North Atlantic" "sunk", 
    Outcome "California" "Surigao Strait" "ok", 
    Outcome "Duke of York" "North Cape" "ok", 
    Outcome "Fuso" "Surigao Strait" "sunk", 
    Outcome "Hood" "North Atlantic" "sunk", 
    Outcome "King George V" "North Atlantic" "ok", 
    Outcome "Kirishima" "Guadalcanal" "sunk", 
    Outcome "Prince of Wales" "North Atlantic" "damaged", 
    Outcome "Rodney" "North Atlantic" "ok", 
    Outcome "Schamhorst" "North Cape" "sunk", 
    Outcome "South Dakota" "Guadalcanal" "damaged", 
    Outcome "Tennessee" "Surigao Strait" "ok", 
    Outcome "Washington" "Guadalcanal" "ok", 
    Outcome "Prince of Wales" "Guadalcanal" "ok", 
    Outcome "West Virginia" "Surigao Strait" "ok", 
    Outcome "Yamashiro" "Surigao Strait" "sunk", 
    Outcome "California" "Guadalcanal" "damaged" ]

battles :: [Battle]
battles = [ Battle "Guadalcanal" "1942-11-15", 
    Battle "North Atlantic" "1941-05-25", 
    Battle "North Cape" "1943-12-26", 
    Battle "Surigao Strait" "1944-10-25" ]

ships :: [Ship]
ships = [ Ship "California" "Tennessee" 1921, 
    Ship "Haruna" "Kongo" 1916, 
    Ship "Hiei" "Kongo" 1914, 
    Ship "Iowa" "Iowa" 1943, 
    Ship "Kirishima" "Kongo" 1915, 
    Ship "Kongo" "Kongo" 1913, 
    Ship "Missouri" "Iowa" 1944, 
    Ship "Musashi" "Yamato" 1942, 
    Ship "New Jersey" "Iowa" 1943, 
    Ship "North Carolina" "North Carolina" 1941, 
    Ship "Ramillies" "Revenge" 1917, 
    Ship "Renown" "Renown" 1916, 
    Ship "Repulse" "Renown" 1916, 
    Ship "Resolution" "Renown" 1916, 
    Ship "Revenge" "Revenge" 1916, 
    Ship "Royal Oak" "Revenge" 1916, 
    Ship "Royal Sovereign" "Revenge" 1916, 
    Ship "Tennessee" "Tennessee" 1920, 
    Ship "Washington" "North Carolina" 1941, 
    Ship "Wisconsin" "Iowa" 1944, 
    Ship "Yamato" "Yamato" 1941, 
    Ship "Yamashiro" "Yamato" 1947, 
    Ship "South Dakota" "North Carolina" 1941, 
    Ship "Bismarck" "North Carolina" 1911, 
    Ship "Duke of York" "Renown" 1916, 
    Ship "Fuso" "Iowa" 1940, 
    Ship "Hood" "Iowa" 1942, 
    Ship "Rodney" "Yamato" 1915, 
    Ship "Yanashiro" "Yamato" 1918, 
    Ship "Schamhorst" "North Carolina" 1917, 
    Ship "Prince of Wales" "North Carolina" 1937, 
    Ship "King George V" "Iowa" 1942, 
    Ship "West Virginia" "Iowa" 1942 ]

database :: Database
database = (outcomes, battles, ships)

database1 :: Database
database1 = ([], battles, ships)
--------------------------------------------------------
--task2
data BTree = Nil | Node Int BTree BTree

checkProperty :: BTree -> BTree -> Bool
checkProperty Nil _ = True
checkProperty (Node valChild _ _) (Node valOpa _ _) = valChild > valOpa

grandchildrenIncreased :: BTree -> Bool
grandchildrenIncreased Nil = True
grandchildrenIncreased (Node _ Nil Nil) = True
grandchildrenIncreased curr@(Node _ Nil right@(Node _ rltree rrtree)) =   
    checkProperty rltree curr && checkProperty rrtree curr && grandchildrenIncreased right
grandchildrenIncreased curr@(Node _ left@(Node _ lltree lrtree) Nil) =   
    checkProperty lltree curr && checkProperty lrtree curr && grandchildrenIncreased left
grandchildrenIncreased curr@(Node _ left@(Node _ lltree lrtree) right@(Node _ rltree rrtree)) = 
    length [vertex | vertex <- [lltree, lrtree, rltree, rrtree], checkProperty vertex curr] == 4 
    && grandchildrenIncreased left 
    && grandchildrenIncreased right

t1 :: BTree
t1 = (Node 1 (Node (-1) (Node 2 Nil Nil) (Node 2 (Node 0 Nil Nil) Nil)) (Node (-1) Nil Nil))

t2 :: BTree
t2 = (Node 1 (Node 2 (Node 1 Nil Nil) (Node 1 (Node 10 Nil Nil) Nil)) (Node 3 Nil Nil))
