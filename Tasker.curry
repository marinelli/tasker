module Tasker where


data Task = Cut | Sew | Paste
  deriving (Eq, Show)


data Worker =  Qui | Quo | Qua | Paperino | Topolino | Dumbo
  deriving (Eq, Show)


assign :: Task -> Worker
assign Cut   = Qui
assign Cut   = Quo
assign Cut   = Paperino
assign Sew   = Qua
assign Sew   = Quo
assign Sew   = Paperino
assign Sew   = Topolino
assign Paste = Paperino
assign Paste = Dumbo
assign Paste = Qui
assign Paste = Qua


type Team = (Worker, Worker, Worker)


team :: Team
team | x /= y && y /= z && x /= z =  (x, y, z)
  where
    x = assign Cut
    y = assign Sew
    z = assign Paste


teamList :: Team -> [Worker]
teamList (w1, w2, w3) = [w1, w2, w3]


distinctTeams :: Team -> Team -> Bool
distinctTeams team1 team2 =
  foldr (\ w ws -> not (w `elem` (teamList team2)) && ws) True (teamList team1)


allDistinctTeams :: Team -> [Team] -> Bool
allDistinctTeams myteam teams = all (distinctTeams myteam) teams


teams2 :: [Team]
teams2 | t1 =:= team & t2 =:= team & distinctTeams t1 t2  =  [t1, t2]
  where
    t1, t2 free


teams3 :: [Team]
teams3
  | t1 =:= team & t2 =:= team & t3 =:= team
      & distinctTeams t1 t2 & distinctTeams t2 t3 & distinctTeams t1 t3
    =  [t1, t2, t3]
  where
    t1, t2, t3 free


takeTeams :: Int -> [Team]
takeTeams = takeTeams' []
  where
    takeTeams' :: [Team] -> Int -> [Team]
    takeTeams' ts n
      | n == 0  =  []
      | n > 0  &  t =:= team  &  allDistinctTeams t ts  =  t : takeTeams' (t : ts) (n - 1)
      where
       t free
