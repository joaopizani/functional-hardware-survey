dead, alive :: Parser Bool
dead  = fmap (const False) (char '.')
alive = fmap (const True) (char 'O')

line :: Parser [Bool]
line  = many1 (dead <|> alive)

board :: Parser [[Bool]]
board = line `endBy1` newline

parseBoardFromFile :: FilePath -> IO [[Bool]]
parseBoardFromFile filename =
    do  result <- parseFromFile board filename
        return $ either (error . show) id result
