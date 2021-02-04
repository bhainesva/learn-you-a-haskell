main = interact $ unlines . filter ((<10) . length) . lines

shortLinesOnly :: String -> String
shortLinesOnly input =
  let allLines = lines input
      shortLines = filter ((<10) . length) allLines
      result = unlines shortLines
  in result
